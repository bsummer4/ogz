{-# LANGUAGE DefaultSignatures, FlexibleContexts                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}

module Mapfile (loadOGZ, dumpOGZ, test, exaustiveTest, testLoad) where

import Prelude hiding (foldr, mapM, mapM_, sequence)

import Types
import HomoTuple

import           Codec.Compression.GZip (compress, decompress)
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad          (MonadPlus, guard, replicateM, unless)
import           Data.Binary            (Get, decode, getWord8, putWord8)
import           Data.Binary.Get        (getWord16le, getWord32le, runGet,
                                         runGetOrFail)
import           Data.Binary.IEEE754    (getFloat32le, putFloat32le)
import           Data.Binary.Put        (PutM, putWord16le, putWord32le, runPut)
import           Data.Bits
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Short  as BSS
import           Data.Char              (ord)
import           Data.DeriveTH
import           Data.Foldable
import           Data.Maybe             (isJust)
import           Data.Monoid            ((<>))
import           Data.Proxy
import           Data.Traversable
import           Data.Word
import           GHC.Generics
import           Prelude.Unicode
import           System.Directory
import           Test.QuickCheck        (Arbitrary, arbitrary)
import qualified Test.QuickCheck        as QC
import qualified Test.SmallCheck        as SC
import qualified Test.SmallCheck.Series as SC
import           Test.Tasty
import qualified Test.Tasty.QuickCheck  as QC
import qualified Test.Tasty.SmallCheck  as SC
import           Text.Printf


-- Read and Write Map Files ----------------------------------------------------

loadOGZ ∷ FilePath → IO OGZ
loadOGZ = fmap (runGet (unLoad load) . decompress) . BSL.readFile

dumpOGZ ∷ FilePath → OGZ → IO ()
dumpOGZ fp = BSL.writeFile fp . compress . runPut . unDump . dump


-- The Mapfile Type Class ------------------------------------------------------

newtype DumpM a = DumpM { unDump ∷ PutM a }
  deriving (Functor,Applicative,Monad)

newtype Load a = Load { unLoad ∷ Get a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

type Dump = DumpM ()

class Mapfile t where
  dump ∷ t → Dump
  load ∷ Load t

  default dump :: (Generic t, GMapfile (Rep t)) => t → Dump
  dump = gDump . from

  default load :: (Generic t, GMapfile (Rep t)) => Load t
  load = to <$> gLoad

class GMapfile f where
  gDump ∷ f t → Dump
  gLoad ∷ Load (f t)

instance GMapfile U1 where
  gLoad = return U1
  gDump U1 = return()
  {-# INLINE gLoad #-}
  {-# INLINE gDump #-}

instance (GMapfile a, GMapfile b) => GMapfile (a :*: b) where
  gDump (x :*: y) = gDump x >> gDump y
  gLoad = (:*:) <$> gLoad <*> gLoad
  {-# INLINE gLoad #-}
  {-# INLINE gDump #-}

instance (GMapfile a) => GMapfile (M1 i c a) where
  gDump (M1 x) = gDump x
  gLoad = M1 <$> gLoad
  {-# INLINE gLoad #-}
  {-# INLINE gDump #-}

instance (Mapfile a) => GMapfile (K1 i a) where
  gDump (K1 x) = dump x
  gLoad = K1 <$> load
  {-# INLINE gLoad #-}
  {-# INLINE gDump #-}

-- There is no instance for `GMapfile(a:+:b)`, since there's no way to write one.


-- Internal Types --------------------------------------------------------------

data Header = Hdr
  { hdrMagic       ∷ Tup4 Word8 -- Always "OCTA"
  , hdrVersion     ∷ !Word32 -- Always 29.
  , _hdrHeaderSize ∷ !Word32 -- Always 36. This is the number of bytes in the header.
  , hdrWorldSize   ∷ !Word32 -- Length of one side of the world-cube. Should be a power of two.
  , hdrNumEnts     ∷ !Word32 -- Number of entities.
  , _hdrNumPvs     ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , _hdrLightMaps  ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , _hdrBlendMaps  ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , hdrNumVars     ∷ !Word32 -- Number of OGZVars.
  } deriving (Ord,Eq,Show,Generic)

data PartialFaces a = YesNorms (Tup6 (Maybe ((FaceInfo,a),Normals)))
                    | NoNorms  (Tup6 (Maybe (FaceInfo,a)))
  deriving (Functor,Foldable,Traversable)

data FaceInfoPlus = FaceInfoPlus !FaceInfo !Bool
  deriving (Eq,Ord,Show,Generic)

derive makeArbitrary ''FaceInfoPlus
derive makeArbitrary ''Header
instance Monad m ⇒ SC.Serial m FaceInfoPlus
instance Monad m ⇒ SC.Serial m Header


-- Utilities -------------------------------------------------------------------

word8 ∷ Tup8 Bool → Word8
word8 = fst . foldr f (zeroBits,0)
  where f b (w,ix) = (if b then setBit w ix else w, ix+1)

unWord8 ∷ Word8 → Tup8 Bool
unWord8 w = Tup8 (b 7) (b 6) (b 5) (b 4) (b 3) (b 2) (b 1) (b 0)
  where b = (w `testBit`)

maybeRun ∷ Monad m ⇒ m a → Bool → m (Maybe a)
maybeRun _   False = return Nothing
maybeRun act True = return . Just =<< act

maybeDump ∷ Mapfile a ⇒ Maybe a → Dump
maybeDump Nothing  = return ()
maybeDump (Just x) = dump x

packWorldSize ∷ Word32 → Maybe WorldSize
packWorldSize word | popCount word ≠ 1 = Nothing
packWorldSize word = mkWorldSize $ round $ logBase 2 (fromIntegral word∷Double)

unpackWorldSize ∷ WorldSize → Word32
unpackWorldSize (WorldSize n) = bit (fromIntegral n)

octa ∷ Tup4 Word8
octa = Tup4 (x 'O') (x 'C') (x 'T') (x 'A')
  where x = fromIntegral . ord

-- bitSplitAt n t returns a pair whose first element is a prefix of t
-- of length n, and whose second is the remainder of the string.
bitSplitAt ∷ FiniteBits a ⇒ Int → a → (a,a)
bitSplitAt rightSz bits = (bits `shiftR` rightSz, bits .&. rightMask)
  where ones = complement zeroBits
        rightMask = complement (ones `shiftL` rightSz)


-- Trivial Mapfile Instances ---------------------------------------------------

instance Mapfile Float  where dump = DumpM . putFloat32le
                              load = Load getFloat32le

instance Mapfile Word8  where dump = DumpM . putWord8
                              load = Load getWord8

instance Mapfile Word16 where dump = DumpM . putWord16le
                              load = Load getWord16le

instance Mapfile Word32 where dump = DumpM . putWord32le
                              load = Load getWord32le

instance Mapfile a ⇒ Mapfile (LzTup0 a)
instance Mapfile a ⇒ Mapfile (LzTup1 a)
instance Mapfile a ⇒ Mapfile (LzTup2 a)
instance Mapfile a ⇒ Mapfile (LzTup3 a)
instance Mapfile a ⇒ Mapfile (LzTup4 a)
instance Mapfile a ⇒ Mapfile (LzTup5 a)
instance Mapfile a ⇒ Mapfile (LzTup6 a)
instance Mapfile a ⇒ Mapfile (LzTup7 a)
instance Mapfile a ⇒ Mapfile (LzTup8 a)
instance Mapfile a ⇒ Mapfile (LzTup9 a)
instance Mapfile a ⇒ Mapfile (Tup0 a)
instance Mapfile a ⇒ Mapfile (Tup1 a)
instance Mapfile a ⇒ Mapfile (Tup2 a)
instance Mapfile a ⇒ Mapfile (Tup3 a)
instance Mapfile a ⇒ Mapfile (Tup4 a)
instance Mapfile a ⇒ Mapfile (Tup5 a)
instance Mapfile a ⇒ Mapfile (Tup6 a)
instance Mapfile a ⇒ Mapfile (Tup7 a)
instance Mapfile a ⇒ Mapfile (Tup8 a)
instance Mapfile a ⇒ Mapfile (Tup9 a)

instance Mapfile Vec3
instance Mapfile BVec3
instance Mapfile Header
instance Mapfile Extras
instance Mapfile Entity
instance Mapfile Textures
instance Mapfile Offsets
instance Mapfile Material
instance Mapfile Normals
instance Mapfile LightMap
instance Mapfile MergeInfo


-- Mapfile Instances -----------------------------------------------------------

instance Mapfile OGZVar where --------------------------------------------------

  dump (OGZVar nm val) = do
    let dumpVarStr s = do dump (fromIntegral(BS.length s) ∷ Word16)
                          mapM_ dump $ BS.unpack s
    let ty ∷ Word8 = case val of {(OInt _)→0; (OFloat _)→1; (OStr _)→2}
    dump ty
    dumpVarStr nm
    case val of OInt i → dump (i∷Word32)
                OFloat f → dump (f∷Float)
                OStr s → dumpVarStr s

  load = do
    let getStr = do nmLen∷Word16 ← load
                    BS.pack <$> replicateM (fromIntegral nmLen) load
    ty∷Word8 ← load
    nm ← getStr
    val ← case ty of 0 → OInt <$> load
                     1 → OFloat <$> load
                     2 → OStr <$> getStr
                     _ → error "Invalid var type code!"

    return $ OGZVar nm val


instance Mapfile GameType where ------------------------------------------------

  load = do
      nmLen∷Word8 ← load
      result ← BSS.pack <$> replicateM (fromIntegral nmLen) load
      nullChr∷Word8 ← load
      guard $ nullChr ≡ 0
      return $ GameType result

  dump (GameType bs) = do
      dump (fromIntegral(BSS.length bs) ∷ Word8)
      mapM_ dump $ BSS.unpack bs
      dump (0∷Word8)

instance Mapfile WorldSize where -----------------------------------------------
  dump ws = dump (unpackWorldSize ws ∷ Word32)
  load = do Just ws ← packWorldSize <$> (load ∷ Load Word32)
            return ws

instance Mapfile TextureMRU where ----------------------------------------------
  load = do len∷Word16 ← load
            TextureMRU <$> replicateM (fromIntegral len) (load ∷ Load Word16)
  dump (TextureMRU l) = do dump (fromIntegral(length l) ∷ Word16)
                           mapM_ dump l

instance Mapfile EntTy where ---------------------------------------------------
  load = toEnum . fromIntegral <$> (load ∷ Load Word8)
  dump ty = dump (fromIntegral(fromEnum ty) ∷ Word8)

instance Mapfile FaceInfoPlus where --------------------------------------------

  load = do
      tc ← load
      dims ← load
      pos ← load
      lm ← load

      Tup8 False False False False False False mergeBit layerBit
        ← unWord8 <$> load

      let layer∷Layer = toEnum $ if layerBit then 1 else 0
      return $ FaceInfoPlus (FaceInfo tc dims pos lm layer) mergeBit

  dump (FaceInfoPlus (FaceInfo tc dims pos lm lay) mergeBit) = do
      let layerBit  = fromEnum lay ≠ 0
      let layerWord = word8 $ Tup8 False False False    False
                                   False False mergeBit layerBit

      dump tc; dump dims; dump pos; dump lm; dump layerWord


instance Mapfile Properties where ----------------------------------------------

  load = do

      Tup8 materialF normalsF a b c d e f ← unWord8 <$> load

      let faceFs = Tup6 a b c d e f

      let loadFace ∷ Load (FaceInfo, Bool)
          loadFace = (\(FaceInfoPlus x y) → (x,y)) <$> load

      let loadFaceWNormals ∷ Load ((FaceInfo,Bool),Normals)
          loadFaceWNormals = (,) <$> loadFace <*> load

      let partialFaces ∷ Load (PartialFaces Bool)
          partialFaces = if normalsF
            then YesNorms <$> forM faceFs (maybeRun loadFaceWNormals)
            else NoNorms  <$> forM faceFs (maybeRun loadFace        )

      let toFaces ∷ PartialFaces(Maybe FaceInfo) → Faces
          toFaces (YesNorms fs) = FacesNormals (fmap cvt <$> fs)
            where cvt((i,Just m ),n) = FaceWithNormals (MergedFace i m) n
                  cvt((i,Nothing),n) = FaceWithNormals (Face i)         n
          toFaces (NoNorms fs)  = Faces        (fmap cvt <$> fs)
            where cvt (i,Just m )    = MergedFace i m
                  cvt (i,Nothing)    = Face i

      let fillMergeInfo ∷ PartialFaces Bool → Load (PartialFaces(Maybe FaceInfo))
          fillMergeInfo = mapM $ maybeRun $ do (face,False) ← loadFace
                                               return face

      material ∷ Maybe Word8       ← maybeRun load materialF
      basics   ∷ PartialFaces Bool ← partialFaces
      faces    ∷ Faces             ← toFaces <$> fillMergeInfo basics

      return $ Properties (Material <$> material) faces

  dump (Properties material faces) = do

      let materialF       = isJust material
      let normalsF        = case faces of Faces _          → False
                                          FacesNormals _   → True
      let faceFlags       = case faces of Faces fs         → isJust <$> fs
                                          FacesNormals fns → isJust <$> fns

      let mask = word8 $ Tup8 materialF normalsF a b c d e f
                     where (Tup6 a b c d e f) = faceFlags

      let dumpFaceInfo ∷ Bool → FaceInfo → Dump
          dumpFaceInfo mb fi = dump $ FaceInfoPlus fi mb

      let dumpFace ∷ Face → Dump
          dumpFace (Face i)         = dumpFaceInfo False i
          dumpFace (MergedFace i _) = dumpFaceInfo True  i

      let dumpFaceNormals ∷ FaceWithNormals → Dump
          dumpFaceNormals (FaceWithNormals f n) = dumpFace f >> dump n

      let dumpFaces ∷ Faces → Dump
          dumpFaces (Faces        fs)  = mapM_ (mapM_ dumpFace)        fs
          dumpFaces (FacesNormals fns) = mapM_ (mapM_ dumpFaceNormals) fns

      let dumpMergeInfo ∷ Face → Dump
          dumpMergeInfo (MergedFace _ m) = dumpFaceInfo False m
          dumpMergeInfo _                = return()

      let dumpAllMergeInfo ∷ Faces → Dump
          dumpAllMergeInfo (Faces x) = mapM_ (mapM_ dumpMergeInfo) x
          dumpAllMergeInfo (FacesNormals x) = mapM_ (mapM_ (cvt dumpMergeInfo)) x
            where cvt g (FaceWithNormals face _) = g face

      dump (mask ∷ Word8)
      maybeDump material
      dumpFaces faces
      dumpAllMergeInfo faces


instance Mapfile MergeData where -----------------------------------------------

  load = do
      merged ∷ Word8 ← load

      mergeInfos ← flip maybeRun (merged `testBit` 7) $ do
          flags ∷ Word8 ← load
          forM (unWord8 flags) $ maybeRun load

      return $ MergeData (merged `clearBit` 7) mergeInfos

  dump (MergeData w Nothing) =
      dump (w `clearBit` 7)

  dump (MergeData w (Just infos)) = do
      dump $ w `setBit` 7
      dump $ word8(isJust <$> infos)
      mapM_ maybeDump infos


instance Mapfile Octree where --------------------------------------------------

  dump oct = do
      let (geoTy,hasMergeData) =
            case oct of NBroken _         → (0, False)
                        NEmpty _ _ x      → (1, isJust x)
                        NSolid _ _ x      → (2, isJust x)
                        NDeformed _ _ _ x → (3, isJust x)
                        NLodCube _ _ _ x  → (4, isJust x)

      let mask = if hasMergeData then geoTy `setBit`   7
                                 else geoTy `clearBit` 7

      dump (mask ∷ Word8)

      case oct of
        NBroken clds            → do dump clds
        NEmpty ts ps md         → do dump ts; dump ps; maybeDump md
        NSolid ts ps md         → do dump ts; dump ps; maybeDump md
        NDeformed ts ps offs md → do dump ts; dump ps; dump offs; maybeDump md
        NLodCube ts ps clds md  → do dump ts; dump ps; dump clds; maybeDump md

  load = do
    mask ∷ Word8 ← load

    let (_,typeTag)   = bitSplitAt 3 mask
        mergeDataF    = mask `testBit` 7
        loadMergeData = maybeRun load mergeDataF ∷ Load(Maybe MergeData)

    case fromIntegral typeTag ∷ Int of
      0 → NBroken <$> load
      1 → NEmpty <$> load <*> load <*> loadMergeData
      2 → NSolid <$> load <*> load <*> loadMergeData
      3 → NDeformed <$> load <*> load <*> load <*> loadMergeData
      4 → NLodCube <$> load <*> load <*> load <*> loadMergeData
      n → fail $ "Invalid octree node tag: " <> show n


instance Mapfile OGZ where -----------------------------------------------------

  dump (OGZ sz vars gameTy extras mru ents tree) = do

      let header = Hdr octa 29 36 wsz (elems ents) 0 0 0 (elems vars)
            where wsz    = unpackWorldSize sz
                  elems  = fromIntegral . length

      dump header
      mapM_ dump vars
      dump gameTy
      dump extras
      dump mru
      mapM_ dump ents
      dump tree

  load = do
      hdr ← load

      let magic   = hdrMagic hdr
          version = hdrVersion hdr

      unless (octa == magic) $ do
          fail "This is not a Sauerbraten map!"

      unless (29 == version) $ do
          fail $ "Only version 29 is supported. This map has version: "
                  <> show version

      sz ← case packWorldSize (hdrWorldSize hdr) of
             Nothing → fail $ "Invalid world size: " <> show(hdrWorldSize hdr)
             Just x  → return x

      vars   ← replicateM (fromIntegral $ hdrNumVars hdr) load
      gameTy ← load
      extras ← load
      mru    ← load
      ents   ← replicateM (fromIntegral $ hdrNumEnts hdr) load
      tree   ← load

      return $ OGZ sz vars gameTy extras mru ents tree


-- Testable Properties ---------------------------------------------------------

-- This assumes that the bytestrings are valid.
reversibleLoad ∷ (Mapfile a,Eq a) ⇒ Proxy a → BSL.ByteString → Bool
reversibleLoad p bs =
    case runGetOrFail (unLoad load) bs of
        Right (_,_,x) → bs ≡ runPut(unDump $ dump $ x `asProxyTypeOf` p)
        _             → False

reversibleDump ∷ (Show t,Eq t,Mapfile t) ⇒ t → Bool
reversibleDump x = runGet (unLoad load) (runPut $ unDump $ dump x) ≡ x


-- Test Suite ------------------------------------------------------------------

getTestMaps ∷ IO [FilePath]
getTestMaps = do
  let root = "./testdata/maps/"
  mapnames ← filter (\x → x≠"." ∧ x≠"..") <$> getDirectoryContents root
  return $ (root <>) <$> mapnames

testLoad ∷ IO ()
testLoad = do
  testMaps ← getTestMaps
  forM_ testMaps $ \filename → do
    result ← loadOGZ filename
    result `deepseq`
      printf "PASS: map depth is %02d (%s)\n"
        (unWorldSize $ ogzWorldSize result)
        filename

mapfileTests ∷ (Show a, Eq a, Mapfile a, QC.Arbitrary a, SC.Serial IO a)
             ⇒ Proxy a → String → (QC.QuickCheckTests,SC.SmallCheckDepth,Int)
             → IO TestTree
mapfileTests ty tyName (qcTests,scDepth,fileLimit) = do

  let suitePath = "testdata/" <> tyName
  suiteExists ← doesFileExist suitePath
  suiteCases ← if not suiteExists
                 then do printf "[WARN] %s does not exist!!!\n" suitePath
                         return []
                 else decode <$> BSL.readFile suitePath

  let testDump o = reversibleDump(o `asProxyTypeOf` ty)
  return $ localOption qcTests $ localOption scDepth $ testGroup tyName
    [ QC.testProperty "x ≡ load(dump x)" testDump
    , SC.testProperty "x ≡ load(dump x)" testDump
    , SC.testProperty "b ≡ dump(load b)" $
        SC.over (SC.generate $ const $ take fileLimit suiteCases) $
          reversibleLoad ty
    ]

test ∷ IO ()
test = defaultMain =<< testGroup "tests" <$> sequence
  [ mapfileTests (Proxy∷Proxy Header)       "Header"       ( 100, 2,1000)
  , mapfileTests (Proxy∷Proxy OGZVar)       "OGZVar"       (1000, 5,1000)
  , mapfileTests (Proxy∷Proxy GameType)     "GameType"     ( 100, 5,1000)
  , mapfileTests (Proxy∷Proxy WorldSize)    "WorldSize"    ( 100, 5,1000)
  , mapfileTests (Proxy∷Proxy Extras)       "Extras"       ( 100, 5,1000)
  , mapfileTests (Proxy∷Proxy TextureMRU)   "TextureMRU"   ( 100, 5,1000)
  , mapfileTests (Proxy∷Proxy EntTy)        "EntTy"        ( 100, 5,1000)
  , mapfileTests (Proxy∷Proxy Vec3)         "Vec3"         ( 100, 3,1000)
  , mapfileTests (Proxy∷Proxy Entity)       "Entity"       (1000, 0,1000)
  , mapfileTests (Proxy∷Proxy Textures)     "Textures"     ( 100, 3,1000)
  , mapfileTests (Proxy∷Proxy Offsets)      "Offsets"      ( 100, 5,1000)
  , mapfileTests (Proxy∷Proxy Material)     "Material"     ( 100, 5,1000)
  , mapfileTests (Proxy∷Proxy Normals)      "Normals"      ( 100, 0,1000)
  , mapfileTests (Proxy∷Proxy LightMap)     "LightMap"     ( 100, 5,1000)
  , mapfileTests (Proxy∷Proxy FaceInfoPlus) "FaceInfoPlus" ( 100, 3,1000)
  , mapfileTests (Proxy∷Proxy Properties)   "Properties"   (1000, 0,1000)
  , mapfileTests (Proxy∷Proxy MergeInfo)    "MergeInfo"    ( 100, 0,1000)
  , mapfileTests (Proxy∷Proxy MergeData)    "MergeData"    (1000, 0,1000)
  , mapfileTests (Proxy∷Proxy Octree)       "Octree"       ( 100, 0,1000)
  , mapfileTests (Proxy∷Proxy OGZ)          "OGZ"          ( 100, 0,1000)
  ]

exaustiveTest ∷ IO ()
exaustiveTest = defaultMain =<< testGroup "tests" <$> sequence
  [ mapfileTests (Proxy∷Proxy Header)       "Header"       (5000, 2,maxBound)
  , mapfileTests (Proxy∷Proxy OGZVar)       "OGZVar"       (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy GameType)     "GameType"     (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy WorldSize)    "WorldSize"    (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy Extras)       "Extras"       (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy TextureMRU)   "TextureMRU"   (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy EntTy)        "EntTy"        (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy Vec3)         "Vec3"         (5000, 3,maxBound)
  , mapfileTests (Proxy∷Proxy Entity)       "Entity"       (5000, 0,maxBound)
  , mapfileTests (Proxy∷Proxy Textures)     "Textures"     (5000, 3,maxBound)
  , mapfileTests (Proxy∷Proxy Offsets)      "Offsets"      (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy Material)     "Material"     (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy Normals)      "Normals"      (5000, 0,maxBound)
  , mapfileTests (Proxy∷Proxy LightMap)     "LightMap"     (5000, 5,maxBound)
  , mapfileTests (Proxy∷Proxy FaceInfoPlus) "FaceInfoPlus" (5000, 3,maxBound)
  , mapfileTests (Proxy∷Proxy Properties)   "Properties"   (5000, 0,maxBound)
  , mapfileTests (Proxy∷Proxy MergeInfo)    "MergeInfo"    (5000, 0,maxBound)
  , mapfileTests (Proxy∷Proxy MergeData)    "MergeData"    (5000, 0,maxBound)
  , mapfileTests (Proxy∷Proxy Octree)       "Octree"       (5000, 0,maxBound)
  , mapfileTests (Proxy∷Proxy OGZ)          "OGZ"          ( 100, 0,  1000)
  ]
