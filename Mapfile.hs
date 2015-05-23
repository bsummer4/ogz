{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveTraversable       #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax, LambdaCase                             #-}

module Mapfile where

import           Codec.Compression.GZip (compress, decompress)
import           Control.Applicative
import           Control.Exception      (assert)
import           Control.Monad          (guard, replicateM, replicateM_, unless,
                                         void)
import           Data.Binary            (Get, Put, decode, getWord8, putWord8)
import qualified Data.Binary            as B
import           Data.Binary.Get        (getWord16le, getWord32le, getWord8,
                                         runGet, runGetOrFail)
import           Data.Binary.IEEE754    (getFloat32le, putFloat32le)
import           Data.Binary.Put        (PutM, putWord16le, putWord32le,
                                         putWord8, runPut)
import           Data.Bits
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.ByteString.Short  (ShortByteString)
import qualified Data.ByteString.Short  as BSS
import           Data.Char              (ord)
import           Data.DeriveTH
import           Data.Either            (isRight)
import           Data.Foldable
import           Data.IORef
import           Data.List              as L
import           Data.Maybe             (isJust)
import           Data.Monoid            ((<>))
import           Data.Proxy
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Traversable
import           Data.Word
import           GHC.Generics
import           Prelude.Unicode
import           System.Directory
import           Test.QuickCheck        (Arbitrary, arbitrary)
import qualified Test.QuickCheck        as QC
import qualified Test.SmallCheck        as SC
import           Test.SmallCheck.Series (Series)
import qualified Test.SmallCheck.Series as SC
import           Test.Tasty
import qualified Test.Tasty.QuickCheck  as QC
import qualified Test.Tasty.SmallCheck  as SC
import           Text.Printf
import           Types

import Debug.Trace
import Text.Show.Pretty


-- Types and Type Classes ------------------------------------------------------

newtype DumpM a = DumpM { unDump ∷ PutM a }
  deriving (Functor,Applicative,Monad)

newtype Load a = Load { unLoad ∷ Get a }
  deriving (Functor,Applicative,Alternative,Monad)

type Dump = DumpM ()

class Mapfile t where
  dump ∷ t → Dump
  load ∷ Load t


-- Mapfile Headers -------------------------------------------------------------

data Header = Hdr
  { hdrMagic      ∷ Four Word8 -- Always "OCTA"
  , hdrVersion    ∷ !Word32 -- Always 29.
  , hdrHeaderSize ∷ !Word32 -- Always 36. This is the number of bytes in the header.
  , hdrWorldSize  ∷ !Word32 -- Length of one side of the world-cube. Should be a power of two.
  , hdrNumEnts    ∷ !Word32 -- Number of entities.
  , hdrNumPvs     ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , hdrLightMaps  ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , hdrBlendMaps  ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , hdrNumVars    ∷ !Word32 -- Number of OGZVars.
  } deriving (Ord,Eq,Show,Generic)

derive makeArbitrary ''Header

instance Monad m ⇒ SC.Serial m Header


-- Trivial Mapfile Instances ---------------------------------------------------

instance Mapfile a ⇒ Mapfile (Two a) where
  dump (Two a b) = dump a >> dump b
  load = Two <$> load <*> load

instance Mapfile a ⇒ Mapfile (Three a) where
  dump (Three a b c) = dump a >> dump b >> dump c
  load = Three <$> load <*> load <*> load

instance Mapfile a ⇒ Mapfile (Four a) where
  dump (Four a b c e) = dump a >> dump b >> dump c >> dump e
  load = Four <$> load <*> load <*> load <*> load

instance Mapfile a ⇒ Mapfile (Five a) where
  dump (Five a b c d e) = dump a >> dump b >> dump c >> dump d >> dump e
  load = Five <$> load <*> load <*> load <*> load <*> load

instance Mapfile a ⇒ Mapfile (Six a) where
  dump (Six a b c d e f) = dump a >> dump b >> dump c >> dump d >> dump e >> dump f
  load = Six <$> load <*> load <*> load <*> load <*> load <*> load

instance Mapfile a ⇒ Mapfile (Eight a) where
  dump (Eight a b c d e f g h) = dump a >> dump b >> dump c >> dump d >>
                                 dump e >> dump f >> dump g >> dump h
  load = Eight <$> load <*> load <*> load <*> load <*> load <*> load <*> load
               <*> load

instance Mapfile a ⇒ Mapfile (LazyEight a) where
  dump (LazyEight a b c d e f g h) = dump a >> dump b >> dump c >> dump d >>
                                     dump e >> dump f >> dump g >> dump h
  load = LazyEight <$> load <*> load <*> load <*> load <*> load <*> load <*> load
                   <*> load

instance Mapfile Word8 where
  dump = DumpM . putWord8
  load = Load getWord8

instance Mapfile Word16 where
  dump = DumpM . putWord16le
  load = Load getWord16le

instance Mapfile Word32 where
  dump = DumpM . putWord32le
  load = Load getWord32le

instance Mapfile Float where
  dump = DumpM . putFloat32le
  load = Load getFloat32le

instance Mapfile Vec3 where
  load = Vec3 <$> load
  dump (Vec3 v) = dump v

instance Mapfile BVec3 where
  dump (BVec3 t) = dump t
  load = BVec3 <$> load


-- Mapfile Instances -----------------------------------------------------------

instance Mapfile Header where
  dump (Hdr m a b c d e f g h) = dump m >> mapM_ dump [a,b,c,d,e,f,g,h]
  load = Hdr <$> load <*> load <*> load <*> load <*> load
                      <*> load <*> load <*> load <*> load

instance Mapfile OGZVar where --------------------------------------------------

  dump (OGZVar nm val) = do
    let dumpStr s = do dump (fromIntegral(BS.length s) ∷ Word16)
                       mapM_ dump $ BS.unpack s
    let ty ∷ Word8 = case val of {(OInt _)→0; (OFloat _)→1; (OStr _)→2}
    dump ty
    dumpStr nm
    case val of OInt i → dump (i∷Word32)
                OFloat f → dump (f∷Float)
                OStr s → dumpStr s

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

packWorldSize ∷ Word32 → Maybe WorldSize
packWorldSize word | popCount word ≠ 1 = Nothing
packWorldSize word = mkWorldSize $ round $ logBase 2 $ fromIntegral word

unpackWorldSize ∷ WorldSize → Word32
unpackWorldSize (WorldSize n) = bit (fromIntegral n)


instance Mapfile Extras where --------------------------------------------------

  dump (Extras a b) = dump a >> dump b

  load = Extras <$> load <*> load


instance Mapfile TextureMRU where ----------------------------------------------

  load = do len∷Word16 ← load
            TextureMRU <$> replicateM (fromIntegral len) (load ∷ Load Word16)

  dump (TextureMRU l) = do dump (fromIntegral(length l) ∷ Word16)
                           mapM_ dump l


instance Mapfile EntTy where ---------------------------------------------------

  load = toEnum . fromIntegral <$> (load ∷ Load Word8)

  dump ty = dump (fromIntegral(fromEnum ty) ∷ Word8)


instance Mapfile Entity where --------------------------------------------------

  load = do
    pos ∷ Vec3 ← load
    attrs ∷ Five Word16 ← load
    ty ∷ EntTy ← load
    unused ∷ Word8 ← load
    return $ Entity pos ty attrs unused

  dump (Entity pos ty attrs unused) = do
    dump pos
    dump attrs
    dump ty
    dump unused


instance Mapfile Textures where ------------------------------------------------
  dump (Textures t) = dump t
  load = Textures <$> load

instance Mapfile Offsets where -------------------------------------------------
  dump (Offsets t) = dump t
  load = Offsets <$> load

instance Mapfile Material where ------------------------------------------------
  dump (Material t) = dump t
  load = Material <$> load

instance Mapfile Normals where -------------------------------------------------
  dump (Normals t) = dump t
  load = Normals <$> load

instance Mapfile LightMap where ------------------------------------------------
  dump (LightMap w) = dump w
  load = LightMap <$> load

word8 ∷ Eight Bool → Word8
word8 = fst . foldr f (zeroBits,0)
  where f b (w,ix) = (if b then setBit w ix else w, ix+1)

unWord8 ∷ Word8 → Eight Bool
unWord8 w = Eight (b 7) (b 6) (b 5) (b 4) (b 3) (b 2) (b 1) (b 0)
  where b = (w `testBit`)

maybeRun ∷ Monad m ⇒ m a → Bool → m (Maybe a)
maybeRun _   False = return Nothing
maybeRun act True = Just <$> act

maybeDump ∷ Mapfile a ⇒ Maybe a → Dump
maybeDump Nothing  = return ()
maybeDump (Just x) = dump x

data PartialFaces a = YesNorms (Six (Maybe ((FaceInfo,a),Normals)))
                    | NoNorms  (Six (Maybe (FaceInfo,a)))
  deriving (Functor,Foldable,Traversable)

instance Mapfile Properties where ----------------------------------------------

  load = do

      Eight materialF normalsF a b c d e f ← unWord8 <$> load

      let faceFs = Six a b c d e f

          loadFace ∷ Load (FaceInfo, Bool)
          loadFace = do
              tc ← load
              dims ← load
              pos ← load
              lm ← load

              Eight False False False False False False mergeBit layerBit
                ← unWord8 <$> load

              let layer∷Layer = toEnum $ if layerBit then 1 else 0
              return (FaceInfo tc dims pos lm layer, mergeBit)

          loadFaceWNormals ∷ Load ((FaceInfo,Bool),Normals)
          loadFaceWNormals = (,) <$> loadFace <*> load

          partialFaces ∷ Load (PartialFaces Bool)
          partialFaces = if normalsF
            then YesNorms <$> forM faceFs (maybeRun loadFaceWNormals)
            else NoNorms  <$> forM faceFs (maybeRun loadFace        )

          toFaces ∷ PartialFaces(Maybe FaceInfo) → Faces
          toFaces (YesNorms fs) = FacesNormals (fmap cvt <$> fs)
            where cvt((i,Just m ),n) = FaceWithNormals (MergedFace i m) n
                  cvt((i,Nothing),n) = FaceWithNormals (Face i)         n

          toFaces (NoNorms fs)  = Faces        (fmap cvt <$> fs)
            where cvt (i,Just m )    = MergedFace i m
                  cvt (i,Nothing)    = Face i

          fillMergeInfo ∷ PartialFaces Bool → Load (PartialFaces(Maybe FaceInfo))
          fillMergeInfo = mapM $ maybeRun $ do (face,False) ← loadFace
                                               return face

      material ∷ Maybe Word8       ← maybeRun load materialF
      basics   ∷ PartialFaces Bool ← partialFaces
      faces    ∷ Faces             ← toFaces <$> fillMergeInfo basics

      return $ Properties (Material <$> material) faces

  dump (Properties material faces) = do

      let materialF       = isJust material
          normalsF        = case faces of Faces _          → False
                                          FacesNormals _   → True
          faceFlags       = case faces of Faces fs         → isJust <$> fs
                                          FacesNormals fns → isJust <$> fns

          mask = word8 $ Eight materialF normalsF a b c d e f
                     where (Six a b c d e f) = faceFlags

          dumpFaceInfo ∷ Bool → FaceInfo → Dump
          dumpFaceInfo mergeBit (FaceInfo tc dims pos lm lay) = do
              let layerBit  = if fromEnum lay≡0 then False else True
                  layerWord = word8 $ Eight False False False    False
                                            False False mergeBit layerBit
              do {dump tc; dump dims; dump pos; dump lm; dump layerWord}

          dumpFace ∷ Face → Dump
          dumpFace (Face i)         = dumpFaceInfo False i
          dumpFace (MergedFace i _) = dumpFaceInfo True  i

          dumpFaceNormals ∷ FaceWithNormals → Dump
          dumpFaceNormals (FaceWithNormals f n) = dumpFace f >> dump n

          dumpFaces ∷ Faces → Dump
          dumpFaces (Faces        fs)  = mapM_ (mapM_ dumpFace)        fs
          dumpFaces (FacesNormals fns) = mapM_ (mapM_ dumpFaceNormals) fns

          dumpMergeInfo ∷ Face → Dump
          dumpMergeInfo (MergedFace _ m) = dumpFaceInfo False m
          dumpMergeInfo _                = return()

          dumpAllMergeInfo ∷ Faces → Dump
          dumpAllMergeInfo (Faces x) = mapM_ (mapM_ dumpMergeInfo) x
          dumpAllMergeInfo (FacesNormals x) = mapM_ (mapM_ (cvt dumpMergeInfo)) x
            where cvt g (FaceWithNormals face ns) = g face

      dump (mask ∷ Word8)
      maybeDump material
      dumpFaces faces
      dumpAllMergeInfo faces


-- splitAt n t returns a pair whose first element is a prefix of t
-- of length n, and whose second is the remainder of the string.
bitSplitAt ∷ FiniteBits a ⇒ Int → a → (a,a)
bitSplitAt rightSz bits = (bits `shiftR` rightSz, bits .&. rightMask)
  where ones = complement zeroBits
        rightMask = complement (ones `shiftL` rightSz)


instance Mapfile MergeInfo where -----------------------------------------------
  load = MergeInfo <$> load
  dump (MergeInfo m) = dump m

instance Mapfile MergeData where -----------------------------------------------

  load = do
      merged ∷ Word8 ← load

      let moreInfo = merged `testBit` 7
      let some7bits = merged `clearBit` 7

      mergeInfos ← flip maybeRun (merged `testBit` 7) $ do
          flags ∷ Word8 ← load
          forM (unWord8 flags) $ maybeRun $ load

      return $ MergeData some7bits mergeInfos

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

      let mask = if hasMergeData then geoTy `clearBit` 7
                                 else geoTy `setBit`   7

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
        mergeDataF    = not (mask `testBit` 7)
        loadMergeData = maybeRun load mergeDataF ∷ Load(Maybe MergeData)

    case fromIntegral typeTag of
      0 → NBroken <$> load
      1 → NEmpty <$> load <*> load <*> loadMergeData
      2 → NSolid <$> load <*> load <*> loadMergeData
      3 → NDeformed <$> load <*> load <*> load <*> loadMergeData
      4 → NLodCube <$> load <*> load <*> load <*> loadMergeData
      n → fail $ "Invalid octree node tag: " <> show n


instance Mapfile OGZ where -----------------------------------------------------

  dump (OGZ sz vars gameTy extras mru ents geo) = do

      let header = Hdr octa 29 36 wsz (elems ents) 0 0 0 (elems vars)
            where wsz    = unpackWorldSize sz
                  elems  = fromIntegral . length
                  x      = fromIntegral . ord
                  octa   = Four (x 'O') (x 'C') (x 'T') (x 'A')

      dump header
      mapM_ dump vars
      dump gameTy
      dump extras
      dump mru
      mapM_ dump ents
      dump geo

  load = do
      hdr ← load

      let magic   = hdrMagic hdr
          version = hdrVersion hdr
          x       = fromIntegral . ord
          octa    = Four (x 'O') (x 'C') (x 'T') (x 'A')

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



-- Properties ------------------------------------------------------------------

-- This assumes that the bytestrings are valid.
reversibleLoad ∷ (Mapfile a,Eq a) ⇒ Proxy a → BSL.ByteString → Bool
reversibleLoad p bs =
    case runGetOrFail (unLoad load) bs of
        Right (_,_,x) → bs ≡ runPut(unDump $ dump $ x `asProxyTypeOf` p)
        _             → False

reversibleDump ∷ (Show t,Eq t,Mapfile t) ⇒ t → Bool
reversibleDump x = runGet (unLoad load) (runPut $ unDump $ dump x) ≡ x


-- Test Suite ------------------------------------------------------------------

assertM c = assert c (return())

listSingleton ∷ a → [a]
listSingleton x = [x]

attemptThings ∷ (Mapfile a, Eq a, Show a) ⇒ a → IO Bool
attemptThings p = do
  let bs = runPut $ unDump $ dump p
  putStrLn $ dumpBytes $ BSL.unpack bs
  let p' = runGet (unLoad load) bs
  putStrLn "================="
  putStrLn $ ppShow p
  putStrLn $ if p == p' then "========== DOES MATCH ==========" else "!!!!!!!! DOES NOT MATCH !!!!!!"
  putStrLn $ ppShow p'
  putStrLn "================="
  _ ← getLine
  return $ p == p'

dumpBytes ∷ [Word8] → String
dumpBytes = r 0 where
  r i [] = ""
  r 32 bs = '\n' : r 0 bs
  r 0 (b:bs) = printf "%02x" b ++ r 1 bs
  r i (b:bs) = " " ++ printf "%02x" b ++ r (i+1) bs


mapfileTests ∷ (Show a, Eq a, Mapfile a, QC.Arbitrary a, SC.Serial IO a)
             ⇒ Proxy a → String → (QC.QuickCheckTests,SC.SmallCheckDepth)
             → IO TestTree
mapfileTests ty tyName (qcTests,scDepth) = do

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
        SC.over (SC.generate(const suiteCases)) $
          reversibleLoad ty
    ]

loadOGZ ∷ FilePath → IO OGZ
loadOGZ = fmap (runGet (unLoad load) . decompress) . BSL.readFile

getTestMaps = do
  let root = "./testdata/maps/"
  mapnames ← filter (\x → x≠"." ∧ x≠"..") <$> getDirectoryContents root
  return $ (root <>) <$> mapnames

testLoad ∷ IO ()
testLoad = do
  testMaps ← getTestMaps
  testMaps ← return ["testdata/maps/example.ogz"]
  forM_ testMaps $ \filename → do
    result ← loadOGZ filename
    putStrLn $ printf "PASS: world size is %d (%s)"
                 (unWorldSize $ ogzWorldSize result)
                 filename

test = defaultMain =<< testGroup "tests" <$> sequence
  [ mapfileTests (Proxy∷Proxy Header)     "Header"     (100 , 2)
  , mapfileTests (Proxy∷Proxy OGZVar)     "OGZVar"     (100 , 5)
  , mapfileTests (Proxy∷Proxy GameType)   "GameType"   (100 , 5)
  , mapfileTests (Proxy∷Proxy WorldSize)  "WorldSize"  (100 , 5)
  , mapfileTests (Proxy∷Proxy Extras)     "Extras"     (100 , 5)
  , mapfileTests (Proxy∷Proxy TextureMRU) "TextureMRU" (100 , 5)
  , mapfileTests (Proxy∷Proxy EntTy)      "EntTy"      (100 , 5)
  , mapfileTests (Proxy∷Proxy Vec3)       "Vec3"       (100 , 3)
  , mapfileTests (Proxy∷Proxy Entity)     "Entity"     (1000, 0)
  , mapfileTests (Proxy∷Proxy Textures)   "Textures"   (100 , 3)
  , mapfileTests (Proxy∷Proxy Offsets)    "Offsets"    (100 , 5)
  , mapfileTests (Proxy∷Proxy Material)   "Material"   (100 , 5)
  , mapfileTests (Proxy∷Proxy Normals)    "Normals"    (1000, 0)
  , mapfileTests (Proxy∷Proxy LightMap)   "LightMap"   (100 , 5)
  , mapfileTests (Proxy∷Proxy Properties) "Properties" (1000, 0)
  , mapfileTests (Proxy∷Proxy MergeInfo)  "MergeInfo"  (1000, 0)
  , mapfileTests (Proxy∷Proxy MergeData)  "MergeData"  (1000, 0)
  , mapfileTests (Proxy∷Proxy Octree)     "Octree"     (1000, 0)
  , mapfileTests (Proxy∷Proxy OGZ)        "OGZ"        (100 , 0)
  ]

main = test
