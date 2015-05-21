{-# LANGUAGE DeriveFunctor, DeriveTraversable, FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax                                               #-}

module Mapfile where

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
import           Prelude.Unicode
import           System.Directory
import qualified Test.QuickCheck        as QC
import qualified Test.SmallCheck        as SC
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
  dump (Eight a b c d e f g h) = dump a >> dump b >> dump c >> dump d
                              >> dump e >> dump f >> dump g >> dump h
  load = Eight <$> load <*> load <*> load <*> load <*> load <*> load <*> load
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

attemptThings ∷ Properties → IO Bool
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

mergeBit ∷ Word → Bool
mergeBit = (`testBit` 1)

word8 ∷ Eight Bool → Word8
word8 = fst . foldr f (zeroBits,0)
  where f b (w,ix) = (if b then setBit w ix else w, ix+1)

unWord8 ∷ Word8 → Eight Bool
unWord8 w = Eight (b 7) (b 6) (b 5) (b 4) (b 3) (b 2) (b 1) (b 0)
  where b = testBit w

maybeRun ∷ Monad m ⇒ m a → Bool → m (Maybe a)
maybeRun _   False = return Nothing
maybeRun act True = Just <$> act

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

          maybeDump ∷ Mapfile a ⇒ Maybe a → Dump
          maybeDump Nothing  = return ()
          maybeDump (Just x) = dump x

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
          dumpFaces (Faces        fs)  = mapM_ (mapM_ $ dumpFace)        fs
          dumpFaces (FacesNormals fns) = mapM_ (mapM_ $ dumpFaceNormals) fns

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


test = defaultMain =<< testGroup "tests" <$> sequence
  [ mapfileTests (Proxy∷Proxy OGZVar)     "OGZVar"     (100 , 5)
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
  ]

main = test
