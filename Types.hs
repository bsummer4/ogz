-- This module deals with how we represent the imformation that is
-- stored in an `.ogz` file.

{-# LANGUAGE DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable, TemplateHaskell, UnicodeSyntax            #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Types where

import           Data.Binary
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.ByteString.Short  (ShortByteString)
import qualified Data.ByteString.Short  as BSS
import           Data.DeriveTH
import           Data.Text              (Text)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Word              (Word16, Word32, Word8)
import           GHC.Generics
import           Orphans                ()
import           Test.QuickCheck        (Arbitrary, Gen, arbitrary, choose)
import           Test.SmallCheck.Series (Series)
import qualified Test.SmallCheck        as SC
import qualified Test.SmallCheck.Series as SC


-- Utilities -------------------------------------------------------------------

data Three a = Three !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic,Binary)

data Four a = Four !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic,Binary)

data Six a = Six !a !a !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic,Binary)

data Eight a = Eight !a !a !a !a !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic,Binary)

data LazyEight a = LazyEight a a a a a a a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic,Binary)


-- High-Level Structure --------------------------------------------------------

newtype GameType = GameType { unGameType ∷ ShortByteString }
  deriving (Show,Ord,Eq,Generic,Binary)

data OGZ = OGZ {
    ogzWorldSize  ∷ Word32
  , ogzVars       ∷ [OGZVar]
  , ogzGameType   ∷ ByteString
  , ogzExtras     ∷ Extras
  , ogzTextureMRU ∷ TextureMRU
  , ogzEntities   ∷ [Entity]
  , ogzGeometry   ∷ Octree
  } deriving (Show,Ord,Eq,Generic,Binary)


-- Variables -------------------------------------------------------------------

data OGZVar = OGZVar !ByteString !OGZVal
  deriving (Show,Ord,Eq,Generic,Binary)

data OGZVal = OInt !Word32 | OFloat !Float | OStr !ByteString
  deriving (Show,Ord,Eq,Generic,Binary)


-- Extras ----------------------------------------------------------------------

data Extras = Extras !Word16 !Word16
  deriving (Show,Ord,Eq,Generic,Binary)


-- Texture Stuff ---------------------------------------------------------------

data TextureMRU = TextureMRU ![Word16]
  deriving (Show,Ord,Eq,Generic,Binary)

data Textures = Textures !(Six Word16)
  deriving (Show,Ord,Eq,Generic,Binary)


-- Entities --------------------------------------------------------------------

data Entity = Entity !Vec3 !EntTy !Word16 !Word16 !Word16 !Word16 !Word16 !Word8
  deriving (Show,Ord,Eq,Generic,Binary)

data Vec3 = Vec3 !Float !Float !Float
  deriving (Show,Ord,Eq,Generic,Binary)

data EntTy = Empty | Light | MapModel | PlayerStart | EnvMap | Particles
           | Sound | SpotLight | GameSpecific
  deriving (Show,Ord,Eq,Enum,Generic,Binary)


-- Surfaces --------------------------------------------------------------------

data Surface a = UnspecifiedSurface
               | BasicSurface !SurfaceInfo !a
               | MergedSurface !SurfaceInfo !SurfaceInfo !a
  deriving (Show,Ord,Eq,Generic,Binary)

data FaceInfo = FlatFaces !(Six (Surface ()))
              | ShapedFaces !(Six (Surface SurfaceNormals))
  deriving (Show,Ord,Eq,Generic,Binary)

data SurfaceInfo = SurfaceInfo {
    surfaceA ∷ ![Word8]
  , surfaceB ∷ !(Word8,Word8)
  , surfaceC ∷ !(Word16,Word16)
  , surfaceD ∷ !(Word8,Word8)
  } deriving (Eq,Ord,Show,Generic,Binary)

data MergeInfo = MergeInfo {
    mergeAA ∷ !Word16
  , mergeAB ∷ !Word16
  , mergeBA ∷ !Word16
  , mergeBB ∷ !Word16
  } deriving (Eq,Ord,Show,Generic,Binary)

data BVec = BVec { bvA ∷ !Word8, bvB ∷ !Word8, bvC ∷ !Word8 }
  deriving (Eq,Ord,Show,Generic,Binary)

type SurfaceNormals = Four BVec


-- Geometry --------------------------------------------------------------------

data Octree = Octree (LazyEight OctreeNode)
  deriving (Show,Ord,Eq,Generic,Binary)

data OctreeNode = NSolid !Textures !Properties
                | NEmpty !Textures !Properties
                | NDeformed !Offsets !Textures !Properties
                | NBroken Octree
                | NLodCube !Textures !Properties Octree
  deriving (Show,Ord,Eq,Generic,Binary)

type Material = Maybe Word8

data Properties = Properties !Material !FaceInfo
  deriving (Show,Ord,Eq,Generic,Binary)

data Offsets = Offsets !(Three Word32)
  deriving (Show,Ord,Eq,Generic,Binary)


-- Serial Instances -------------------------------------------------------

instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Three a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Four a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Six a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Eight a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LazyEight a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Surface a)

instance Monad m ⇒ SC.Serial m OGZ
instance Monad m ⇒ SC.Serial m OGZVar
instance Monad m ⇒ SC.Serial m OGZVal
instance Monad m ⇒ SC.Serial m Extras
instance Monad m ⇒ SC.Serial m TextureMRU
instance Monad m ⇒ SC.Serial m Textures
instance Monad m ⇒ SC.Serial m Entity
instance Monad m ⇒ SC.Serial m Vec3
instance Monad m ⇒ SC.Serial m EntTy
instance Monad m ⇒ SC.Serial m FaceInfo
instance Monad m ⇒ SC.Serial m SurfaceInfo
instance Monad m ⇒ SC.Serial m MergeInfo
instance Monad m ⇒ SC.Serial m BVec
instance Monad m ⇒ SC.Serial m Octree
instance Monad m ⇒ SC.Serial m Properties
instance Monad m ⇒ SC.Serial m Offsets
instance Monad m ⇒ SC.Serial m GameType
instance (Monad m) ⇒ SC.Serial m OctreeNode


-- Arbitrary Instances -------------------------------------------------------

derive makeArbitrary ''Three
derive makeArbitrary ''Four
derive makeArbitrary ''Six
derive makeArbitrary ''Eight
derive makeArbitrary ''LazyEight
derive makeArbitrary ''OGZ
derive makeArbitrary ''OGZVar
derive makeArbitrary ''OGZVal
derive makeArbitrary ''Extras
derive makeArbitrary ''TextureMRU
derive makeArbitrary ''Textures
derive makeArbitrary ''Entity
derive makeArbitrary ''Vec3
derive makeArbitrary ''EntTy
derive makeArbitrary ''Surface
derive makeArbitrary ''FaceInfo
derive makeArbitrary ''SurfaceInfo
derive makeArbitrary ''MergeInfo
derive makeArbitrary ''BVec
derive makeArbitrary ''Octree
derive makeArbitrary ''Properties
derive makeArbitrary ''Offsets
derive makeArbitrary ''GameType

arb ∷ Arbitrary a ⇒ Gen a
arb = arbitrary

genOctreeWDepth ∷ Int → Gen Octree
genOctreeWDepth d = do
  let b = genOctreeNodeWDepth d
  Octree <$> (LazyEight <$> b <*> b <*> b <*> b <*> b <*> b <*> b <*> b)

genOctreeNodeWDepth ∷ Int → Gen OctreeNode
genOctreeNodeWDepth d = do
  depthBelow ← (`mod` (d∷Int)) <$> arb
  let modTagBy = if depthBelow <= 0 then 3 else 4 ∷ Int
  ty ← (`mod` modTagBy) <$> arb

  case ty of
    0 → NEmpty <$> arb <*> arb
    1 → NSolid <$> arb <*> arb
    2 → NDeformed <$> arb <*> arb <*> arb
    3 → NBroken <$> genOctreeWDepth depthBelow
    4 → NLodCube <$> arb <*> arb <*> genOctreeWDepth depthBelow
    _ → error "The impossible happened in genOctreeNodeWDepth."

instance Arbitrary OctreeNode where arbitrary = genOctreeNodeWDepth 3
