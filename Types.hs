-- This module deals with how we represent the imformation that is
-- stored in an `.ogz` file.

{-# LANGUAGE TemplateHaskell #-}

module Types where

import HomoTuple

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Binary
import           Data.Bits
import           Data.ByteString        (ByteString)
import           Data.ByteString.Short  (ShortByteString)
import           Data.DeriveTH
import           Data.Maybe
import           GHC.Generics
import           Orphans                ()
import           Prelude.Unicode
import           Test.QuickCheck        (Arbitrary, Gen, arbitrary, choose)
import qualified Test.SmallCheck.Series as SC


-- High-Level Structure --------------------------------------------------------

newtype GameType = GameType { unGameType ∷ ShortByteString }
  deriving (Show,Ord,Eq,Generic)

-- TODO What does this mean? I think this is the maximum geometry
--      depth, but I need to double check that.
newtype WorldSize = WorldSize { unWorldSize ∷ Int }
  deriving (Show,Ord,Eq,Generic)

mkWorldSize ∷ Int → Maybe WorldSize
mkWorldSize n | n<1 ∨ n>31 = Nothing
mkWorldSize n              = Just $ WorldSize n

-- TODO What is the point of these? In all of the built-in maps,
--      these are both set to zero.
data Extras = Extras {
    extraEntInfoSize ∷ !Word16
  , extrasLength     ∷ !Word16
  } deriving (Show,Ord,Eq,Generic)

data OGZ = OGZ {
    ogzWorldSize  ∷ WorldSize
  , ogzVars       ∷ [OGZVar]
  , ogzGameType   ∷ GameType
  , ogzExtras     ∷ Extras
  , ogzTextureMRU ∷ TextureMRU
  , ogzEntities   ∷ [Entity]
  , ogzGeometry   ∷ Octree
  } deriving (Show,Ord,Eq,Generic)


-- Variables -------------------------------------------------------------------

data OGZVar = OGZVar !ByteString !OGZVal
  deriving (Show,Ord,Eq,Generic)

data OGZVal = OInt !Word32 | OFloat !Float | OStr !ByteString
  deriving (Show,Ord,Eq,Generic)


-- Texture Stuff ---------------------------------------------------------------

newtype TextureMRU = TextureMRU [Word16]
  deriving (Show,Ord,Eq,Generic)

newtype Textures = Textures (Tup6 Word16)
  deriving (Show,Ord,Eq,Generic)


-- Entities --------------------------------------------------------------------

-- TODO Decide if this is useful and then use it or toss it.
--
-- This just documents the data contained in various entities. Using
-- this type would be a lot more code and provide questionable
-- value. Also, it's not clear what to do with the unused parameters.
--
-- If we just enforce that they're always zero, this probably wont agree with
-- the actual bits-on-disk from the map files.
--
-- TODO Can we do an experiement to see if this is true?
--
-- Also, if it's NOT true, then maybe that doesn't matter. Instead
-- of requiring that mapfiles can be reproduce in a bit-perfect
-- way. We could store the expected parse along with all of the example
-- bytestrings. For example, the current types of an on-disk test-suite is [1]:
--
--     ∀(ty∷Mapfile a⇒Proxy a, file∷[ByteString]),
--       ∀bs←file,
--         bs ≡ dump (load bs `asProxyType` ty)
--
-- This might be too restrictive, the following[2] gives weaker
-- guarentees, but should be sufficient for preventing regressions:
--
--     ∀ (Mapfile a,Binary a) ⇒ file∷Map ByteString a,
--       ∀ (bs,v)←file,
--         load bs ≡ v
--
-- TODO Implement property #2, and make property #1 opt-in.
data EntData = AEmpty
             | ALight       { entLightSrc,entRadius,entIntensity ∷ !Word16 }
             | AMapModel    { entAngle,entIdx ∷ !Word16 }
             | APlayerStart { entAngle,entTeam ∷ !Word16 }
             | AEnvMap      { entRadius ∷ !Word16 }
             | AParticles
             | ASound
             | ASpotLight
             | AShells
             | ABullets
             | ARockets
             | ARounds
             | AGrenades
             | ACartridges
             | AHealth
             | ABoost
             | AGreenArmour
             | AYellowArmour
             | AQuad
             | ATeleport    { entIdx,entModel,entTag ∷ !Word16 }
             | ATeledest    { entAngle,entIdx ∷ !Word16 }
             | AMonster     { entAngle,entMonsterTy ∷ !Word16 }
             | ACarrot      { entTag,entCarrotTy ∷ !Word16 }
             | AJumpPad     { entPushX,entPushY,entPushZ ∷ !Word16 }
             | ABase
             | ARespawnPoint
             | ABox
             | ABarrel      { entAngle,entIdx,entWight,entHealth ∷ !Word16 }
             | APlatform    { entAngle,endIdx,entTag,entSpeed ∷ !Word16 }
             | AElevator    { entAngle,endIdx,entTag,entSpeed ∷ !Word16 }
             | AFlag
  deriving (Show,Ord,Eq,Generic)

data EntTy = Empty        | Light        | MapModel | PlayerStart
           | EnvMap       | Particles    | Sound    | SpotLight
           | Shells       | Bullets      | Rockets  | Rounds
           | Grenades     | Cartridges   | Health   | Boost
           | GreenArmour  | YellowArmour | Quad     | Teleport
           | Teledest     | Monster      | Carrot   | JumpPad
           | Base         | RespawnPoint | Box      | Barrel
           | Platform     | Elevator     | Flag
  deriving (Show,Ord,Enum,Bounded,Eq,Generic)

newtype Vec3 = Vec3 (Tup3 Float)
  deriving (Show,Ord,Eq,Generic)

newtype BVec3 = BVec3 (Tup3 Word8)
  deriving (Show,Ord,Eq,Generic)

bVec3ToVec3 ∷ BVec3 → Vec3
bVec3ToVec3 (BVec3(Tup3 x y z)) = Vec3 $ Tup3 (cvt x) (cvt y) (cvt z)
  where cvt c = (fromIntegral c * (2/255)) - 1

vec3ToBVec3 ∷ Vec3 → BVec3
vec3ToBVec3 (Vec3(Tup3 x y z)) = BVec3 $ Tup3 (cvt x) (cvt y) (cvt z)
  where cvt c = floor $ (c+1)*(255/2)

-- TODO Why does the `unusedByte` take on different values?
-- TODO If the `unusedByte` is just the result of uninitialized data, then
--      try to change the test framework to be robust to this shit.
-- TODO Instead of having a tag and then 80 bits of info, why not
--      use an ADT? Need to understand what the attr data means per
--      entity first, though.
data Entity = Entity {
    entityPosition ∷ !Vec3
  , entityAttrs    ∷ !(Tup5 Word16)
  , entityType     ∷ !EntTy
  , unusedByte     ∷ !Word8
  } deriving (Show,Ord,Eq,Generic)


-- Faces -----------------------------------------------------------------------

-- This contains the surface normal at each corner of a (square)
-- surface. This information is used by the engine in lighting calculations.
--
-- This can be derived from the rest of the geometry.
-- For now, we are storing it so that we can rebuild map files
-- bit-for-bit, but it might make sense to drop it eventually.
data LightMapTy =
  Ambient | Ambient1 | Bright | Bright1 | Dark | Dark1 | Reserved
  deriving (Eq,Ord,Enum,Show,Generic)

-- The type is stored in the lowest three bits, and the remaining
-- 5 are used to store an id.
newtype LightMap = LightMap { unLightMap ∷ Word8 }
    deriving (Eq,Ord,Show,Generic)

-- TODO What is this?
data Layer = Top | Bottom
  deriving (Eq,Ord,Show,Enum,Generic)

-- Per-surface lighting information.
data FaceInfo = FaceInfo {
    sfTexCoords ∷ !(Tup8 Word8)
  , sfDims      ∷ !(Tup2 Word8)
  , sfPos       ∷ !(Tup2 Word16)
  , sfLightMap  ∷ !LightMap
  , sfLayer     ∷ !Layer
  } deriving (Eq,Ord,Show,Generic)

data Face = Face       !FaceInfo
          | MergedFace !FaceInfo !FaceInfo
  deriving (Show,Ord,Eq,Generic)

newtype Normals = Normals (Tup4 BVec3)
  deriving (Show,Ord,Eq,Generic)

data FaceWithNormals = FaceWithNormals !Face !Normals
  deriving (Show,Ord,Eq,Generic)

data Faces = Faces        !(Tup6 (Maybe Face))
           | FacesNormals !(Tup6 (Maybe FaceWithNormals))
  deriving (Show,Ord,Eq,Generic)

lightMapTy ∷ LightMap → LightMapTy
lightMapTy = toEnum . fromIntegral . (.&. 0x07) . unLightMap

lightMapId ∷ LightMap → Word8
lightMapId = (`shiftR` 3) . unLightMap

mkLightMap ∷ LightMapTy → Word8 → Maybe LightMap
mkLightMap ty lmid = do
  let low3∷Word8  = fromIntegral(fromEnum ty)
  let high5∷Word8 = lmid
  guard $ high5 < 32
  return $ LightMap $ (high5 `shiftL` 3) .|. low3


-- Geometry --------------------------------------------------------------------

newtype MergeInfo = MergeInfo (Tup4 Word16)
  deriving (Eq,Ord,Show,Generic)

newtype Material = Material Word8
  deriving (Show,Ord,Eq,Generic)

-- TODO This is invalid: MergeData w Nothing where (w `testBit` 7)
-- TODO This is invalid: MergeData w (Just _) where (not (w `testBit` 7))
data MergeData = MergeData !Word8 !(Maybe (Tup8 (Maybe MergeInfo)))
  deriving (Eq,Ord,Show,Generic)

data Octree = NBroken (LzTup8 Octree)
            | NEmpty !Textures !Properties !(Maybe MergeData)
            | NSolid !Textures !Properties !(Maybe MergeData)
            | NDeformed !Offsets !Textures !Properties !(Maybe MergeData)
            | NLodCube !Textures !Properties (LzTup8 Octree) !(Maybe MergeData)
  deriving (Show,Ord,Eq,Generic)

data Properties = Properties !(Maybe Material) !Faces
  deriving (Show,Ord,Eq,Generic)

newtype Offsets = Offsets (Tup3 Word32)
  deriving (Show,Ord,Eq,Generic)


-- Serial Instances -------------------------------------------------------

instance NFData GameType
instance NFData WorldSize
instance NFData Extras
instance NFData OGZ
instance NFData OGZVar
instance NFData OGZVal
instance NFData TextureMRU
instance NFData Textures
instance NFData EntData
instance NFData EntTy
instance NFData Vec3
instance NFData BVec3
instance NFData Entity
instance NFData LightMapTy
instance NFData LightMap
instance NFData Layer
instance NFData FaceInfo
instance NFData Face
instance NFData Normals
instance NFData FaceWithNormals
instance NFData Faces
instance NFData MergeInfo
instance NFData Material
instance NFData MergeData
instance NFData Octree
instance NFData Properties
instance NFData Offsets

instance Binary GameType
instance Binary WorldSize
instance Binary Extras
instance Binary OGZ
instance Binary OGZVar
instance Binary OGZVal
instance Binary TextureMRU
instance Binary Textures
instance Binary EntData
instance Binary EntTy
instance Binary Vec3
instance Binary BVec3
instance Binary Entity
instance Binary LightMapTy
instance Binary LightMap
instance Binary Layer
instance Binary FaceInfo
instance Binary Face
instance Binary Normals
instance Binary FaceWithNormals
instance Binary Faces
instance Binary MergeInfo
instance Binary Material
instance Binary MergeData
instance Binary Octree
instance Binary Properties
instance Binary Offsets

instance Monad m ⇒ SC.Serial m BVec3 where series = BVec3 <$> SC.series
instance Monad m ⇒ SC.Serial m GameType where series = GameType <$> SC.series
instance Monad m ⇒ SC.Serial m LightMap where series = LightMap <$> SC.series
instance Monad m ⇒ SC.Serial m Material where series = Material <$> SC.series
instance Monad m ⇒ SC.Serial m MergeInfo where series = MergeInfo <$> SC.series
instance Monad m ⇒ SC.Serial m Normals where series = Normals <$> SC.series
instance Monad m ⇒ SC.Serial m Offsets where series = Offsets <$> SC.series
instance Monad m ⇒ SC.Serial m TextureMRU where series = TextureMRU <$> SC.series
instance Monad m ⇒ SC.Serial m Textures where series = Textures <$> SC.series
instance Monad m ⇒ SC.Serial m Vec3 where series = Vec3 <$> SC.series

instance Monad m ⇒ SC.Serial m EntTy
instance Monad m ⇒ SC.Serial m Octree
instance Monad m ⇒ SC.Serial m Entity
instance Monad m ⇒ SC.Serial m Extras
instance Monad m ⇒ SC.Serial m Face
instance Monad m ⇒ SC.Serial m FaceInfo
instance Monad m ⇒ SC.Serial m FaceWithNormals
instance Monad m ⇒ SC.Serial m Faces
instance Monad m ⇒ SC.Serial m Layer
instance Monad m ⇒ SC.Serial m OGZ
instance Monad m ⇒ SC.Serial m OGZVal
instance Monad m ⇒ SC.Serial m OGZVar
instance Monad m ⇒ SC.Serial m Properties

instance Monad m ⇒ SC.Serial m WorldSize where
  series = SC.generate $ \d → catMaybes $ mkWorldSize <$> [0..d]


-- Arbitrary Instances -------------------------------------------------------

derive makeArbitrary ''BVec3
derive makeArbitrary ''EntTy
derive makeArbitrary ''Entity
derive makeArbitrary ''Extras
derive makeArbitrary ''Face
derive makeArbitrary ''FaceInfo
derive makeArbitrary ''FaceWithNormals
derive makeArbitrary ''Faces
derive makeArbitrary ''GameType
derive makeArbitrary ''Layer
derive makeArbitrary ''LightMap
derive makeArbitrary ''Material
derive makeArbitrary ''MergeInfo
derive makeArbitrary ''Normals
derive makeArbitrary ''OGZ
derive makeArbitrary ''OGZVal
derive makeArbitrary ''OGZVar
derive makeArbitrary ''Offsets
derive makeArbitrary ''Properties
derive makeArbitrary ''TextureMRU
derive makeArbitrary ''Textures
derive makeArbitrary ''Vec3

arb ∷ Arbitrary a ⇒ Gen a
arb = arbitrary

genOctreeWDepth ∷ Int → Gen Octree
genOctreeWDepth d = do
  depthBelow ← (`mod` (d∷Int)) <$> arb
  let modTagBy = if depthBelow <= 0 then 3 else 4 ∷ Int
  ty ← (`mod` modTagBy) <$> arb

  let times8 x = LzTup8 <$> x <*> x <*> x <*> x <*> x <*> x <*> x <*> x
      children = times8 $ genOctreeWDepth depthBelow

  case ty of
    0 → NEmpty <$> arb <*> arb <*> arb
    1 → NSolid <$> arb <*> arb <*> arb
    2 → NDeformed <$> arb <*> arb <*> arb <*> arb

    -- Nodes with children. These aren't generated if d≤1.
    3 → NBroken <$> children
    4 → NLodCube <$> arb <*> arb <*> children <*> arb

    _ → error "The impossible happened in genOctreeWDepth."

instance Arbitrary Octree where
  arbitrary = genOctreeWDepth 3

instance Arbitrary WorldSize where
  arbitrary = do
    arbInt ← arbitrary
    let arbSize = 1 + (arbInt `mod` 31)
    Just ws ← return $ mkWorldSize arbSize
    return ws

instance Arbitrary MergeData where
  arbitrary = do
    tag ∷ Word8 ← (`clearBit` 7) <$> arbitrary
    MergeData tag <$> arbitrary

instance Monad m ⇒ SC.Serial m MergeData
