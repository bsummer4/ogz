{-
  Resources:

    - http://incoherency.co.uk/interest/sauer_map.html
    - https://www.haskell.org/haskellwiki/Dealing_with_binary_data#Binary_parsing
    - http://hackage.haskell.org/package/binary
    - http://hackage.haskell.org/package/bytestring-0.9.0.4/docs/Data-ByteString-Lazy.html

  TODOs

    - TODO Header doesn't need to be a part of OGZ. The header can
      be reconstructed from other information in the OGZ.
    - TODO Too many data types! Use raw types, and use Get/Put directorly
    - TODO Expose a sane data type, the user doesn't need all of these details.
    - TODO Rework the Properties data type. We don't to store the mask!
    - TODO The MergeInfo data is parsed, but not stored.
-}

{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module OGZ where

import ClassyPrelude

import           Codec.Compression.GZip (compress,decompress)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word
import           Prelude.Unicode
import           Test.QuickCheck hiding ((.&.))
import           Data.Bits
import           Numeric
import           Text.Printf
import           Data.DeriveTH


-- Data Types ----------------------------------------------------------------

data OGZ = OGZ !Word32 ![OGZVar] !Text !Extras !TextureMRU ![Entity] !Octree
  deriving (Show,Ord,Eq)

data OGZVar = OGZVar !BL.ByteString !OGZVal
  deriving (Show,Ord,Eq)

data Extras = Extras !Word16 !Word16
  deriving (Show,Ord,Eq)

data TextureMRU = TextureMRU ![Word16]
  deriving (Show,Ord,Eq)

data OGZVal = OInt !Word32 | OFloat !Float | OStr !BL.ByteString
  deriving (Show,Ord,Eq)

data EntTy = Empty | Light | MapModel | PlayerStart | EnvMap | Particles
           | Sound | SpotLight | GameSpecific
  deriving (Show,Ord,Eq,Enum)

data Vec3 = Vec3 !Float !Float !Float
  deriving (Show,Ord,Eq)

data Entity = Entity !Vec3 !EntTy !Word16 !Word16 !Word16 !Word16 !Word16 !Word8
  deriving (Show,Ord,Eq)

data Textures = Textures !(Six Word16)
  deriving (Show,Ord,Eq)

data Offsets = Offsets !(Three Word32)
  deriving (Show,Ord,Eq)

data Properties = Properties !Word8 [Maybe(SurfaceInfo,Maybe SurfaceNormals,Maybe SurfaceInfo)]
  deriving (Show,Ord,Eq)

data Octree = Octree (Eight OctreeNode)
  deriving (Show,Ord,Eq)

data OctreeNode = NSolid !Textures !Properties
                | NEmpty !Textures !Properties
                | NDeformed !Offsets !Textures !Properties
                | NBroken Octree
                | NLodCube !Textures !Properties Octree
  deriving (Show,Ord,Eq)

type SurfaceInfo = ([Word8], (Word8,Word8), (Word16,Word16), (Word8,Word8))
type MergeInfo = ((Word16,Word16),(Word16,Word16))
type BVec = (Word8, Word8, Word8)
type SurfaceNormals = Four BVec


-- Internal Types ------------------------------------------------------------

data Three a = Three a a a
  deriving (Show,Ord,Eq)

data Four a = Four a a a a
  deriving (Show,Ord,Eq)

data Six a = Six a a a a a a
  deriving (Show,Ord,Eq)

data Eight a = Eight a a a a a a a a
  deriving (Show,Ord,Eq)

data Header = Hdr
  { hdrMagic      ∷ Four Word8 -- Always "OCTA"
  , hdrVersion    ∷ !Word32 -- Always 29.
  , hdrHeaderSize ∷ !Word32 -- Always 36. This is the number of bytes in the header.
  , hdrWorldSize  ∷ !Word32 -- Length of one size of the world-cube. Should be a power of two.
  , hdrNumEnts    ∷ !Word32 -- Number of entities. Derivable from other information
  , hdrNumPvs     ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , hdrLightMaps  ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , hdrBlendMaps  ∷ !Word32 -- I don't care about lighting, so this is always zero.
  , hdrNumVars    ∷ !Word32 -- Number of OGZVars. Derivable from other information.
  } deriving (Ord,Eq,Show)


-- Utilities -----------------------------------------------------------------

octa ∷ Four Word8
octa = Four (x 'O') (x 'C') (x 'T') (x 'A')
  where x = fromIntegral . ord

deriveHeader ∷ OGZ → Header
deriveHeader (OGZ worldSize vars _ _ _ ents _) =
  Hdr octa 29 36 worldSize (fromIntegral $ length ents) 0 0 0 (fromIntegral $ length vars)

octreeNodeCount ∷ OctreeNode → Int
octreeNodeCount (NSolid _ _) = 1
octreeNodeCount (NEmpty _ _) = 1
octreeNodeCount (NDeformed _ _ _) = 1
octreeNodeCount (NBroken children) = 1 + octreeCount children
octreeNodeCount (NLodCube _ _ children) = 1 + octreeCount children

octreeCount ∷ Octree → Int
octreeCount (Octree(Eight a b c d e f g h)) =
  sum $ octreeNodeCount <$> [a,b,c,d,e,f,g,h]

ogzNodes ∷ OGZ → Int
ogzNodes (OGZ _ _ _ _ _ _ tree) = octreeCount tree


-- Binary Instances ----------------------------------------------------------

getStr = do
  nmLen ← getWord16le
  BL.pack <$> replicateM (fromIntegral nmLen) getWord8

dumpStr s = do
  putWord16le $ fromIntegral $ BL.length s
  mapM_ putWord8 $ BL.unpack s

instance Binary a => Binary (Three a) where
  put (Three a b c) = put a >> put b >> put c
  get = Three <$> get <*> get <*> get

instance Binary a => Binary (Four a) where
  put (Four a b c d) = put a >> put b >> put c >> put d
  get = Four <$> get <*> get <*> get <*> get

instance Binary a => Binary (Six a) where
  put (Six a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get = Six <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary a => Binary (Eight a) where
  get = Eight <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  put (Eight a b c d e f g h) =
    put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h

instance Binary Header where
  put (Hdr m a b c d e f g h) = do put m >> mapM_ putWord32le [a,b,c,d,e,f,g,h]
  get = Hdr <$> get <*> i <*> i <*> i <*> i <*> i <*> i <*> i <*> i
          where i = getWord32le


instance Binary OGZVar where
  put (OGZVar nm val) = do
    putWord8 $ case val of {(OInt _)→0; (OFloat _)→1; (OStr _)→2}
    dumpStr nm
    case val of OInt i → putWord32le i
                OFloat f → putFloat32le f
                OStr s → dumpStr s

  get = do
    ty ← getWord8
    nm ← getStr
    val ← case ty of 0 → OInt <$> getWord32le
                     1 → OFloat <$> getFloat32le
                     2 → OStr <$> getStr
                     _ → error "Invalid var type code!"

    return $ OGZVar nm val

instance Binary Extras where
  get = Extras <$> getWord16le <*> getWord16le
  put (Extras x y) = putWord16le x >> putWord16le y

instance Binary TextureMRU where
  get = do len ← getWord16le
           TextureMRU <$> replicateM (fromIntegral len) getWord16le
  put (TextureMRU l) = do putWord16le $ fromIntegral $ length l
                          mapM_ putWord16le l

instance Binary EntTy where
  put = putWord8 . fromIntegral . fromEnum
  get = do w ← fromIntegral <$> getWord8
           if w ≤ 8 then return $ toEnum w
                    else do pass -- traceM $ "Invalid entity type! " <> show w
                            return $ toEnum (w `mod` 9)

instance Binary Vec3 where
  get = Vec3 <$> getFloat32le <*> getFloat32le <*> getFloat32le
  put (Vec3 a b c) = mapM_ putFloat32le [a,b,c]

instance Binary Entity where
  get = do
    pos ← get
    let gw = getWord16le
    a←gw; b←gw; c←gw; d←gw; e←gw
    ty ← get
    reserved ← get
    return $ Entity pos ty a b c d e reserved

  put (Entity pos ty a b c d e reserved) = do
    put pos
    mapM_ putWord16le [a,b,c,d,e]
    put ty
    putWord8 reserved

instance Binary Octree where
  put (Octree(Eight a b c d e f g h)) = mapM_ put [a,b,c,d,e,f,g,h]
  get = Octree <$> (Eight <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get)

instance Binary Offsets where
  put (Offsets(Three a b c)) = mapM_ putWord32le [a,b,c]
  get = Offsets <$> (Three <$> w <*> w <*> w)
          where w = getWord32le

pass ∷ Monad m ⇒ m ()
pass = return()

surfaceLayer ∷ SurfaceInfo → Word8
surfaceLayer (_,_,_,(_,layer)) = layer

getMergeInfo ∷ Get MergeInfo
getMergeInfo = do
  a ← getWord16le
  b ← getWord16le
  c ← getWord16le
  d ← getWord16le
  return ((a,b),(c,d))

getSurfaceInfo ∷ Get SurfaceInfo
getSurfaceInfo = do
  texcoords ← replicateM 8 getWord8
  w ← getWord8
  h ← getWord8
  x ← getWord16le
  y ← getWord16le
  lmid ← getWord8
  layer ← getWord8
  return (texcoords, (w,h), (x,y), (lmid,layer))

getBVec ∷ Get BVec
getBVec = do
  x ← getWord8
  y ← getWord8
  z ← getWord8
  return (x,y,z)

getSurfaceNormals ∷ Get SurfaceNormals
getSurfaceNormals = Four <$> getBVec <*> getBVec <*> getBVec <*> getBVec

instance Binary Properties where
  put (Properties mask' surfaces) = do

    -- TODO This is a hack because the information in Mask is redundant.
    let mask = mask' .&. (complement 0x3F)

    putWord8 mask

    if not (null surfaces) then error "Not implemented!" else pass

    if testBit mask 7 then putWord8 0
                      else pass

  get = do
    mask ← getWord8

    material ← if testBit mask 7
                 then Just <$> getWord8
                 else return Nothing

    surfacesPre ← if 0≡(mask .&. 0x3F)
      then return [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      else do
        let normalsFlag = testBit mask 6
        forM (testBit mask <$> [0..5]) $ \flag → do
          if not flag then return Nothing else do
            surf ← getSurfaceInfo
            norm ← if normalsFlag
                     then Just <$> getSurfaceNormals
                     else return Nothing
            let blendBit = testBit (surfaceLayer surf) 1 -- (surf&(1<<1))
            return $ Just (surf,norm,blendBit)

    let fillBlendBit Nothing = return Nothing
        fillBlendBit (Just(s,n,False)) = return $ Just(s,n,Nothing)
        fillBlendBit (Just(s,n,True)) = do
          info ← getSurfaceInfo
          return $ Just (s,n,Just info)

    surfaces ← mapM fillBlendBit surfacesPre

    return $ Properties mask surfaces

instance Binary Textures where
  put (Textures(Six a b c d e f)) = mapM_ putWord16le [a,b,c,d,e,f]
  get = do
    a ← getWord16le
    b ← getWord16le
    c ← getWord16le
    d ← getWord16le
    e ← getWord16le
    f ← getWord16le

    return $ Textures $ Six a b c d e f

dumpBytes ∷ [Word8] → String
dumpBytes = r 0 where
  r i [] = ""
  r 16 bs = '\n' : r 0 bs
  r 0 (b:bs) = printf "0x%02x" b ++ r 1 bs
  r i (b:bs) = " " ++ printf "0x%02x" b ++ r (i+1) bs

instance Binary OctreeNode where
  put (NBroken childs)        = putWord8 0 >> put childs
  put (NEmpty ts ps)          = putWord8 1 >> put ts >> put ps
  put (NSolid ts ps)          = putWord8 2 >> put ts >> put ps
  put (NDeformed ts ps offs)  = putWord8 3 >> put ts >> put ps >> put offs
  put (NLodCube ts ps childs) = putWord8 4 >> put ts >> put ps >> put childs

  get = do
    firstByte ← getWord8
    let typeTag = firstByte .&. 0x07
        extraBits = firstByte .&. (complement 0x07)

    result ← case fromIntegral typeTag of
      0 → NBroken <$> get
      1 → NEmpty <$> get <*> get
      2 → NSolid <$> get <*> get
      3 → NDeformed <$> get <*> get <*> get
      4 → NLodCube <$> get <*> get <*> get
      n → fail $ "Invalid octree node tag: " <> show n

    if fromIntegral typeTag ≡ 0 then return result else do

      if not(testBit extraBits 7) then return() else do
        merged ← getWord8
        if not(testBit merged 7) then return() else do
          mergeInfos ← getWord8
          replicateM_ (popCount mergeInfos) getMergeInfo

      return result

getGameType ∷ Get Text
getGameType = do nmLen ← getWord8
                 result ← BS.pack <$> replicateM (fromIntegral nmLen) getWord8
                 nullChr ← getWord8
                 guard $ nullChr ≡ 0
                 return $ T.decodeUtf8 result

putGameType ∷ Text → Put
putGameType t = do
  let s = T.encodeUtf8 t
  putWord8 $ fromIntegral $ BS.length s
  mapM_ putWord8 $ BS.unpack s
  putWord8 0

instance Binary OGZ where
  put ogz@(OGZ _ vars gameTy extras mru ents tree) = do
    let h = deriveHeader ogz
    put h
    mapM_ put vars
    putGameType gameTy; put extras; put mru
    mapM_ put ents

  get = do
    hdr ← get

    let magic = hdrMagic hdr
        version = hdrVersion hdr

    if octa == magic then pass else
      fail "This is not a Sauerbraten map!"

    if 29 == version then pass else
      fail $ "Only version 29 is supported. This map has version: " <> show version

    vars ← replicateM (fromIntegral $ hdrNumVars hdr) get
    gameTy ← getGameType
    extras ← get
    mru ← get
    ents ← replicateM (fromIntegral $ hdrNumEnts hdr) get
    tree ← get
    return $ OGZ (hdrWorldSize hdr) vars gameTy extras mru ents tree



-- Arbitrary Instances -------------------------------------------------------

arb ∷ Arbitrary a => Gen a
arb = arbitrary

genOctreeWDepth ∷ Int → Gen Octree
genOctreeWDepth d = do
  let b = genOctreeNodeWDepth d
  Octree <$> (Eight <$> b <*> b <*> b <*> b <*> b <*> b <*> b <*> b)

genOctreeNodeWDepth ∷ Int → Gen OctreeNode
genOctreeNodeWDepth d = do
  depthBelow ← (`mod` (d∷Int)) <$> arb
  let modTagBy = if depthBelow ≤ 0 then 3 else 4 ∷ Int
  ty ← (`mod` modTagBy) <$> arb

  case ty of
    0 → NEmpty <$> arb <*> arb
    1 → NSolid <$> arb <*> arb
    2 → NDeformed <$> arb <*> arb <*> arb
    3 → NBroken <$> genOctreeWDepth depthBelow
    4 → NLodCube <$> arb <*> arb <*> genOctreeWDepth depthBelow
    _ → error "The impossible happened in genOctreeNodeWDepth."

instance Arbitrary BL8.ByteString where arbitrary = BL8.pack <$> arbitrary
instance Arbitrary OctreeNode where arbitrary = genOctreeNodeWDepth 1

derive makeArbitrary ''OGZVal
derive makeArbitrary ''OGZVar
derive makeArbitrary ''Entity
derive makeArbitrary ''Vec3
derive makeArbitrary ''EntTy
derive makeArbitrary ''TextureMRU
derive makeArbitrary ''Extras
derive makeArbitrary ''Three
derive makeArbitrary ''Four
derive makeArbitrary ''Six
derive makeArbitrary ''Eight
derive makeArbitrary ''Textures
derive makeArbitrary ''Offsets
derive makeArbitrary ''Octree
derive makeArbitrary ''Properties
derive makeArbitrary ''Header


-- Tests ---------------------------------------------------------------------

reversibleSerialization ∷ Eq a => Binary a => a → Bool
reversibleSerialization x = x ≡ decode(encode x)

checkReversibleSerializations ∷ IO()
checkReversibleSerializations = do
  let f ∷ Eq a => Binary a => a → Bool
      f = reversibleSerialization

  quickCheck $ (f ∷ Three Word8 → Bool)
  quickCheck $ (f ∷ Six Word8 → Bool)
  quickCheck $ (f ∷ Four Word8 → Bool)
  quickCheck $ (f ∷ Eight Word8 → Bool)
  quickCheck $ (f ∷ Header → Bool)
  quickCheck $ (f ∷ OGZVar → Bool)
  quickCheck $ (f ∷ Extras → Bool)
  quickCheck $ (f ∷ TextureMRU → Bool)
  quickCheck $ (f ∷ EntTy → Bool)
  quickCheck $ (f ∷ Vec3 → Bool)
  quickCheck $ (f ∷ Entity → Bool)
  quickCheck $ (f ∷ Textures → Bool)
  quickCheck $ (f ∷ Offsets → Bool)

  -- quickCheck $ f (undefined ∷ Properties → Bool)
  -- quickCheck $ f (undefined ∷ Octree → Bool)
  -- quickCheck $ f (undefined ∷ OctreeNode → Bool)
  -- quickCheck $ f (undefined ∷ OGZ → Bool)

check = checkReversibleSerializations

test ∷ IO ()
test = do
  let builtInMaps = ("/Users/b/fuck/sauerbraten-code/packages/base/" ++) <$>
        [ "DM_BS1.ogz", "aard3c.ogz", "academy.ogz", "akroseum.ogz"
        , "aqueducts.ogz", "arabic.ogz", "asteroids.ogz", "authentic.ogz"
        , "berlin_wall.ogz", "box_demo.ogz", "bt_falls.ogz", "c_egypt.ogz"
        , "c_valley.ogz", "campo.ogz", "canyon.ogz", "capture_night.ogz"
        , "castle_trap.ogz", "complex.ogz", "core_transfer.ogz"
        , "corruption.ogz", "curvedm.ogz", "curvy_castle.ogz", "cwcastle.ogz"
        , "damnation.ogz", "darkdeath.ogz", "deathtek.ogz", "desecration.ogz"
        , "dock.ogz", "door_demo.ogz", "douze.ogz", "duel7.ogz", "duel8.ogz"
        , "duomo.ogz", "dust2.ogz", "europium.ogz", "face-capture.ogz"
        , "fanatic_quake.ogz", "fb_capture.ogz", "fc3.ogz", "fc4.ogz"
        , "firstevermap.ogz", "flagstone.ogz", "forge.ogz", "fragplaza.ogz"
        , "frostbyte.ogz", "frozen.ogz", "guacamole.ogz", "hades.ogz"
        , "hallo.ogz", "hog2.ogz", "industry.ogz", "injustice.ogz", "island.ogz"
        , "justice.ogz", "kalking1.ogz", "katrez_d.ogz", "kffa.ogz"
        , "killcore3.ogz", "killfactory.ogz", "kmap5.ogz", "konkuri-to.ogz"
        , "ksauer1.ogz", "l_ctf.ogz", "ladder.ogz", "level9.ogz", "lost.ogz"
        , "lostinspace.ogz", "mach2.ogz", "mbt1.ogz", "mbt2.ogz", "memento.ogz"
        , "metl2.ogz", "metl3.ogz", "metl4.ogz", "monastery.ogz", "moonlite.ogz"
        , "mpsp10.ogz", "mpsp6a.ogz", "mpsp6b.ogz", "mpsp6c.ogz", "mpsp9a.ogz"
        , "mpsp9b.ogz", "mpsp9c.ogz", "neondevastation.ogz", "neonpanic.ogz"
        , "nevil_c.ogz", "nmp4.ogz", "nmp8.ogz", "nmp9.ogz", "oasis.ogz"
        , "oddworld.ogz", "ogrosupply.ogz", "orbe.ogz", "orion.ogz"
        , "osiris.ogz", "ot.ogz", "paradigm.ogz", "park.ogz", "pgdm.ogz"
        , "ph-capture.ogz", "phosgene.ogz", "platform_demo.ogz"
        , "powerplant.ogz", "recovery.ogz", "redemption.ogz", "refuge.ogz"
        , "reissen.ogz", "relic.ogz", "river_c.ogz", "roughinery.ogz"
        , "ruby.ogz", "sacrifice.ogz", "sauerbraten.ogz", "sdm1.ogz"
        , "secondevermap.ogz", "serenity.ogz", "shadowed.ogz", "shindou.ogz"
        , "shinmei1.ogz", "shipwreck.ogz", "spiralz.ogz", "stemple.ogz"
        , "tartech.ogz", "tejen.ogz", "tempest.ogz", "thetowers.ogz", "thor.ogz"
        , "torment.ogz", "turbine.ogz", "urban_c.ogz", "valhalla.ogz"
        , "venice.ogz", "wake5.ogz", "wdcd.ogz"
        ]

  let filenames = "example.ogz" : builtInMaps

  forM_ filenames $ \filename → do
    result ← (runGetOrFail get . decompress) <$> BL.readFile filename
    case result of
      Left (_,_,errmsg) → putStrLn $ T.pack $
        printf "FAIL: %s (%s)" errmsg filename
      Right (_,_,result) → putStrLn $ T.pack $
        printf "PASS: %d node were parsed (%s)" (ogzNodes result) filename
