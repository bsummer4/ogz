{-
  Resources:

    - http://incoherency.co.uk/interest/sauer_map.html
    - https://www.haskell.org/haskellwiki/Dealing_with_binary_data#Binary_parsing
    - http://hackage.haskell.org/package/binary
    - http://hackage.haskell.org/package/bytestring-0.9.0.4/docs/Data-ByteString-Lazy.html
-}

{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, RecordWildCards #-}

module OGZ where

import ClassyPrelude

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Word
import           Prelude.Unicode
import           Codec.Compression.GZip (compress,decompress)

data Header = Hdr
  { ogzVersion    ∷ !Word32
  , ogzHeaderSize ∷ !Word32
  , ogzWolrdSize  ∷ !Word32
  , ogzWorldSize  ∷ !Word32
  , ogzNumEnts    ∷ !Word32
  , ogzNumPvs     ∷ !Word32
  , ogzLightMaps  ∷ !Word32
  , ogzBlendMaps  ∷ !Word32
  , ogzNumVars    ∷ !Word32
  } deriving (Ord,Eq,Show)

data Magic = Magic
  deriving (Ord,Eq,Show)

data OGZ = OGZ Magic Header
  deriving (Show,Ord,Eq)

hdrFields ∷ Header → [Word32]
hdrFields (Hdr a b c d e f g h i) = [a,b,c,d,e,f,g,h,i]

instance Binary Magic where
  put _ = forM_ "octa" $ putWord8 . fromIntegral . ord
  get =  do
    o ← getWord8
    c ← getWord8
    t ← getWord8
    a ← getWord8
    let ok = \(n,c) → ord c≡fromIntegral n
    guard $ all ok [(o,'O'),(c,'C'),(t,'T'),(a,'A')]
    return $ Magic

instance Binary Header where
  put h = mapM_ putWord32le (hdrFields h)
  get = Hdr <$> i <*> i <*> i <*> i <*> i <*> i <*> i <*> i <*> i
          where i = getWord32le

instance Binary OGZ where
  put (OGZ m h) = put m >> put h
  get = OGZ <$> get <*> get

test = do
  let filename = "/Users/b/fuck/sauerbraten-code/packages/base/aard3c.ogz"
  o@OGZ{..} ← (decode . decompress) <$> BL.readFile filename
  print o
