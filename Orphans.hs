{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans() where

import           Control.Applicative
import           Data.Binary
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Short  as BSS
import qualified Data.Text              as T
import qualified Test.QuickCheck        as QC
import qualified Test.SmallCheck.Series as SC

instance Binary BSS.ShortByteString where
  put = put . BSS.fromShort
  get = BSS.toShort <$> get

instance QC.Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> QC.arbitrary

instance QC.Arbitrary BSL.ByteString where
  arbitrary = BSL.pack <$> QC.arbitrary

instance QC.Arbitrary BSS.ShortByteString where
  arbitrary = BSS.pack . take 256 <$> QC.arbitrary

instance QC.Arbitrary T.Text where
  arbitrary = T.pack <$> QC.arbitrary

instance Monad m ⇒ SC.Serial m BSL.ByteString where
  series = BSL.pack <$> SC.series

instance Monad m ⇒ SC.Serial m BS.ByteString where
  series = BS.pack <$> SC.series

instance Monad m => SC.Serial m Word8 where
  series = SC.generate $ \d -> [0 .. fromIntegral d]

instance Monad m => SC.Serial m Word16 where
  series = SC.generate $ \d -> [0 .. fromIntegral d]

instance Monad m => SC.Serial m Word32 where
  series = SC.generate $ \d -> [0 .. fromIntegral d]

instance Monad m ⇒ SC.Serial m BSS.ShortByteString where
  series = BSS.pack <$> SC.series
