{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax                                                 #-}

module Mapfile where

import           Control.Exception      (assert)
import           Control.Monad          (guard, replicateM, replicateM_, unless)
import           Data.Binary            (Get, Put, decode, getWord8, putWord8)
import qualified Data.Binary            as B
import           Data.Binary.Get        (getWord16le, getWord32le, getWord8,
                                         runGet, runGetOrFail)
import           Data.Binary.IEEE754    (getFloat32le, putFloat32le)
import           Data.Binary.Put        (putWord16le, putWord32le, putWord8,
                                         runPut)
import           Data.Bits              (complement, popCount, testBit, (.&.))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.ByteString.Short  (ShortByteString)
import qualified Data.ByteString.Short  as BSS
import           Data.Char              (ord)
import           Data.Either            (isRight)
import           Data.Foldable
import           Data.IORef
import           Data.Monoid            ((<>))
import           Data.Proxy
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Word
import           Prelude.Unicode
import qualified Test.QuickCheck        as QC
import qualified Test.SmallCheck        as SC
import qualified Test.SmallCheck.Series as SC
import           Test.Tasty
import qualified Test.Tasty.QuickCheck  as QC
import qualified Test.Tasty.SmallCheck  as SC
import           Text.Printf
import           Types


-- Types and Type Classes ------------------------------------------------------

class Mapfile t where
  dump ∷ t → Put
  load ∷ Get t


-- Mapfile Instances -----------------------------------------------------------

instance Mapfile OGZVar where

  dump (OGZVar nm val) = do
    let dumpStr s = do putWord16le $ fromIntegral $ BS.length s
                       mapM_ putWord8 $ BS.unpack s
    putWord8 $ case val of {(OInt _)→0; (OFloat _)→1; (OStr _)→2}
    dumpStr nm
    case val of OInt i → putWord32le i
                OFloat f → putFloat32le f
                OStr s → dumpStr s

  load = do
    let getStr = do nmLen ← getWord16le
                    BS.pack <$> replicateM (fromIntegral nmLen) getWord8
    ty ← getWord8
    nm ← getStr
    val ← case ty of 0 → OInt <$> getWord32le
                     1 → OFloat <$> getFloat32le
                     2 → OStr <$> getStr
                     _ → error "Invalid var type code!"

    return $ OGZVar nm val


instance Mapfile GameType where

  load = do
      nmLen ← getWord8
      result ← BSS.pack <$> replicateM (fromIntegral nmLen) getWord8
      nullChr ← getWord8
      guard $ nullChr ≡ 0
      return $ GameType result

  dump (GameType bs) = do
      putWord8 $ fromIntegral $ BSS.length bs
      mapM_ putWord8 $ BSS.unpack bs
      putWord8 0


-- Properties ------------------------------------------------------------------

-- This assumes that the bytestrings are valid.
reversibleLoad ∷ (Mapfile a,Eq a) ⇒ Proxy a → BSL.ByteString → Bool
reversibleLoad p bs =
    case runGetOrFail load bs of
        Right (_,_,x) → bs ≡ runPut(dump $ x `asProxyTypeOf` p)
        _             → False

reversibleDump ∷ (Show t,Eq t,Mapfile t) ⇒ t → Bool
reversibleDump x = (runGet load $ runPut $ dump x) ≡ x


-- Test Suite ------------------------------------------------------------------

assertM c = assert c (return())

dumpBytes ∷ [Word8] → String
dumpBytes = r 0 where
  r i [] = ""
  r 32 bs = '\n' : r 0 bs
  r 0 (b:bs) = printf "%02x" b ++ r 1 bs
  r i (b:bs) = " " ++ printf "%02x" b ++ r (i+1) bs

loadTestSuite ∷ (Eq a,Mapfile a) ⇒ String → Proxy a → IO (SC.Property IO)
loadTestSuite fn p = do
    cases ← decode <$> BSL.readFile ("testdata/" <> fn)
    let series = SC.generate $ const cases
    return $ SC.over series $ reversibleLoad p

test = do
    ogzVarSuite   ← loadTestSuite "ogzvars" (Proxy∷Proxy OGZVar)
    gameTypeSuite ← loadTestSuite "gametypes" (Proxy∷Proxy GameType)

    defaultMain $ testGroup "tests"
        [ QC.testProperty "x∷OGZVar ≡ load(dump OGZVar)"
              (reversibleDump ∷ OGZVar → Bool)
        , SC.testProperty "x∷OGZVar ≡ load(dump OGZVar)"
              (reversibleDump ∷ OGZVar → Bool)

        , QC.testProperty "x∷GameType ≡ load(dump GameType)"
              (reversibleDump ∷ GameType → Bool)
        , SC.testProperty "x∷GameType ≡ load(dump GameType)"
              (reversibleDump ∷ GameType → Bool)

        , SC.testProperty "bs ≡ dump((load bs)∷OGZVar)" $
              ogzVarSuite

        , SC.testProperty "bs ≡ dump(load bs ∷ GameType)" $
              gameTypeSuite
        ]
