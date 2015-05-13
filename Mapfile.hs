{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax #-}

module Mapfile where

import           Control.Exception    (assert)
import           Control.Monad        (guard, replicateM, replicateM_)
import           Data.Binary          (Get, Put, decode, getWord8, putWord8)
import qualified Data.Binary          as B
import           Data.Binary.Get      (getWord16le, getWord32le, getWord8,
                                       runGet)
import           Data.Binary.IEEE754  (getFloat32le, putFloat32le)
import           Data.Binary.Put      (putWord16le, putWord32le, putWord8,
                                       runPut)
import           Data.Bits            (complement, popCount, testBit, (.&.))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char            (ord)
import           Data.Foldable
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Data.Word
import           Prelude.Unicode
import           Test.QuickCheck
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


-- Testing ---------------------------------------------------------------------

loadOGZVarTestCases ∷ IO [BSL.ByteString]
loadOGZVarTestCases = decode <$> BSL.readFile "testdata/ogzvars"

assertM c = assert c (return())

dumpBytes ∷ [Word8] → String
dumpBytes = r 0 where
  r i [] = ""
  r 32 bs = '\n' : r 0 bs
  r 0 (b:bs) = printf "%02x" b ++ r 1 bs
  r i (b:bs) = " " ++ printf "%02x" b ++ r (i+1) bs

-- TODO How can I tell quickcheck to use a file as a source of inputs?
runOGZVarTestCases ∷ IO ()
runOGZVarTestCases = do
  cases ← loadOGZVarTestCases
  forM_ cases $ \bs → do
    let var∷OGZVar = runGet load bs
    if bs ≡ runPut (dump var)
      then printf "[PASS] (%s) ≡ [%s]\n" (show var) (dumpBytes $ BSL.unpack bs)
      else printf "[FAIL] [%s]\n" (dumpBytes $ BSL.unpack bs)

reversibleDump ∷ (Eq t,Mapfile t) ⇒ t → Bool
reversibleDump x = (runGet load $ runPut $ dump x) ≡ x

test = do
  runOGZVarTestCases
  quickCheck (reversibleDump ∷ OGZVar → Bool)
