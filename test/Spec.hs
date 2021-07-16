module Main where

import qualified Hspec.DBSpec
import           System.IO
import           Test.Hspec   (Spec, describe, hspec)

main :: IO ()
main = do
  withFile "/tmp/pgto.log" AppendMode $
    hspec . spec stdin

spec :: Handle -> Handle -> Spec
spec i o = do
  describe "pg-transact-hspec" $ do
    Hspec.DBSpec.defaultSpec
    Hspec.DBSpec.customSpec i o
