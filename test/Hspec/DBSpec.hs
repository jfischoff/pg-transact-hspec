{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hspec.DBSpec where

import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Functor                     (void)
import           Data.Monoid                      (Last (Last))
import           Database.PostgreSQL.Simple       (Only (..))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Transact     (DBT, execute, execute_,
                                                   query_, runDBTSerializable)
import           Database.Postgres.Temp           (Config (port, postgresConfig),
                                                   ProcessConfig (stdIn, stdOut),
                                                   defaultConfig)
import           System.IO                        (Handle)
import           Test.Hspec                       (SpecWith, shouldBe)
import           Test.Hspec.Core.Spec             (Spec)
import           Test.Hspec.DB                    (TestDB, describeDB,
                                                   describeDBWithConfig, itDB)

defaultSpec :: Spec
defaultSpec =
  describeDB (runDBTSerializable migrate) "with default config" test

customSpec :: Handle -> Handle -> Spec
customSpec i o =
  describeDBWithConfig
    ( defaultConfig
        { postgresConfig =
            (postgresConfig defaultConfig)
              { stdIn = Last $ Just i,
                stdOut = Last $ Just o
              }
        , port = Last $ Just $ Just 8999
        }
    )
    (runDBTSerializable migrate)
    "with custom config"
    test

migrate :: DBT IO ()
migrate = void $ execute_ [sql| CREATE TABLE things (n VARCHAR NOT NULL) |]

test :: SpecWith TestDB
test =
  itDB "basic insert/select" $ do
    execute [sql| INSERT INTO things (n) VALUES (?) |] (Only name)
    name' <- query_ [sql| SELECT n FROM things |]
    liftIO $ name' `shouldBe` [Only name]
  where
    name = "dupont"
