{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.DB where
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8        as BSC
import           Data.Pool
import qualified Database.Postgres.Temp       as Temp
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact
import           Test.Hspec

data TestDB = TestDB
  { tempDB     :: Temp.DB
  , pool :: Pool Connection
  }

setupDB :: (Connection -> IO ()) -> IO TestDB
setupDB migrate = do
  tempDB     <- either throwIO return =<< Temp.startAndLogToTmp []
  putStrLn $ Temp.connectionString tempDB
  pool <- createPool
    (connectPostgreSQL (BSC.pack $ Temp.connectionString tempDB))
    close
    1
    100000000
    50
  withResource pool migrate
  return TestDB {..}

teardownDB :: TestDB -> IO ()
teardownDB TestDB {..} = do
  destroyAllResources pool
  void $ Temp.stop tempDB

withPool :: TestDB -> (Connection -> IO a) -> IO a
withPool testDB = withResource (pool testDB)

withDB :: DB a -> TestDB -> IO a
withDB action testDB =
  withResource (pool testDB) (runDBTSerializable action)

runDB :: TestDB -> DB a -> IO a
runDB = flip withDB

itDB :: String -> DB a -> SpecWith TestDB
itDB msg action = it msg $ void . withDB action

describeDB :: (Connection -> IO ()) -> String -> SpecWith TestDB -> Spec
describeDB migrate str =
  beforeAll (setupDB migrate) . afterAll teardownDB . describe str
