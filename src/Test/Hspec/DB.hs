{-|
Helpers for creating database tests with hspec and pg-transact

@hspec-pg-transact@ utilizes @tmp-postgres@ to automatically and connect to a temporary instance of @postgres@ on a random port.

 @
  'describeDB' migrate "Query” $
    itDB "work" $ do
      'execute_' [sql|
        INSERT INTO things
        VALUES (‘me’) |]
      'query_' [sql|
        SELECT name
         FROM things |]
        `shouldReturn` [Only "me"]
 @

In the example above 'describeDB' wraps 'describe' with a 'beforeAll' hook for creating a db and a 'afterAll' hook for stopping a db.
.
Tests can be written with 'itDB' which is wrapper around 'it' that uses the passed in 'TestDB' to run a db transaction automatically for the test.

The libary also provides a few other functions for more fine grained control over running transactions in tests.

-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.DB where
import           Control.Exception
import           Control.Monad
import           Data.Pool
import qualified Database.Postgres.Temp       as Temp
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact
import           Test.Hspec

data TestDB = TestDB
  { tempDB     :: Temp.DB
  -- ^ Handle for temporary @postgres@ process
  , pool :: Pool Connection
  -- ^ Pool of 50 connections to the temporary @postgres@
  }

-- | Start a temporary @postgres@ process and create a pool of connections to it
setupDB :: (Connection -> IO ()) -> IO TestDB
setupDB migrate = do
  tempDB     <- either throwIO return =<< Temp.start
  pool <- createPool
    (connectPostgreSQL $ Temp.toConnectionString tempDB)
    close
    1
    100000000
    50
  withResource pool migrate
  return TestDB {..}

-- | Drop all the connections and shutdown the @postgres@ process
teardownDB :: TestDB -> IO ()
teardownDB TestDB {..} = do
  destroyAllResources pool
  void $ Temp.stop tempDB

-- | Run an 'IO' action with a connection from the pool
withPool :: TestDB -> (Connection -> IO a) -> IO a
withPool testDB = withResource (pool testDB)

-- | Run an 'DB' transaction. Uses 'runDBTSerializable'.
withDB :: DB a -> TestDB -> IO a
withDB action testDB =
  withResource (pool testDB) (runDBTSerializable action)

-- | Flipped version of 'withDB'
runDB :: TestDB -> DB a -> IO a
runDB = flip withDB

-- | Helper for writing tests. Wrapper around 'it' that uses the passed
--   in 'TestDB' to run a db transaction automatically for the test.
itDB :: String -> DB a -> SpecWith TestDB
itDB msg action = it msg $ void . withDB action

-- | Wraps 'describe' with a
--
-- @
--   'beforeAll' ('setupDB' migrate)
-- @
--
-- hook for creating a db and a
--
-- @
--   'afterAll' 'teardownDB'
-- @
--
-- hook for stopping a db.
describeDB :: (Connection -> IO ()) -> String -> SpecWith TestDB -> Spec
describeDB migrate str =
  beforeAll (setupDB migrate) . afterAll teardownDB . describe str
