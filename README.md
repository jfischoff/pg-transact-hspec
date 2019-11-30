# hspec-pg-transact

Helpers for creating database tests with hspec and pg-transact

hspec-pg-transact utilizes tmp-postgres to automatically and connect to a
temporary instance of postgres on a random port.

 ```haskell
describeDB migrate "Query" $
  itDB "work" $ do
    execute_ [sql|
      INSERT INTO things
      VALUES (‘me’) |]
    query_ [sql|
      SELECT name
        FROM things |]
      `shouldReturn` [Only "me"]
 ```

In the example above describeDB wraps describe with a beforeAll hook for
creating a db and a afterAll hook for stopping a db.

Tests can be written with itDB which is wrapper around it that uses the passed
in TestDB to run a db transaction automatically for the test.

The libary also provides a few other functions for more fine grained control
over running transactions in tests.
