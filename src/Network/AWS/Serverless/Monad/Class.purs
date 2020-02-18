module Network.AWS.Serverless.DynamoDB.Monad.Class
    ( class MonadDynamoDBDelete
    , class MonadDynamoDBGet
    , class MonadDynamoDBPut
    , class MonadDynamoDBQuery
    , class MonadDynamoDBScan
    , class MonadDynamoDBUpdate
    , class MonadDynamoDBReadOnlyAccess
    , class MonadDynamoDBFullAccess
    , deleteItem
    , getItem
    , putItem
    , queryItems
    , scanItems
    , updateItem
    ) where

import Data.Unit                       (Unit)
import Network.AWS.Serverless.DynamoDB (DeleteParams, GetParams, GetResponse,
                                        PutParams, QueryParams, QueryResponse,
                                        ScanParams, ScanResponse, UpdateParam)


-- |The `MonadDynamoDBDelete` type class represents those monads support a `deleteItem` operation
-- |on DynamoDB.
class MonadDynamoDBDelete m where
    deleteItem :: forall k r. DeleteParams k r -> m Unit

-- |The `MonadDynamoDBGet` type class represents those monads support a `getItem` operation
-- |on DynamoDB.
class MonadDynamoDBGet a m | m -> a where
    getItem :: forall k r. GetParams k r -> m (GetResponse a)

-- |The `MonadDynamoDBPut` type class represents those monads support a `putItem` operation
-- |on DynamoDB.
class MonadDynamoDBPut a m | m -> a where
    putItem :: forall r. PutParams a r -> m Unit

-- |The `MonadDynamoDBQuery` type class represents those monads support a `queryItems` operation
-- |on DynamoDB.
class MonadDynamoDBQuery a m | m -> a where
    queryItems :: forall r. QueryParams r -> m (QueryResponse a)

-- |The `MonadDynamoDBScan` type class represents those monads support a `scanItems` operation
-- |on DynamoDB.
class MonadDynamoDBScan a m | m -> a where
    scanItems :: forall r. ScanParams r -> m (ScanResponse a)

-- |The `MonadDynamoDBUpdate` type class represents those monads support a `updateItem` operation
-- |on DynamoDB.
class MonadDynamoDBUpdate m where
    updateItem :: forall k r. UpdateParam k r -> m Unit

-- |The `MonadDynamoDBReadOnlyAccess` type class represents those monads support read-only operations
-- |on DynamoDB.
-- |
-- | - `MonadDynamoDBGet`
-- | - `MonadDynamoDBQuery`
-- | - `MonadDynamoDBScan`
class ( MonadDynamoDBGet a m
      , MonadDynamoDBQuery a m
      , MonadDynamoDBScan a m
      ) <= MonadDynamoDBReadOnlyAccess a m | m -> a

-- |The `MonadDynamoDBFullAccess` type class represents those monads support all operations
-- |on DynamoDB.
-- |
-- | - `MonadDynamoDBGet`
-- | - `MonadDynamoDBQuery`
-- | - `MonadDynamoDBScan`
-- | - `MonadDynamoDBDelete`
-- | - `MonadDynamoDBPut`
-- | - `MonadDynamoDBUpdate`
class ( MonadDynamoDBDelete m
      , MonadDynamoDBPut a m
      , MonadDynamoDBUpdate m
      , MonadDynamoDBReadOnlyAccess a m
      ) <= MonadDynamoDBFullAccess a m | m -> a
