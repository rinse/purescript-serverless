module Network.AWS.Serverless.DynamoDB
    ( documentClient
    , getItem
    , deleteItem
    , putItem
    , queryItems
    , scanItems
    , updateItem
    , DocumentClient
    , GetParams
    , DeleteParams
    , PutParams
    , QueryParams
    , ScanParams
    , UpdateParam
    , ConsumedCapacity
    , GetResponse
    , QueryResponse
    , ScanResponse
    ) where


import Control.Monad.Except
import Control.Monad.Error.Class
import Control.Promise
import Data.Foldable
import Data.List.Types
import Data.Maybe
import Data.Profunctor.Choice (left, (|||))
import Effect.Aff
import Effect.Uncurried
import Foreign
import Foreign.Generic.Class
import Prelude
import Type.Proxy (Proxy)

foreign import data DocumentClient :: Type -> Type

foreign import _documentClient :: forall r a. { | r } -> DocumentClient a
foreign import _getItem :: forall k r a. EffectFn2 (DocumentClient a) (GetParams k r) (Promise (GetResponse Foreign))
foreign import _deleteItem :: forall k r a. EffectFn2 (DocumentClient a) (DeleteParams k r) (Promise Unit)
foreign import _putItem :: forall r a. EffectFn2 (DocumentClient a) (PutParams a r) (Promise Unit)
foreign import _queryItems :: forall r a. EffectFn2 (DocumentClient a) (QueryParams r) (Promise (QueryResponse Foreign))
foreign import _scanItems :: forall r a. EffectFn2 (DocumentClient a) (ScanParams r) (Promise (ScanResponse Foreign))
foreign import _updateItem :: forall k r a. EffectFn2 (DocumentClient a) (UpdateParam k r) (Promise Unit)


fromJSPromise2 :: forall a b c. EffectFn2 a b (Promise c) -> a -> b -> Aff c
fromJSPromise2 = (map <<< map) toAffE <<< runEffectFn2

documentClient :: forall r a. Proxy a -> { | r } -> DocumentClient a
documentClient = const _documentClient

type GetParams k r = { "TableName" :: String, "Key" :: k | r }
type DeleteParams i r = { "TableName" :: String, "Key" :: i | r }
type PutParams i r = { "TableName" :: String, "Item" :: i | r }
type QueryParams r = { "TableName" :: String | r }
type ScanParams r = { "TableName" :: String | r }
type UpdateParam k r = { "TableName" :: String, "Key" :: k | r }

type ConsumedCapacity =
    { "TableName" :: String
    , "CapacityUnits" :: Number
    , "ReadCapacityUnits" :: Number
    , "WriteCapacityUnits" :: Number
    , "Table" ::
        { "ReadCapacityUnits" :: Number
        , "WriteCapacityUnits" :: Number
        , "CapacityUnits" :: Number
        }
    , "LocalSecondaryIndexes" ::
        { "ReadCapacityUnits" :: Number
        , "WriteCapacityUnits" :: Number
        , "CapacityUnits" :: Number
        }
    , "GlobalSecondaryIndexes" ::
        { "ReadCapacityUnits" :: Number
        , "WriteCapacityUnits" :: Number
        , "CapacityUnits" :: Number
        }
    }

type GetResponse i =
    { "Item" :: i
    , "ConsumedCapacity" :: ConsumedCapacity
    }

type QueryResponse i = ScanResponse i

type ScanResponse i =
    { "Items" :: i
    , "Count" :: Int
    , "ScannedCount" :: Int
    -- , "LastEvaluatedKey" :: ?
    , "ConsumedCapacity" :: ConsumedCapacity
    }

-- |Gets an item of DynamoDB.
getItem :: forall k r a. Decode a
        => DocumentClient a
        -> GetParams k r
        -> Aff (GetResponse (Maybe a))
getItem client params = do
    response <- getItem' client params
    item <- readItem response."Item"
    pure $ response { "Item" = item }
    where
    getItem' = fromJSPromise2 _getItem

-- |'Nothing' for null and undefined.
-- |throws an exception for mimatched types.
readItem :: forall m a. MonadThrow Error m => Decode a => Foreign -> m (Maybe a)
readItem = (throwError ||| pure)
    <<< left (error <<< renderMultipleErrors)
    <<< runExcept
    <<< (decode :: Foreign -> F (Maybe a))

renderMultipleErrors :: MultipleErrors -> String
renderMultipleErrors = renderForeignErrors <<< toList
    where
    renderForeignErrors = foldr (\a b -> renderForeignError a <> b) mempty

-- |Deletes an item of DynamoDB.
deleteItem :: forall k r a. DocumentClient a -> DeleteParams k r -> Aff Unit
deleteItem = fromJSPromise2 _deleteItem

-- |Puts an item on DynamoDB.
putItem :: forall r a. DocumentClient a -> PutParams a r -> Aff Unit
putItem = fromJSPromise2 _putItem

-- |Scans items of DynamoDB
queryItems :: forall r a. Decode a => DocumentClient a -> QueryParams r -> Aff (QueryResponse (Array a))
queryItems client params = do
    response <- queryItems' client params
    items <- readItems response."Items"
    pure $ response { "Items" = items }
    where
    queryItems' = fromJSPromise2 _queryItems

-- |Queries items of DynamoDB
scanItems :: forall r a. Decode a => DocumentClient a -> ScanParams r -> Aff (ScanResponse (Array a))
scanItems client params = do
    response <- scanItems' client params
    items <- readItems response."Items"
    pure $ response { "Items" = items }
    where
    scanItems' = fromJSPromise2 _scanItems

readItems :: forall m a. MonadThrow Error m => Decode a => Foreign -> m (Array a)
readItems = (throwError ||| pure)
    <<< left (error <<< renderMultipleErrors)
    <<< runExcept
    <<< (decode :: Foreign -> F (Array a))

-- |Updates an item on DynamoDB.
updateItem :: forall k r a. DocumentClient a -> UpdateParam k r -> Aff Unit
updateItem = fromJSPromise2 _updateItem
