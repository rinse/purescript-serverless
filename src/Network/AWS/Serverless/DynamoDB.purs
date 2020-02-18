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
    , GetResponse
    , QueryResponse
    , ScanResponse
    ) where

import Control.Monad.Except      (runExcept)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Promise           (Promise, toAffE)
import Data.Foldable             (fold)
import Data.List.Types           (toList)
import Data.Maybe                (Maybe)
import Data.Profunctor.Choice    (left, (|||))
import Effect.Aff                (Aff, Error, error, throwError)
import Effect.Uncurried          (EffectFn2, runEffectFn2)
import Foreign                   (Foreign, MultipleErrors, renderForeignError)
import Foreign.Generic.Class     (class Decode, decode)
import Prelude                   (Unit, const, map, pure, (<<<), (>=>))
import Type.Proxy                (Proxy)

foreign import data DocumentClient :: Type -> Type
foreign import _documentClient :: forall r a. { | r } -> DocumentClient a
foreign import _getItem :: forall k r a. EffectFn2 (DocumentClient a) (GetParams k r) (Promise Foreign)
foreign import _deleteItem :: forall k r a. EffectFn2 (DocumentClient a) (DeleteParams k r) (Promise Unit)
foreign import _putItem :: forall r a. EffectFn2 (DocumentClient a) (PutParams a r) (Promise Unit)
foreign import _queryItems :: forall r a. EffectFn2 (DocumentClient a) (QueryParams r) (Promise Foreign)
foreign import _scanItems :: forall r a. EffectFn2 (DocumentClient a) (ScanParams r) (Promise Foreign)
foreign import _updateItem :: forall k r a. EffectFn2 (DocumentClient a) (UpdateParam k r) (Promise Unit)


fromJSPromise2 :: forall a b c. EffectFn2 a b (Promise c) -> a -> b -> Aff c
fromJSPromise2 = (map <<< map) toAffE <<< runEffectFn2

documentClient :: forall r a. Proxy a -> { | r } -> DocumentClient a
documentClient = const _documentClient

-- |Extensible parameters for `getItem`.
type GetParams k r = { "TableName" :: String, "Key" :: k | r }
-- |Extensible parameters for `deleteItem`.
type DeleteParams i r = { "TableName" :: String, "Key" :: i | r }
-- |Extensible parameters for `putItem`.
type PutParams i r = { "TableName" :: String, "Item" :: i | r }
-- |Extensible parameters for `queryItems`.
type QueryParams r = { "TableName" :: String | r }
-- |Extensible parameters for `scanItems`.
type ScanParams r = { "TableName" :: String | r }
-- |Extensible parameters for `updateItem`.
type UpdateParam k r = { "TableName" :: String, "Key" :: k | r }

-- |Represents response data from `getItem`
type GetResponse i = { "Item" :: Maybe i }

-- |Represents response data from `queryItems`
type QueryResponse i =
    { "Items" :: Array i
    , "Count" :: Int
    , "ScannedCount" :: Int
    }

-- |Represents response data from `scanItems`
type ScanResponse i =
    { "Items" :: Array i
    , "Count" :: Int
    , "ScannedCount" :: Int
    }

-- |Gets an item of DynamoDB.
getItem :: forall k r a. Decode a => DocumentClient a -> GetParams k r -> Aff (GetResponse a)
getItem client = getItem' client >=> decode'
    where
    getItem' = fromJSPromise2 _getItem

-- |Deletes an item of DynamoDB.
deleteItem :: forall k r a. DocumentClient a -> DeleteParams k r -> Aff Unit
deleteItem = fromJSPromise2 _deleteItem

-- |Puts an item on DynamoDB.
putItem :: forall r a. DocumentClient a -> PutParams a r -> Aff Unit
putItem = fromJSPromise2 _putItem

-- |Scans items of DynamoDB
queryItems :: forall r a. Decode a => DocumentClient a -> QueryParams r -> Aff (QueryResponse a)
queryItems client = queryItems' client >=> decode'
    where
    queryItems' = fromJSPromise2 _queryItems

-- |Queries items of DynamoDB
scanItems :: forall r a. Decode a => DocumentClient a -> ScanParams r -> Aff (ScanResponse a)
scanItems client = scanItems' client >=> decode'
    where
    scanItems' = fromJSPromise2 _scanItems

decode' :: forall m a. MonadThrow Error m => Decode a => Foreign -> m a
decode' = (throwError ||| pure)
    <<< left (error <<< renderMultipleErrors)
    <<< runExcept
    <<< decode

renderMultipleErrors :: MultipleErrors -> String
renderMultipleErrors = fold <<< map renderForeignError <<< toList

-- |Updates an item on DynamoDB.
updateItem :: forall k r a. DocumentClient a -> UpdateParam k r -> Aff Unit
updateItem = fromJSPromise2 _updateItem
