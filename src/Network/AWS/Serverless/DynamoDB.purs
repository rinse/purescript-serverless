module Network.AWS.Serverless.DynamoDB
    ( documentClient
    , getItem
    , putItem
    , DocumentClient
    , GetParams
    , PutParams
    , GetResponse
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
foreign import _putItem :: forall r a. EffectFn2 (DocumentClient a) (PutParams a r) (Promise Unit)


fromJSPromise2 :: forall a b c. EffectFn2 a b (Promise c) -> a -> b -> Aff c
fromJSPromise2 = (map <<< map) toAffE <<< runEffectFn2

documentClient :: forall r a. Proxy a -> { | r } -> DocumentClient a
documentClient = const _documentClient

type GetParams k r = { "TableName" :: String, "Key" :: k | r }
type PutParams i r = { "TableName" :: String, "Item" :: i | r }

type GetResponse i =
    { "Item" :: i
    , "ConsumedCapacity" ::
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

-- |Puts an item on DynamoDB.
putItem :: forall r a. DocumentClient a -> PutParams a r -> Aff Unit
putItem = fromJSPromise2 _putItem
