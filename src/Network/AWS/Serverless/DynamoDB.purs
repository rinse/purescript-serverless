module Network.AWS.Serverless.DynamoDB
    ( documentClient
    , getItem
    , putItem
    , DocumentClient
    , GetParams
    , PutParams
    ) where

import Control.Promise
import Effect.Aff
import Effect.Uncurried
import Prelude
import Type.Proxy (Proxy)

foreign import data DocumentClient :: Type -> Type

foreign import _documentClient :: forall r a. { | r } -> DocumentClient a
foreign import _getItem :: forall k r a. EffectFn2 (DocumentClient a) (GetParams k r) (Promise a)
foreign import _putItem :: forall r a. EffectFn2 (DocumentClient a) (PutParams a r) (Promise Unit)


fromJSPromise2 :: forall a b c. EffectFn2 a b (Promise c) -> a -> b -> Aff c
fromJSPromise2 = (map <<< map) toAffE <<< runEffectFn2

documentClient :: forall r a. Proxy a -> { | r } -> DocumentClient a
documentClient = const _documentClient

type GetParams k r = { "TableName" :: String, "Key" :: k | r }
type PutParams i r = { "TableName" :: String, "Item" :: i | r }

getItem :: forall k r a. DocumentClient a -> GetParams k r -> Aff a
getItem = fromJSPromise2 _getItem

putItem :: forall r a. DocumentClient a -> PutParams a r -> Aff Unit
putItem = fromJSPromise2 _putItem
