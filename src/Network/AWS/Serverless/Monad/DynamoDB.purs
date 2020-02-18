module Network.AWS.Serverless.DynamoDB.Monad
    ( getItem
    , deleteItem
    , putItem
    , queryItems
    , scanItems
    , updateItem
    , runDynamoDB
    , DynamoDB
    ) where

import Control.Monad.Reader.Trans
import Data.Maybe
import Effect.Aff
import Foreign.Generic.Class
import Network.AWS.Serverless.DynamoDB as D
import Prelude


-- |A monadic alternatives of DoculemtClient.
type DynamoDB r a = ReaderT (D.DocumentClient r) Aff a

runDynamoDB :: forall r a. DynamoDB r a -> D.DocumentClient r -> Aff a
runDynamoDB = runReaderT

getItem :: forall k r a. Decode a
     => D.GetParams k r
     -> DynamoDB a (D.GetResponse (Maybe a))
getItem = ReaderT <<< flip D.getItem

deleteItem :: forall k r a. D.DeleteParams k r -> DynamoDB a Unit
deleteItem = ReaderT <<< flip D.deleteItem

putItem :: forall r a. D.PutParams a r -> DynamoDB a Unit
putItem = ReaderT <<< flip D.putItem

queryItems :: forall r a. Decode a => D.QueryParams r -> DynamoDB a (D.QueryResponse (Array a))
queryItems = ReaderT <<< flip D.queryItems

scanItems :: forall r a. Decode a => D.ScanParams r -> DynamoDB a (D.ScanResponse a)
scanItems = ReaderT <<< flip D.scanItems

updateItem :: forall k r a. D.UpdateParam k r -> DynamoDB a Unit
updateItem = ReaderT <<< flip D.updateItem
