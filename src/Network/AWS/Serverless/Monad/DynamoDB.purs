module Network.AWS.Serverless.DynamoDB.Monad
    ( getItem
    , putItem
    , runDynamoDB
    , DynamoDB
    ) where

import Control.Monad.Reader.Trans
import Data.Maybe
import Effect.Aff
import Foreign.Generic.Class
import Network.AWS.Serverless.DynamoDB as D
import Prelude


-- |A monadic alternative of above.
type DynamoDB r a = ReaderT (D.DocumentClient r) Aff a

runDynamoDB :: forall r a. DynamoDB r a -> D.DocumentClient r -> Aff a
runDynamoDB = runReaderT

getItem :: forall k r a. Decode a
     => D.GetParams k r
     -> DynamoDB a (D.GetResponse (Maybe a))
getItem = ReaderT <<< flip D.getItem

putItem :: forall r a. D.PutParams a r -> DynamoDB a Unit
putItem = ReaderT <<< flip D.putItem
