module Network.AWS.Serverless.DynamoDB.Monad
    ( DynamoDB
    , runDynamoDB
    ) where

import Control.Alt                                 (class Alt)
import Control.Monad.Reader.Class                  (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans                  (ReaderT (..), runReaderT)
import Control.Monad.Error.Class                   (class MonadThrow, class MonadError)
import Control.Monad.Rec.Class                     (class MonadRec)
import Control.Plus                                (class Plus)
import Data.Newtype                                (class Newtype, unwrap)
import Effect.Aff                                  (Aff, Error)
import Effect.Class                                (class MonadEffect)
import Foreign.Generic.Class                       (class Decode)
import Network.AWS.Serverless.DynamoDB as D
import Network.AWS.Serverless.DynamoDB.Monad.Class
import Prelude                                     (class Applicative, class Apply,
                                                    class Bind, class Functor, class Monad,
                                                    class Monoid, class Semigroup,
                                                    flip, (<<<))


-- |`DynamoDB` provides monadic actions to operate DynamoDB.
-- |The `Aff` monad underlies the `DynamoDB` monad and each operations are asynchronous.
newtype DynamoDB r a = DynamoDB (ReaderT (D.DocumentClient r) Aff a)

runDynamoDB :: forall r a. DynamoDB r a -> D.DocumentClient r -> Aff a
runDynamoDB = runReaderT <<< unwrap

derive instance         newtypeDynamoDB     :: Newtype (DynamoDB r a) _
derive newtype instance functorDynamoDB     :: Functor (DynamoDB a)
derive newtype instance applyDynamoDB       :: Apply (DynamoDB a)
derive newtype instance applicativeDynamoDB :: Applicative (DynamoDB a)
derive newtype instance altDynamoDB         :: Alt (DynamoDB a)
derive newtype instance plusDynamoDB        :: Plus (DynamoDB a)
derive newtype instance bindDynamoDB        :: Bind (DynamoDB a)
derive newtype instance monadDynamoDB       :: Monad (DynamoDB a)
derive newtype instance semigroupDynamoDB   :: Semigroup a => Semigroup (DynamoDB r a)
derive newtype instance monoidDynamoDB      :: Monoid a => Monoid (DynamoDB r a)
derive newtype instance monadEffectDynamoDB :: MonadEffect (DynamoDB a)
derive newtype instance monadThrowDynamoDB  :: MonadThrow Error (DynamoDB a)
derive newtype instance monadErrorDynamoDB  :: MonadError Error (DynamoDB a)
derive newtype instance monadAskDynamoDB    :: MonadAsk (D.DocumentClient a) (DynamoDB a)
derive newtype instance monadReaderDynamoDB :: MonadReader (D.DocumentClient a) (DynamoDB a)
derive newtype instance monadRecDynamoDB    :: MonadRec (DynamoDB a)

instance dynamoDBDelete :: MonadDynamoDBDelete (DynamoDB a) where
    deleteItem = DynamoDB <<< ReaderT <<< flip D.deleteItem

instance dynamoDBGet :: Decode a => MonadDynamoDBGet a (DynamoDB a) where
    getItem = DynamoDB <<< ReaderT <<< flip D.getItem

instance dynamoDBPut :: MonadDynamoDBPut a (DynamoDB a) where
    putItem = DynamoDB <<< ReaderT <<< flip D.putItem

instance dynamoDBQuery :: Decode a => MonadDynamoDBQuery a (DynamoDB a) where
    queryItems = DynamoDB <<< ReaderT <<< flip D.queryItems

instance dynamoDBScan :: Decode a => MonadDynamoDBScan a (DynamoDB a) where
    scanItems = DynamoDB <<< ReaderT <<< flip D.scanItems

instance dynamoDBUpdate :: MonadDynamoDBUpdate (DynamoDB a) where
    updateItem = DynamoDB <<< ReaderT <<< flip D.updateItem

instance dynamoDBReadOnlyAccess :: Decode a => MonadDynamoDBReadOnlyAccess a (DynamoDB a)
instance dynamoDBFullAccess :: Decode a => MonadDynamoDBFullAccess a (DynamoDB a)
