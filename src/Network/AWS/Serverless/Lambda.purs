module Network.AWS.Serverless.Lambda
    ( LambdaHandler1
    , LambdaHandler2
    , lambdaMain1
    , lambdaMain2
    ) where

import Control.Promise       (fromAff, Promise)
import Effect.Aff            (Aff)
import Effect.Aff.Compat     (mkEffectFn1, mkEffectFn2)
import Effect.Uncurried      (EffectFn1, EffectFn2)
import Foreign.Generic.Class (class Decode)
import Prelude               (Void, const, flip, map, (<<<))


-- |A two-argumented function type for handlers of AWS Lambda.
type LambdaHandler1 a b = EffectFn1 a (Promise b)
type LambdaHandler2 a b c = EffectFn2 a b (Promise c)

-- |Processes incoming events using the given function.
lambdaMain1 :: forall a b. Decode a => Decode b
            => (a -> Aff b)
            -> LambdaHandler1 a b
lambdaMain1 = mkEffectFn1 <<< (fromAff <<< _)

-- |Processes incoming events using the given function.
-- |This variety passes context to the function as well.
lambdaMain2 :: forall a b c. Decode a => Decode b => Decode c
            => (b -> a -> Aff c)
            -> LambdaHandler2 a b c
lambdaMain2 = mkEffectFn2 <<< flip <<< (map fromAff <<< _)
