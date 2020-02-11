module Network.AWS.Serverless.Lambda
    ( LambdaHandler
    , lambdaMain1
    , lambdaMain2
    ) where

import Control.Promise   (fromAff, Promise)
import Effect.Aff        (Aff)
import Effect.Aff.Compat (mkEffectFn2)
import Effect.Uncurried  (EffectFn2)
import Prelude           ((<<<), const, flip, map)


-- |A two-argumented function type for handlers of AWS Lambda.
type LambdaHandler a b c = EffectFn2 a b (Promise c)

-- |Processes incoming events using the given function.
lambdaMain1 :: forall a b c. (a -> Aff c) -> LambdaHandler a b c
lambdaMain1 = lambdaMain2 <<< const

-- |Processes incoming events using the given function.
-- |This variety passes context to the function as well.
lambdaMain2 :: forall a b c. (b -> a -> Aff c) -> LambdaHandler a b c
lambdaMain2 = mkEffectFn2 <<< flip <<< (map fromAff <<< _)
