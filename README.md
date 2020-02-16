# purescript-serverless

Deploying PureScript code onto AWS Lambda using Serverless.

## Sample

Here is an example of your first step on AWS Lambda with PureScirpt.

```purescript
module Main (handler) where

import Prelude                       (pure)
import Effect.Aff                    (Aff)
import Network.AWS.Serverless.Lambda (LambdaHandler1, lambdaMain1)

type Event = {}
type Response = { body :: String }

handler :: LambdaHandler1 Event Response
handler = lambdaMain1 func

func :: Event -> Aff Response
func event = pure { body: "Hello, spagetti!" }
```

`handler` is the handler which runs when a function is triggered.
`Event` and `Response` are your data types.
When you want to trigger this lambda function, you'll have a `serverless.yml` like the following.

```yaml
service: purescript-serverless-sample

frameworkVersion: ">=1.1.0 <2.0.0"

provider:
  name: aws
  runtime: nodejs10.x

functions:
  func:
    handler: output/Main/index.handler
    events:
      - http:
          path: /
          method: any
```
