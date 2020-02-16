module Network.AWS.Serverless.Lambda.Event.ApiGatewayEvent
    ( ApiGatewayEvent
    , ApiGatewayResponse
    ) where


type ApiGatewayEvent r =
    { httpMethod :: String
    , path :: String
    , resource :: String
    | r
    }

type ApiGatewayResponse r = { statusCode :: Int | r }
