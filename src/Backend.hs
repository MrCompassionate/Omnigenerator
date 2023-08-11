{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

import Servant.HTML.Blaze
import Servant
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import DiagramsTutorial

-- Define the API type. This describes the structure of your web service, 
-- specifying each endpoint, its HTTP method, and the type of response it returns.
type HexAPI = "hexinput" :> Get '[HTML] Html
          :<|> "generate-map" :> QueryParam "size" Int :> Get '[HTML] Html
          :<|> "api" :> "generate-map" :> ReqBody '[JSON] Int :> Post '[JSON] SVGData

newtype SVGData = SVGData String
  deriving (Show, Eq)

apiGenerateMapHandler :: Int -> Int -> Handler SVGData
apiGenerateMapHandler x y = 
    return $ SVGData (generateMap x y)

-- The server implementation for our API.
hexServer :: Server HexAPI
hexServer = hexInputHandler
       :<|> generateMapHandler
       :<|> apiGenerateMapHandler
  where
    -- hexInputHandler serves a simple HTML form. This form, when submitted, 
    -- sends a GET request to the /generate-map endpoint with the specified size.
    hexInputHandler :: Handler H.Html
    hexInputHandler = return $ H.form ! A.action "/generate-map" ! A.method "get" $ do
        "Size: "
        H.input ! type_ "number" ! A.name "size"
        H.input ! type_ "submit" ! A.value "Generate" 

    -- generateMapHandler serves the SVG map. It first checks if a size 
    -- parameter was provided. If so, it uses a hypothetical function generateMap 
    -- (not provided in this example) to produce an SVG for that size. 
    -- If the size isn't provided, it sends a 400 Bad Request error.
generateMapHandler :: Maybe Int -> Handler H.Html
generateMapHandler (Just size') = 
    let svgData = generateMap size'
    in return $ do
        H.div ! A.class_ "svg-container" $ 
            H.preEscapedToHtml (renderSvg svgData)

generateMapHandler Nothing = 
    return $ do
        H.div ! A.class_ "error" $ 
            "Size parameter is required"

-- This is the main function that runs the server. 
-- Here, we're saying to run the server on port 8080 and to use our defined API and server implementation.
main :: IO ()
main = run 8080 (serve (Proxy :: Proxy HexAPI) hexServer)
