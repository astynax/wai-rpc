{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8    as BS
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Char                (toUpper)
import           Data.Map.Strict          (Map, fromList, keys, lookup)
import           Network.HTTP.Types       (Status, badRequest400, hContentType,
                                           methodGet, notFound404, status200,
                                           statusCode)
import           Network.Wai              (Application, Middleware, Response,
                                           queryString, rawPathInfo,
                                           rawQueryString, requestMethod,
                                           responseLBS, responseStatus)
import           Network.Wai.Handler.Warp (run)
import           Prelude                  hiding (lookup)

-- Library of functions

type FunctionName        = BS.ByteString
type FunctionDescription = BS.ByteString
type FunctionArg         = BS.ByteString
type FunctionResult      = BS.ByteString
type FunctionSpec        = ( FunctionDescription
                           , (FunctionArg -> FunctionResult) )

library :: Map FunctionName FunctionSpec
library =
  fromList [ ("reverse", ( "returns string with characters in reverset order",
                           BS.reverse ))
           , ("upper",   ( "returns string with each character in upper case",
                           BS.map toUpper ))]

getFunctionSpec :: FunctionName -> Maybe FunctionSpec
getFunctionSpec = (`lookup` library)

listOfFunctions :: [FunctionName]
listOfFunctions = keys library

describe :: FunctionSpec -> FunctionDescription
describe = fst

call :: FunctionSpec -> FunctionArg -> FunctionResult
call = snd

-- Web Application

application :: Application
application req respond
  | requestMethod req /= methodGet =
    respond
    $ responseBadRequest "Only GET method is allowed!"

  | path == "" =
    respond
    $ if query /= ""
      then responseBadRequest "No query parameters needed!"
      else responseOk renderedListOfFunctions

  | otherwise =
    respond
    $ maybe
    (responseNotFound "Unknown function!")
    (\spec -> responseOk
              $ LBS.fromStrict
              $ if query == ""
                then describe spec
                else call spec firstArg)
    $ getFunctionSpec path

  where
    query = rawQueryString req
    path  = BS.tail $ rawPathInfo req

    firstArg = fst $ head $ queryString req

    renderedListOfFunctions =
      LBS.intercalate "\n"
      $ "Available functions:" : map LBS.fromStrict listOfFunctions

-- Main function

main :: IO ()
main = do
  putStrLn "Serving (hit Ctrl+C to stop)..."
  run 8000 $ withLogging application

-- Misc functions

responseOk, responseNotFound, responseBadRequest :: ByteString -> Response
responseOk         = responsePlainText status200
responseNotFound   = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])

-- Logging

withLogging :: Middleware
withLogging app req respond =
  app req $ \response -> do
    putStrLn $ statusOf response ++ ": " ++ query
    respond response
  where
    query = BS.unpack $ BS.concat [rawPathInfo req, rawQueryString req]
    statusOf = show . statusCode . responseStatus
