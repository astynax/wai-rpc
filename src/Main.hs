{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Lazy     (ByteString)
import           Network.HTTP.Types       (Status, hContentType, notFound404,
                                           status200)
import           Network.Wai              (Application, Response, responseLBS)
import           Network.Wai.Handler.Warp (run)

application :: Application
application _ respond = respond $ responseOk "Hello World"

main :: IO ()
main = do
  putStrLn "Serving (hit Ctrl+C to stop)..."
  run 8000 application


responseOk :: ByteString -> Response
responseOk = responsePlainText status200

response404 :: ByteString -> Response
response404 = responsePlainText notFound404

responsePlainText :: Status -> ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])

