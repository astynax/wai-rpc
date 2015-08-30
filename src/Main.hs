{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.HTTP.Types (status200, hContentType)
import Network.Wai.Handler.Warp (run)

application :: Application
application _ respond = respond $
  responseLBS status200 [(hContentType, "text/plain")] "Hello World"

main :: IO ()
main = do
  putStrLn "Serving (hit Ctrl+C to stop)..."
  run 8000 application
