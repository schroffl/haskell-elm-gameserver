{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.WebSockets as WS

main :: IO ()
main = WS.runServer "0.0.0.0" 9160 app

app :: WS.ServerApp
app pending = WS.rejectRequest pending "Nope!"
