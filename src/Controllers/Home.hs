{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home
    ( home
    )
  where

import Web.Scotty (ScottyM, get)
import Views.Home (homeView)

home :: ScottyM ()
home = get "/" homeView
