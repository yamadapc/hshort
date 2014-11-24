{-# LANGUAGE OverloadedStrings #-}
module Views.Home
    ( homeView
    )
  where

import Prelude (($))
import Data.Monoid (mappend, mempty)
import Text.Blaze ((!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, body, button, div, docTypeHtml, h1, head,
                         title, link, input, form, meta, p, script)
import Text.Blaze.Html5.Attributes (action, content, charset, httpEquiv, src,
                                    rel, class_, method, id, type_, href,
                                    media, name, placeholder)
import Web.Scotty (ActionM, html)
import Views.Util

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
  pet "<!--[if lt IE 7]>      <html class='no-js lt-ie9 lt-ie8 lt-ie7'> <![endif]-->"
  pet "<!--[if IE 7]>         <html class='no-js lt-ie9 lt-ie8'></html> <![endif]-->"
  pet "<!--[if IE 8]>         <html class='no-js lt-ie9'> <![endif]-->"
  pet "<!--[if gt IE 8]><!--> <html class='no-js'> <!--<![endif]-->"
  head $ do
    title t
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
    meta ! name "description" ! content "Inspire Text"
    meta ! name "viewport" ! content "width=device-width"
    link ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css"
         ! rel  "stylesheet"
         ! media "screen"
    link ! href "/static/style.css" ! rel "stylesheet" ! media "screen"
  body $ do
    div ! class_ "wrapper" $ b
    script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $
        mempty

homeView :: ActionM ()
homeView = html $ renderHtml $ layout "home" $ do
    h1 "hshort"
    p ! class_ "description" $
        "This is a canonical URL shortener written in Haskell." `mappend`
        "Type an URL and be happy!"
    form ! action "/urls" ! method "post" $ do
        input ! placeholder "Your URL goes here"
        button ! type_ "" ! action "submit" ! id "url-button" $ "submit"
