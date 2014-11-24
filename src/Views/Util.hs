module Views.Util
    ( blaze
    , pet
    )
  where

import Data.Text (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Internal (Markup, preEscapedText)
import Web.Scotty (ActionM, html)

pet :: Text -> Markup
pet = preEscapedText

blaze :: Html -> ActionM ()
blaze = html . renderHtml
