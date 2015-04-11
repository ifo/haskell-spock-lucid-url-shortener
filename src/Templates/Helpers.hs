module Templates.Helpers
  (renderHtmlStrict
  )
where

import Lucid (renderText, Html)
import Web.Spock.Shared (html, ActionT)
import qualified Data.Text.Lazy as L (toStrict)

renderHtmlStrict :: Html a -> ActionT IO a
renderHtmlStrict = html . L.toStrict . renderText
