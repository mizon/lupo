module Lupo.View
    ( exports
    ) where

import Text.XmlHtml
import qualified Text.Templating.Heist as H
import qualified Data.Text as T

editForm :: Monad m => T.Text -> H.Splice m
editForm action = return $ Element "form" [("method", "post"), ("action", action)] []
