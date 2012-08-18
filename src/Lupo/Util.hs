{-# LANGUAGE OverloadedStrings #-}
module Lupo.Util
    ( paramId
    , paramNum
    , param
    , textSplice
    ) where

import Lupo.Application
import qualified Text.Templating.Heist as TH
import qualified Snap.Snaplet.Heist as H
import qualified Data.Attoparsec.Text as A
import Snap
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Prelude hiding (filter)

paramId :: (MonadSnap m, Integral a) => m a
paramId = paramNum "id"

paramNum :: (MonadSnap m, Integral a) => BS.ByteString -> m a
paramNum name = either (error "invalid param type") id . toIntegral <$> param name
  where
    toIntegral = A.parseOnly A.decimal

param :: MonadSnap m => BS.ByteString -> m T.Text
param name = maybe (error "missing param") TE.decodeUtf8 <$> getParam name

textSplice :: T.Text -> H.SnapletSplice Lupo Lupo
textSplice = H.liftHeist . TH.textSplice
