{-# LANGUAGE OverloadedStrings #-}
module Lupo.Util
    ( paramId
    , paramNum
    , param
    , textSplice
    , zonedDay
    , toText
    ) where

import qualified Text.Templating.Heist as TH
import qualified Snap.Snaplet.Heist as H
import qualified Data.Attoparsec.Text as A
import Snap
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Time as Ti
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

textSplice :: T.Text -> H.SnapletSplice a b
textSplice = H.liftHeist . TH.textSplice

zonedDay :: Ti.ZonedTime -> Ti.Day
zonedDay = Ti.localDay . Ti.zonedTimeToLocalTime

toText :: Show a => a -> T.Text
toText = T.pack . show
