module Lupo.Util
  ( localDay
  , zonedTimeToLocalTime
  , paramId
  , paramNum
  , textParam
  , bsParam
  , textSplice
  , zonedDay
  , toText
  , formatTime
  , safeIndex
  , formatTimeForAtom
  ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Ix as Ix
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import qualified Heist as H
import Prelude hiding (filter)
import Snap
import qualified System.Locale as L
import Text.XmlHtml

import Lupo.Import

localDay :: Simple Lens Time.LocalTime Time.Day
localDay = lens Time.localDay $ \ld d ->
  ld {Time.localDay = d}

zonedTimeToLocalTime :: Simple Lens Time.ZonedTime Time.LocalTime
zonedTimeToLocalTime = lens Time.zonedTimeToLocalTime $ \zd ld ->
  zd {Time.zonedTimeToLocalTime = ld}

paramId :: (MonadSnap m, Integral a) => m a
paramId = paramNum "id"

paramNum :: (MonadSnap m, Integral a) => BS.ByteString -> m a
paramNum name = either (error "invalid param type") id . toIntegral <$> textParam name
  where
    toIntegral = A.parseOnly A.decimal

textParam :: MonadSnap m => BS.ByteString -> m T.Text
textParam name = maybe (error "missing param") TE.decodeUtf8 <$> getParam name

bsParam :: MonadSnap m => BS.ByteString -> m BS.ByteString
bsParam name = fromMaybe (error "missing param") <$> getParam name

textSplice :: (Applicative m, Monad m) => T.Text -> m H.Template
textSplice = pure . pure . TextNode

zonedDay :: Time.ZonedTime -> Time.Day
zonedDay = Time.localDay . Time.zonedTimeToLocalTime

toText :: Show a => a -> T.Text
toText = T.pack . show

formatTime :: Time.FormatTime t => String -> t -> T.Text
formatTime fmt d = T.pack $ Time.formatTime L.defaultTimeLocale fmt d

safeIndex :: [b] -> Int -> Maybe b
safeIndex xs i
  | Ix.inRange (0, length xs - 1) i = Just $ xs !! i
  | otherwise = Nothing

formatTimeForAtom :: Time.ZonedTime -> T.Text
formatTimeForAtom zt = formatTime "%FT%T" zt <> offset
  where
    offset = case Time.timeZoneOffsetString $ Time.zonedTimeZone zt of
      (L.splitAt 3 -> (hour, min')) -> T.pack $ hour <> ":" <> min'
