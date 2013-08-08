{-# LANGUAGE TypeFamilies #-}

module Lupo.Backends.View.Views
  ( singleDayView
  , multiDaysView
  , monthView
  , searchResultView
  , loginView
  , initAccountView
  , adminView
  , entryEditorView
  , entryPreviewView
  , entriesFeed
  ) where

import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Snap
import qualified Snap.Snaplet.Heist as SH
import Text.Shakespeare.Text hiding (toText)
import qualified Heist as H
import qualified Heist.Interpreted as H
import Text.XmlHtml hiding (render)

import Lupo.Application
import qualified Lupo.Backends.View.Internal as I
import Lupo.Config
import qualified Lupo.Entry as E
import Lupo.Import
import qualified Lupo.Locale as L
import qualified Lupo.Navigation as N
import qualified Lupo.Syntax as S
import qualified Lupo.URLMapper as U
import Lupo.Util
import qualified Lupo.View as V

singleDayView :: m ~ H.HeistT LupoHandler LupoHandler => E.Page -> N.Navigation m -> E.Comment -> [T.Text] -> [T.Text] -> V.View LupoHandler
singleDayView page nav c notice errs = makeView renderPublic $ ViewRep (formatTime "%Y-%m-%d" $ page ^. E.pageDay) $ do
  H.callTemplate "day"
    [ ("lupo:day-title", I.dayTitle reqDay)
    , ("lupo:entries", pure entriesTemplate)
    , ("lupo:if-commented", ifCommented)
    , ("lupo:comments-caption", H.textSplice =<< L.localize "Comments")
    , ("lupo:comments", H.mapSplices I.comment $ page ^. E.pageComments)
    , ("lupo:page-navigation", I.singleDayNavigation nav)
    , ("lupo:new-comment-caption", H.textSplice =<< L.localize "New Comment")
    , ("lupo:new-comment-notice", newCommentNotice)
    , ("lupo:new-comment-errors", newCommentErrors)
    , ("lupo:new-comment-url", U.urlSplice $ U.commentPostPath reqDay)
    , ("lupo:comment-name", H.textSplice $ c ^. E.commentName)
    , ("lupo:comment-body", H.textSplice $ c ^. E.commentBody)
    , ("lupo:name-label", H.textSplice =<< L.localize "Name")
    , ("lupo:content-label", H.textSplice =<< L.localize "Content")
    ]
  where
    reqDay = page ^. E.pageDay

    entriesTemplate = concat $ snd $ L.mapAccumL accum 1 $ page ^. E.pageEntries
      where
        accum i e = (succ i, I.anEntry (Just i) e)

    ifCommented
      | page ^. E.numOfComments > 0 = childNodes <$> H.getParamNode
      | otherwise = pure []

    newCommentNotice
      | null notice = pure []
      | otherwise = H.callTemplate "_notice"
        [ ("lupo:notice-class", H.textSplice "notice success")
        , ("lupo:notice-messages", H.mapSplices mkRow notice)
        ]

    newCommentErrors
      | null errs = pure []
      | otherwise = H.callTemplate "_notice"
        [ ("lupo:notice-class", H.textSplice "notice error")
        , ("lupo:notice-messages", H.mapSplices mkRow errs)
        ]

    mkRow txt = do
      txt' <- L.localize txt
      pure $ [Element "li" [] [TextNode txt']]

multiDaysView :: m ~ H.HeistT LupoHandler LupoHandler => N.Navigation m -> [E.Page] -> V.View LupoHandler
multiDaysView nav pages = makeView renderPublic $ ViewRep title $ do
  daysPerPage <- refLupoConfig lcDaysPerPage
  H.callTemplate "multi-days"
    [ ("lupo:page-navigation", I.multiDaysNavigation daysPerPage nav)
    , ("lupo:day-summaries", H.mapSplices I.daySummary pages)
    ]
  where
    title = case pages of
      ds@((view E.pageDay -> d) : _) -> [st|#{formatTime "%Y-%m-%d" d}-#{toText $ length ds}|]
      _ -> ""

monthView :: m ~ H.HeistT LupoHandler LupoHandler => N.Navigation m -> [E.Page] -> V.View LupoHandler
monthView nav pages = makeView renderPublic $ ViewRep (formatTime "%Y-%m" $ nav ^. N.getThisMonth) $ do
  H.callTemplate "multi-days"
    [ ("lupo:page-navigation", I.monthNavigation nav)
    , ("lupo:day-summaries", if null pages then
                               I.emptyMonth
                             else
                               H.mapSplices I.daySummary pages)
    ]

searchResultView :: T.Text -> [E.Saved E.Entry] -> V.View LupoHandler
searchResultView word es = makeView renderPublic $ ViewRep word $ H.callTemplate "search-result"
  [ ("lupo:search-word", H.textSplice word)
  , ("lupo:search-results", H.mapSplices I.searchResult es)
  ]

loginView :: V.View LupoHandler
loginView = makeView renderPlain $ ViewRep "Login" $ H.callTemplate "login"
  [ ("lupo:login-url", U.urlSplice U.loginPath)
  ]

initAccountView :: V.View LupoHandler
initAccountView = makeView renderPlain $ ViewRep "Init Account" $ H.callTemplate "init-account"
  [ ("lupo:init-account-url", U.urlSplice U.initAccountPath)
  ]

adminView :: [E.Page] -> V.View LupoHandler
adminView days = makeView renderAdmin $ ViewRep "Lupo Admin" $ H.callTemplate "admin"
  [ ("lupo:days", H.mapSplices makeDayRow days)
  , ("lupo:new-entry-url", U.urlSplice (U.fullPath "admin/new"))
  ]
  where
    makeDayRow E.Page { E._pageEntries = []
                      } = pure []
    makeDayRow p@E.Page { E._pageEntries = e : es
                        } = do
      headRow <- (dateTh :) <$> makeEntryRow e
      tailRows <- sequence $ makeEntryRow <$> es
      pure $ tr <$> headRow : tailRows
      where
        tr t = Element "tr" [] t

        dateTh = Element "th" [("class", "date"), ("rowspan", toText $ length $ e : es)]
          [ TextNode $ formatTime "%Y-%m-%d" $ p ^. E.pageDay
          ]

        makeEntryRow e' = do
          showPath <- Encoding.decodeUtf8 <$> U.getURL (U.entryPath e')
          editPath <- Encoding.decodeUtf8 <$> U.getURL (U.entryEditPath e')
          deletePath <- Encoding.decodeUtf8 <$> U.getURL (U.entryDeletePath e')
          pure
            [ Element "td" [] [TextNode $ e' ^. E.savedContent . E.entryTitle]
            , Element "td" [("class", "action")]
              [ Element "a" [("href", showPath)] [TextNode "Show"]
              , TextNode " "
              , Element "a" [("href", editPath)] [TextNode "Edit"]
              , TextNode " "
              , Element "a"
                [ ("href", deletePath)
                , ("onclick", "return confirm(\"Are you sure you want to delete?\");")
                ]
                [ TextNode "Delete"
                ]
              ]
            ]

entryEditorView :: E.Saved E.Entry -> T.Text -> Getter U.URLMapper U.Path -> V.View LupoHandler
entryEditorView s editType editPath = makeView renderAdmin $
  ViewRep editorTitle $ H.callTemplate "entry-editor"
    [ ("lupo:editor-title", H.textSplice [st|#{editType}: #{editorTitle}: #{entryTitle}|])
    , ("lupo:edit-path", U.urlSplice editPath)
    , ("lupo:entry-title", H.textSplice $ s ^. E.savedContent . E.entryTitle)
    , ("lupo:entry-body", H.textSplice $ s ^. E.savedContent . E.entryBody)
    ]
  where
    entryTitle = s ^. E.savedContent . E.entryTitle
    editorTitle = formatTime "%Y-%m-%d" $ s ^. E.createdAt

entryPreviewView :: E.Saved E.Entry -> T.Text -> Getter U.URLMapper U.Path -> V.View LupoHandler
entryPreviewView e editType editPath = makeView renderAdmin $
  ViewRep previewTitle $ H.callTemplate "entry-preview"
    [ ( "lupo:preview-title"
      , H.textSplice [st|#{editType}: #{previewTitle}: #{entryTitle}|]
      )
    , ("lupo:preview-body", pure $ I.anEntry Nothing e)
    , ("lupo:edit-path", U.urlSplice editPath)
    , ("lupo:entry-title", H.textSplice $ e ^. E.savedContent . E.entryTitle)
    , ("lupo:entry-body", H.textSplice $ e ^. E.savedContent . E.entryBody)
    ]
  where
    entryTitle = e ^. E.savedContent . E.entryTitle
    previewTitle = formatTime "%Y-%m-%d" $ e ^. E.createdAt

entriesFeed :: [E.Saved E.Entry] -> V.View LupoHandler
entriesFeed entries = V.View $ do
  title <- refLupoConfig lcSiteTitle
  author <- refLupoConfig lcAuthorName
  SH.withSplices
    [ ("lupo:feed-title", textSplice title)
    , ("lupo:last-updated", textSplice $ maybe "" formatTimeForAtom lastUpdated)
    , ("lupo:index-path", U.urlSplice $ U.fullPath "")
    , ("lupo:feed-path", U.urlSplice U.feedPath)
    , ("lupo:author-name", textSplice author)
    , ("lupo:entries", H.mapSplices entryToFeed entries)
    ] $ SH.renderAs "application/atom+xml" "feed"
  where
    lastUpdated = view E.modifiedAt <$> listToMaybe entries

entryToFeed :: E.Saved E.Entry -> H.Splice LupoHandler
entryToFeed e = H.callTemplate "_feed-entry"
  [ ("lupo:title", textSplice $ e ^. E.savedContent . E.entryTitle)
  , ("lupo:link", urlSplice)
  , ("lupo:entry-id", urlSplice)
  , ("lupo:published", textSplice $ formatTimeForAtom $ e ^. E.createdAt)
  , ("lupo:updated", textSplice $ formatTimeForAtom $ e ^. E.modifiedAt)
  , ("lupo:summary", textSplice $ getSummary $ e ^. E.savedContent . E.entryBody)
  ]
  where
    getSummary = summarize . nodesToPlainText . S.renderBody
      where
        summarize t = if T.length t <= 140
                      then t
                      else T.take 140 t <> "..."

        nodesToPlainText = L.foldl' (\l r -> l <> nodeText r) ""

    urlSplice = do
      urls <- U.getURLMapper
      textSplice $ Encoding.decodeUtf8 $ urls ^. U.entryPath e

data ViewRep m = ViewRep
  { viewTitle :: T.Text
  , viewSplice :: H.Splice m
  }

makeView :: (ViewRep m -> m ()) -> ViewRep m -> V.View m
makeView renderer rep = V.View $ renderer rep

renderPublic :: ( h ~ H.HeistT (Handler b b) (Handler b b), SH.HasHeist b, GetLupoConfig h, U.HasURLMapper h) => ViewRep (Handler b b) -> Handler b v ()
renderPublic rep = SH.heistLocal bindSplices $ SH.render "public"
  where
    bindSplices =
        H.bindSplices
        [ ("lupo:page-title", H.textSplice =<< makePageTitle rep)
        , ("lupo:site-title", H.textSplice =<< refLupoConfig lcSiteTitle)
        , ("lupo:style-sheet", U.urlSplice $ U.cssPath "diary.css")
        , ("lupo:footer-body", refLupoConfig lcFooterBody)
        , ("lupo:feed-path", U.urlSplice U.feedPath)
        , ("lupo:feed-icon-path", U.urlSplice $ U.fullPath "images/feed.png")
        ]
      . H.bindSplice "lupo:main-body" (viewSplice rep)

renderPlain :: (h ~ (Handler b b), SH.HasHeist b, U.HasURLMapper (H.HeistT h h)) => ViewRep h -> Handler b v ()
renderPlain rep = SH.heistLocal bindSplices $ SH.render "default"
  where
    bindSplices = H.bindSplices
      [ ("lupo:page-title", H.textSplice $ viewTitle rep)
      , ("lupo:style-sheet", U.urlSplice $ U.cssPath "plain.css")
      , ("apply-content", viewSplice rep)
      ]

renderAdmin :: ( h ~ H.HeistT (Handler b b) (Handler b b), SH.HasHeist b, GetLupoConfig h, U.HasURLMapper h) => ViewRep (Handler b b) -> Handler b v ()
renderAdmin v = SH.heistLocal bindSplices $ SH.render "admin-frame"
  where
    bindSplices =
        H.bindSplices
        [ ("lupo:page-title", H.textSplice =<< makePageTitle v)
        , ("lupo:site-title", H.textSplice =<< refLupoConfig lcSiteTitle)
        , ("lupo:style-sheet", U.urlSplice $ U.cssPath "admin.css")
        , ("lupo:admin-url", U.urlSplice U.adminPath)
        , ("lupo:footer-body", refLupoConfig lcFooterBody)
        ]
      . H.bindSplice "lupo:main-body" (viewSplice v)

makePageTitle :: GetLupoConfig n => ViewRep m -> n T.Text
makePageTitle rep = do
  siteTitle <- refLupoConfig lcSiteTitle
  pure $
    case viewTitle rep of
      "" -> siteTitle
      t -> [st|#{siteTitle} | #{t}|]
