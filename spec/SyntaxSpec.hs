{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SyntaxSpec
  ( syntaxSpec
  ) where

import qualified Data.Attoparsec.Text as A
import Test.Hspec
import Text.Shakespeare.Text
import Text.XmlHtml

import qualified Lupo.Syntax as S

syntaxSpec :: Spec
syntaxSpec = describe "diary syntax parser" $ do
  it "parses bullet list" $
    parseDiary [st|
* foo
* bar
|] `shouldBe` Right
    [ Element "ul" []
      [ Element "li" [] [TextNode "foo"]
      , Element "li" [] [TextNode "bar"]
      ]
    ]

  it "parses prefix style headlines" $
    parseDiary [st|## headline|] `shouldBe`
      Right [Element "h2" [] [TextNode "headline"]]

  it "parses underline style headlines" $
    parseDiary [st|
headline
--------------------------------
|] `shouldBe` Right
    [ Element "h2" [] [TextNode "headline"]
    ]

  it "parses paragraphs" $
    parseDiary [st|
Paragraph A, line 1.
Paragraph A, line 2.

Paragraph B, line 1.
Paragraph B, line 2.
|] `shouldBe` Right
    [ Element "p" [] [TextNode "Paragraph A, line 1.Paragraph A, line 2."]
    , Element "p" [] [TextNode "Paragraph B, line 1.Paragraph B, line 2."]
    ]

  it "parses code blocks" $
    parseDiary [st|
  foo
  bar
|] `shouldBe` Right
    [ Element "pre" []
      [ Element "code" [] [TextNode "foo\nbar\n"]
      ]
    ]

  it "parses inline elements" $
    parseDiary [st|
Click here [HaskellWiki](http://www.haskell.org/haskellwiki/Haskell) .
|] `shouldBe` Right
    [ Element "p" []
      [ TextNode "Click here "
      , Element "a" [("href", "http://www.haskell.org/haskellwiki/Haskell")] [TextNode "HaskellWiki"]
      , TextNode " ."
      ]
    ]
  where
    parseDiary = A.parseOnly S.diaryParser
