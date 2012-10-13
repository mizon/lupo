{-# LANGUAGE OverloadedStrings #-}
module Lupo.Test.Syntax
    ( syntaxTest
    ) where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.XmlHtml

import qualified Lupo.Syntax as S

syntaxTest :: Test
syntaxTest = testGroup "diary syntax"
    [ testCase "paragraph" $ S.renderBody "foo" @?= [Element "p" [] [TextNode "foo"]]
    , testCase "blockquote" $ S.renderBody "> foo" @?= [Element "blockquote" [] [TextNode "foo"]]

    , testCase "unordered list" $
            S.renderBody "- foo\n- foo"
        @?= [Element "ul" [] [Element "li" [] [TextNode "foo"], Element "li" [] [TextNode "foo"]]]

    , testCase "heading (prefix)" $ S.renderBody "### fooo" @?= [Element "h3" [] [TextNode "fooo"]]

    , testCase "heading (underline h1)" $
            S.renderBody "foo\n===\n"
        @?= [Element "h1" [] [TextNode "foo"]]

    , testCase "heading (underline h2)" $
            S.renderBody "foo\n---\n"
        @?= [Element "h2" [] [TextNode "foo"]]
    ]
