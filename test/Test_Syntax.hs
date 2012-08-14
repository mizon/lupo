{-# LANGUAGE OverloadedStrings #-}
module Test_Syntax
    ( syntaxTest
    ) where

import qualified Lupo.Syntax as S
import Text.XmlHtml
import Test.HUnit hiding (Test)
import Test.Framework.Providers.HUnit
import Test.Framework

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
