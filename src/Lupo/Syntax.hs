{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lupo.Syntax
  ( renderBody
  ) where

import Control.Applicative
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Text.XmlHtml

renderBody :: T.Text -> [Node]
renderBody = either (error "[BUG] in body parsing") id . A.parseOnly diaryParser

diaryParser :: A.Parser [Node]
diaryParser = trimEmptyLines *> many (block <* trimEmptyLines) <* A.endOfInput
  where
    trimEmptyLines = many $ A.try $ blanks *> A.endOfLine

block :: A.Parser Node
block = heading <|> blockQuote <|> unorderedList <|> paragraph
  where
    heading = prefixStyle <|> underlineStyle
      where
        prefixStyle = do
          (syms, body) <- beginWith $ some $ A.char '#'
          pure $ makeElem (length syms) body

        underlineStyle = A.try $ do
          body <- toEOL
          c <- (chars '=' <|> chars '-') <* A.endOfLine
          pure $ if c == '='
                 then makeElem 1 body
                 else makeElem 2 body
          where
            chars symbol = A.char symbol <* some (A.char symbol)

        makeElem (level :: Int) body =
          Element (T.concat ["h", T.pack $ show level]) [] $ inlineElemnents body

    blockQuote = do
      begin <- bqLine
      follows <- T.concat <$> many (bqLine <|> plainLine)
      pure $ Element "blockquote" [] $ inlineElemnents $ T.append begin follows
      where
        bqLine = snd <$> beginWith (A.char '>')

    unorderedList = do
      lis <- some $ snd <$> beginWith (A.satisfy $ A.inClass "*+-")
      pure $ Element "ul" [] $ makeElem <$> lis
      where
        makeElem body = Element "li" []  $ escapedText body

    paragraph = do
      ls <- some plainLine
      pure $ Element "p" [] $ inlineElemnents $ T.concat ls

    plainLine = do
      (h, t) <- beginWith $ A.satisfy $ not . flip elem specialSymbols
      pure $ T.append (T.singleton h) t

    beginWith p = do
      begin <- p
      blanks
      body <- toEOL
      pure $ (begin, body)

    specialSymbols = ['#', '*', '>', ' ', '\n']
    toEOL = A.takeTill A.isEndOfLine <* blanks <* A.option () A.endOfLine

inlineElemnents :: T.Text -> [Node]
inlineElemnents = either undefined id . parse
  where
    parse = A.parseOnly $ many $ anchor <|> text
      where
        anchor = do
          name <- A.char '[' *> A.takeTill (== ']') <* A.char ']'
          href <- A.char '(' *> A.takeTill (== ')') <* A.char ')'
          pure $ Element "a" [("href", escapeHtml href)] $ escapedText name

        text = TextNode . escapeHtml . T.pack <$> some (A.satisfy (/= '['))

blanks :: A.Parser ()
blanks = A.skipWhile $ A.inClass " \t"

escapedText :: T.Text -> [Node]
escapedText = pure . TextNode . escapeHtml

escapeHtml :: T.Text -> T.Text
escapeHtml = T.concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape c = T.singleton c
