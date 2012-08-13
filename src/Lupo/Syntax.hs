{-# LANGUAGE OverloadedStrings
    , ScopedTypeVariables #-}
module Lupo.Syntax
    ( renderBody
    ) where

import qualified Data.Attoparsec.Text as A
import Text.XmlHtml
import qualified Data.Text as T
import Control.Applicative

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
            return $ makeElem (length syms) body

        underlineStyle = A.try $ do
            body <- toEOL
            c <- (chars '=' <|> chars '-') <* A.endOfLine
            return $ if c == '='
                     then makeElem 1 body
                     else makeElem 2 body
          where
            chars symbol = A.char symbol <* some (A.char symbol)

        makeElem (level :: Int) body = Element (T.concat ["h", T.pack $ show level]) [] $ inlineElemnents body

    blockQuote = do
        begin <- bqLine
        follows <- T.concat <$> many (bqLine <|> plainLine)
        return $ Element "blockquote" [] $ inlineElemnents $ T.append begin follows
      where
        bqLine = snd <$> beginWith (A.char '>')

    unorderedList = do
        lis <- some $ snd <$> beginWith (A.satisfy $ A.inClass "*+-")
        return $ Element "ul" [] $ makeElem <$> lis
      where
        makeElem body = Element "li" []  $ escapedText body

    paragraph = do
        ls <- some plainLine
        return $ Element "p" [] $ inlineElemnents $ T.concat ls

    plainLine = do
        (h, t) <- beginWith $ A.satisfy $ not . flip elem specialSymbols
        return $ T.append (T.singleton h) t

    beginWith p = do
        begin <- p
        blanks
        body <- toEOL
        return $ (begin, body)

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
            return $ Element "a" [("href", htmlEscape href)] $ escapedText name

        text = TextNode . htmlEscape . T.pack <$> some (A.satisfy (/= '['))

blanks :: A.Parser ()
blanks = A.skipWhile $ A.inClass " \t"

escapedText :: T.Text -> [Node]
escapedText = pure . TextNode . htmlEscape

htmlEscape :: T.Text -> T.Text
htmlEscape = T.concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape c = T.singleton c
