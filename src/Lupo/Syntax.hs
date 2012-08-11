{-# LANGUAGE OverloadedStrings
    , ScopedTypeVariables #-}
module Lupo.Syntax
    ( diaryParser
    ) where

import qualified Data.Attoparsec.Text as A
import Text.XmlHtml
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.ByteString.Lazy as BSL
import qualified Blaze.ByteString.Builder as BB
import Control.Applicative
import Control.Monad
import Data.String
import Data.List

diaryParser :: A.Parser [Node]
diaryParser = trimEmptyLines *> many (blockElement <* trimEmptyLines) <* A.endOfInput
  where
    trimEmptyLines = many $ A.try $ A.skipWhile (A.inClass " \t") *> A.endOfLine

blockElement :: A.Parser Node
blockElement = heading <|> blockQuote <|> unorderedList <|> paragraph
  where
    heading = prefixStyle <|> underlineStyle
      where
        prefixStyle = do
            (syms, body) <- beginWith $ some $ A.char '#'
            return $ mkElement (length syms) body

        underlineStyle = A.try $ do
            body <- toEOL
            c <- (chars '=' <|> chars '-') <* A.endOfLine
            return $ if c == '='
                     then mkElement 1 body
                     else mkElement 2 body
          where
            chars symbol = A.char symbol <* some (A.char symbol)

        mkElement (level :: Int) body = Element (T.concat ["h", T.pack $ show level]) [] [TextNode body]

    blockQuote = do
        begin <- bqLine
        follows <- T.concat <$> many (bqLine <|> plainLine)
        return $ Element "blockquote" [] [TextNode $ T.append begin follows]
      where
        bqLine = snd <$> beginWith (A.char '>')

    unorderedList = do
        lis <- some $ snd <$> beginWith (A.satisfy $ A.inClass "*+-")
        return $ Element "ul" [] $ makeElem <$> lis
      where
        makeElem body = Element "li" [] [TextNode body]

    paragraph = do
        lines <- some plainLine
        return $ Element "p" [] [TextNode $ T.concat lines]

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
    blanks = void $ many $ A.satisfy $ A.inClass " \t"
