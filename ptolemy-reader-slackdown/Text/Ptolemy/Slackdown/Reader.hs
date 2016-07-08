module Text.Ptolemy.Slackdown.Reader
         ( SlackdownOpts(..)
         , defaultOpts
         , inlineOpts
         , readSlackdown
         ) where

import           Control.Applicative (empty)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Text.Ptolemy.Core (PtolemyError, Document)
import qualified Text.Ptolemy.Core as P

data SlackdownOpts = SlackdownOpts
  { tdBlockElems :: Bool
  } deriving (Eq, Show)

defaultOpts :: SlackdownOpts
defaultOpts = SlackdownOpts
  { tdBlockElems = True
  }

inlineOpts :: SlackdownOpts
inlineOpts = SlackdownOpts
  { tdBlockElems = False
  }

readSlackdown :: SlackdownOpts -> Text -> Either PtolemyError Document
readSlackdown opts tx = case runParser (pSlackdown opts) "[]" tx of
  Right x -> Right x
  Left err -> Left (P.PtolemyError (show err))

enables :: Bool -> Parser a -> Parser a
enables True  p = p
enables False _ = empty

pSlackdown :: SlackdownOpts -> Parser Document
pSlackdown SlackdownOpts { tdBlockElems = blockElems } =
    P.vec <$> (many pBlock <* eof)
  where pBlock =
          blockElems `enables` (pCodeBlock <|> pQuote) <|>
            pLine

        pLine = (P.Plain . flip V.snoc P.LineBreak . P.vec) <$>
                  manyTill pInline (char '\n')

        pQuote = (P.BlockQuote . P.vec) <$> some (char '>' *> pLine)

        pCodeBlock = (P.CodeBlock P.emptyAttr . T.concat) <$>
                     (string "```\n" *> manyTill pPlainLine (string "```\n"))

        pPlainLine = T.pack <$> manyTill (noneOf ("\n\r" :: String))
                                         (char '\n')

        pInline = pWhitespace
          <|> pString
          <|> (P.Code P.emptyAttr . T.pack) <$>
                  (char '`' *> many (satisfy (/= '`')) <* char '`')
          <|> P.Strong <$> pSurrounded '*'
          <|> P.Emph <$> pSurrounded '_'
          <|> P.Strikeout <$> pSurrounded '~'

        pWhitespace = (P.Space) <$ some (oneOf (" \t" :: String))

        pString = (P.Str . T.pack) <$> some (noneOf ("*_~` \t\r\n" :: String))

        pSurrounded :: Char -> Parser P.Chunk
        pSurrounded c = try (char c *> rest)
           where rest = P.vec <$> (many (notFollowedBy (char c) *> pInline)
                                   <* char c)
