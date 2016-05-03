module Text.Ptolemy.Core where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

type Document = Vector Block
type DocumentList = Vector Document
type Chunk = Vector Inline

data Block
  = Plain Chunk
  | Para Chunk
  | CodeBlock Attr Text
  | RawBlock Format Text
  | BlockQuote Document
  | OrderedList ListAttributes DocumentList
  | BulletList DocumentList
  | DefinitionList (Vector (Chunk, DocumentList))
  | Header Int Attr Chunk
  | HorizontalRule
--  | Table ???
  | Div Attr Document
  | Null
    deriving (Eq, Show, Read, Ord)

data Inline
  = Str Text
  | Emph Chunk
  | Strong Chunk
  | Strikeout Chunk
  | Superscript Chunk
  | Subscript Chunk
  | SmallCaps Chunk
  | Quoted QuoteType Chunk
  | Cite (Vector Citation) Chunk
  | Code Attr Text
  | Space
  | SoftBreak
  | LineBreak
  | Math MathType Text
  | RawInline Format Text
  | Link Attr Chunk Target
  | Image Attr Chunk Target
  | Note Document
  | Span Attr Chunk
    deriving (Eq, Show, Read, Ord)

data Attr = Attr
  { attrIdentifier :: Text
  , attrClasses :: Vector Text
  , attrProps :: Vector (Text, Text)
  } deriving (Eq, Show, Read, Ord)

data ListAttributes = ListAttributes
  { laWhatever    :: Int -- XXX What is this field for?
  , laNumberStyle :: ListNumberStyle
  , laNumberDelim :: ListNumberDelim
  } deriving (Eq, Show, Read, Ord)

data Citation = Citation
  { ciId      :: Text
  , ciPrefix  :: Chunk
  , ciSuffix  :: Chunk
  , ciMode    :: CitationMode
  , ciNoteNum :: Int
  , ciHash    :: Int
  } deriving (Eq, Show, Read, Ord)

data CitationMode
  = AuthorInText
  | SuppressAuthor
  | NormalCitation
    deriving (Eq, Show, Read, Ord)

data ListNumberStyle
  = DefaultStyle
  | Example
  | Decimal
  | LowerRoman
  | UpperRoman
  | LowerAlpha
  | UpperAlpha
    deriving (Eq, Show, Read, Ord)

data ListNumberDelim
  = DefaultDelim
  | Period
  | OneParen
  | TwoParens
    deriving (Eq, Show, Read, Ord)

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Show, Read, Ord)

data MathType = DisplayMath | InlineMath
  deriving (Eq, Show, Read, Ord)

newtype Format = Format { fromFormat :: Text }
  deriving (Eq, Show, Read, Ord)

data Target = Target
  { tgtURL   :: Text
  , tgtTitle :: Text
  } deriving (Eq, Show, Read, Ord)
