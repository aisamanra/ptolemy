{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Ptolemy.Pandoc where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Ptolemy.Core
import qualified Text.Pandoc.Definition as P

-- This module contains the entirely mechanical translation between the
-- Pandoc types and their corresponding Ptolemy types.

class Convert pan pto where
  from :: pan -> pto
  to   :: pto -> pan

instance Convert x y => Convert [x] (Vector y) where
  from xs = V.fromList (map from xs)
  to ys   = map to (V.toList ys)

instance Convert String Text where
  from = T.pack
  to   = T.unpack

instance (Convert a b, Convert c d) => Convert (a, c) (b, d) where
  from (a, b) = (from a, from b)
  to   (a, b) = (to a, to b)

instance Convert P.Block Block where
  from (P.Plain is)          = Plain (from is)
  from (P.Para is)           = Para (from is)
  from (P.CodeBlock as str)  = CodeBlock (from as) (from str)
  from (P.RawBlock fmt str)  = RawBlock (from fmt) (from str)
  from (P.BlockQuote bs)     = BlockQuote (from bs)
  from (P.OrderedList la bs) = OrderedList (from la) (from bs)
  from (P.BulletList bs)     = BulletList (from bs)
  from (P.DefinitionList ds) = DefinitionList (from ds)
  from (P.Header n attr is)  = Header n (from attr) (from is)
  from (P.HorizontalRule)    = HorizontalRule
  from (P.Div attr bs)       = Div (from attr) (from bs)
  from (P.Null)              = Null
  to (Plain is)          = P.Plain (to is)
  to (Para is)           = P.Para (to is)
  to (CodeBlock as str)  = P.CodeBlock (to as) (to str)
  to (RawBlock fmt str)  = P.RawBlock (to fmt) (to str)
  to (BlockQuote bs)     = P.BlockQuote (to bs)
  to (OrderedList la bs) = P.OrderedList (to la) (to bs)
  to (BulletList bs)     = P.BulletList (to bs)
  to (DefinitionList ds) = P.DefinitionList (to ds)
  to (Header n attr is)  = P.Header n (to attr) (to is)
  to (HorizontalRule)    = P.HorizontalRule
  to (Div attr bs)       = P.Div (to attr) (to bs)
  to (Null)              = P.Null

instance Convert P.Inline Inline where
  from (P.Str str)           = Str (from str)
  from (P.Emph is)           = Emph (from is)
  from (P.Strong is)         = Strong (from is)
  from (P.Strikeout is)      = Strikeout (from is)
  from (P.Superscript is)    = Superscript (from is)
  from (P.Subscript is)      = Subscript (from is)
  from (P.SmallCaps is)      = SmallCaps (from is)
  from (P.Quoted qt is)      = Quoted (from qt) (from is)
  from (P.Cite bs is)        = Cite (from bs) (from is)
  from (P.Code attr str)     = Code (from attr) (from str)
  from (P.Space)             = Space
  from (P.SoftBreak)         = SoftBreak
  from (P.LineBreak)         = LineBreak
  from (P.Math mt str)       = Math (from mt) (from str)
  from (P.RawInline fmt str) = RawInline (from fmt) (from str)
  from (P.Link attr is tgt)  = Link (from attr) (from is) (from tgt)
  from (P.Image attr is tgt) = Image (from attr) (from is) (from tgt)
  from (P.Note bs)           = Note (from bs)
  from (P.Span attr is)      = Span (from attr) (from is)
  to (Str str)           = P.Str (to str)
  to (Emph is)           = P.Emph (to is)
  to (Strong is)         = P.Strong (to is)
  to (Strikeout is)      = P.Strikeout (to is)
  to (Superscript is)    = P.Superscript (to is)
  to (Subscript is)      = P.Subscript (to is)
  to (SmallCaps is)      = P.SmallCaps (to is)
  to (Quoted qt is)      = P.Quoted (to qt) (to is)
  to (Cite bs is)        = P.Cite (to bs) (to is)
  to (Code attr str)     = P.Code (to attr) (to str)
  to (Space)             = P.Space
  to (SoftBreak)         = P.SoftBreak
  to (LineBreak)         = P.LineBreak
  to (Math mt str)       = P.Math (to mt) (to str)
  to (RawInline fmt str) = P.RawInline (to fmt) (to str)
  to (Link attr is tgt)  = P.Link (to attr) (to is) (to tgt)
  to (Image attr is tgt) = P.Image (to attr) (to is) (to tgt)
  to (Note bs)           = P.Note (to bs)
  to (Span attr is)      = P.Span (to attr) (to is)

instance Convert P.Citation Citation where
  from cite = Citation
    { ciId      = from (P.citationId cite)
    , ciPrefix  = from (P.citationPrefix cite)
    , ciSuffix  = from (P.citationSuffix cite)
    , ciMode    = from (P.citationMode cite)
    , ciNoteNum = P.citationNoteNum cite
    , ciHash    = P.citationHash cite
    }
  to cite = P.Citation
    { P.citationId      = to (ciId cite)
    , P.citationPrefix  = to (ciPrefix cite)
    , P.citationSuffix  = to (ciSuffix cite)
    , P.citationMode    = to (ciMode cite)
    , P.citationNoteNum = ciNoteNum cite
    , P.citationHash    = ciHash cite
    }

instance Convert P.CitationMode CitationMode where
  from P.AuthorInText   = AuthorInText
  from P.SuppressAuthor = SuppressAuthor
  from P.NormalCitation = NormalCitation
  to AuthorInText   = P.AuthorInText
  to SuppressAuthor = P.SuppressAuthor
  to NormalCitation = P.NormalCitation

instance Convert P.MathType MathType where
  from P.DisplayMath = DisplayMath
  from P.InlineMath = InlineMath
  to DisplayMath = P.DisplayMath
  to InlineMath = P.InlineMath

instance Convert P.Format Format where
  from (P.Format str) = Format (from str)
  to (Format str) = P.Format (to str)

instance Convert P.Attr Attr where
  from (ident, classes, props) = Attr
    { attrIdentifier = from ident
    , attrClasses    = from classes
    , attrProps      = from props
    }
  to attr = ( to (attrIdentifier attr)
            , to (attrClasses attr)
            , to (attrProps attr)
            )

instance Convert P.ListAttributes ListAttributes where
  from (i, lsy, ldl) = ListAttributes
    { laWhatever    = i
    , laNumberStyle = from lsy
    , laNumberDelim = from ldl
    }
  to lattrs = ( laWhatever lattrs
              , to (laNumberStyle lattrs)
              , to (laNumberDelim lattrs)
              )

instance Convert P.ListNumberStyle ListNumberStyle where
  from P.DefaultStyle = DefaultStyle
  from P.Example = Example
  from P.Decimal = Decimal
  from P.LowerRoman = LowerRoman
  from P.UpperRoman = UpperRoman
  from P.LowerAlpha = LowerAlpha
  from P.UpperAlpha = UpperAlpha
  to DefaultStyle = P.DefaultStyle
  to Example = P.Example
  to Decimal = P.Decimal
  to LowerRoman = P.LowerRoman
  to UpperRoman = P.UpperRoman
  to LowerAlpha = P.LowerAlpha
  to UpperAlpha = P.UpperAlpha

instance Convert P.ListNumberDelim ListNumberDelim where
  from P.DefaultDelim = DefaultDelim
  from P.Period = Period
  from P.OneParen = OneParen
  from P.TwoParens = TwoParens
  to DefaultDelim = P.DefaultDelim
  to Period = P.Period
  to OneParen = P.OneParen
  to TwoParens = P.TwoParens

instance Convert P.QuoteType QuoteType where
  from P.SingleQuote = SingleQuote
  from P.DoubleQuote = DoubleQuote
  to SingleQuote = P.SingleQuote
  to DoubleQuote = P.DoubleQuote

instance Convert P.Target Target where
  from (url, title) = Target { tgtURL = from url, tgtTitle = from title }
  to tgt = (to (tgtURL tgt), to (tgtTitle tgt))
