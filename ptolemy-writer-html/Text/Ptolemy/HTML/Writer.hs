module Text.Ptolemy.HTML.Writer (writeHtml, writeHtmlStrict) where

import           Data.Monoid ((<>))
import qualified Data.Text as TS
import           Data.Text.Lazy (Text, toStrict)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Text.Ptolemy.Core

-- | Render a Ptolemy @Document@ as HTML represented as lazy @Text@.
writeHtml :: Document -> Text
writeHtml = B.toLazyText . build

-- | Render a Ptolemy @Document@ as HTML represented as strict @Text@.
writeHtmlStrict :: Document -> TS.Text
writeHtmlStrict = toStrict . writeHtml

-- These will be our helper functions for building tags
tag :: Text -> Builder -> Builder
tag t bs = "<" <> build t <> ">" <> bs <> "</" <> build t <> ">"

tagAttrs :: Text -> [(TS.Text, TS.Text)] -> Builder -> Builder
tagAttrs t [] bs = tag t bs
tagAttrs t as bs =
  "<" <> build t <> attrs as <> ">" <> bs <> "</" <> build t <> ">"
  where attrs [] = mempty
        attrs ((k,v):xs) =
          " " <> build k <> "=\"" <> build v <> "\"" <> attrs xs

-- Right now, this just makes the code below a lot smaller: we
-- abstract out the notion of 'building' a thing, so we can
-- more or less indiscriminately apply @build@ to vectors or
-- maps or what-have-you.
class Build t where
  build :: t -> Builder

-- And to that end, we define a handful of utility
-- implementations of things:
instance Build t => Build (Vector t) where
  build = foldMap build

instance Build Text where
  build = B.fromLazyText

instance Build TS.Text where
  build = B.fromText

instance Build Block where
  build (Plain cs) = build cs
  build (Para cs) = tag "p" $ build cs
  build (CodeBlock _ ts) = tag "pre" $ tag "code" $ build ts
  build (RawBlock _ _) = undefined
  build (BlockQuote bs) = tag "blockquote" $ build bs
  build (OrderedList la ds) =
    tagAttrs "ol" (orderedListAttrs la) $ foldMap (tag "li" . build) ds
  build (BulletList ds) =
    tag "ul" $ foldMap (tag "li" . build) ds
  build (DefinitionList ds) =
    tag "dl" $ foldMap build ds
  build (Header n _ is) =
    case n of
      1 -> tag "h1" $ build is
      2 -> tag "h2" $ build is
      3 -> tag "h3" $ build is
      4 -> tag "h4" $ build is
      _ -> undefined -- XXX
  build HorizontalRule = "<hr/>"
  build (Div as ds) = tagAttrs "div" (mkAttrs as) $ build ds
  build Null = mempty

whitesep :: Foldable f => f TS.Text -> TS.Text
whitesep = foldl sep ""
  where sep x y = x <> " " <> y

mkAttrs :: Attr -> [(TS.Text, TS.Text)]
mkAttrs Attr { attrIdentifier = ai
             , attrClasses    = cs
             , attrProps      = _ -- XXX
             } = htmlId <> htmlCs <> htmlPs
  where htmlId | ai /= ""  = [("id", ai)]
               | otherwise = []
        htmlCs | V.null cs = []
               | otherwise = [("class", whitesep cs)]
        htmlPs = []

orderedListAttrs :: ListAttributes -> [(TS.Text, TS.Text)]
orderedListAttrs _ = [("style", "list-style-type: decimal")]

instance Build Definition where
  build Definition
    { dfTerm       = term
    , dfDefinition = defn
    } = tag "dt" (build term) <> tag "dd" (build defn)

instance Build Inline where
  build (Str t) = build t
  build (Emph is) = tag "em" $ build is
  build (Strong is) = tag "strong" $ build is
  build (Strikeout is) = tag "del" $ build is
  build (Superscript is) = tag "span" $ build is -- XXX
  build (Subscript is) = tag "span" $ build is -- XXX
  build (SmallCaps is) = tag "span" $ build is -- XXX
  build Space = " "
  build (Code _ t) = tag "code" $ build t
  build SoftBreak = mempty
  build LineBreak = "<br/>"
