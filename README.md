# ptolemy

**WARNING: STILL SUPER EARLY IN DEVELOPMENT, AND MAY NEVER GO ANYWHERE
AT ALL.**

The motivation behind Ptolemy is to address some minor gripes about
Pandoc as a library. Let's be clear: Pandoc is _awesome_. It's amazing
both as a utility and as a library, and a ridiculous amount of work
has gone into making it as cool as it is.

One of the major strengths of Pandoc is that it's frankly huge. It's
got all kinds of amazing features and implements a ridiculous amount
of stuff. The downside to that is that when you want to use Pandoc as
a library, you often only want a tiny subset of its functionality.
Maybe you want to parse Markdown and RST, and produce HTML. Maybe
you want to pull in HTML but also spit out LaTeX. With Pandoc, you
can't go in a little bit: you have to get it _all_.

This library is an experiment in building a heavily Pandoc-inspired
system with a more modular structure. With Ptolemy, you can select
just the readers and writers you want and compose them together
without having to draw in the the hundred-odd modules that compose
Pandoc proper. It also should simplify plugging in new modules to
a Pandoc-like ecosystem: I could, for example, create a new markup
format and easily translate it to the Ptolemy internal format and
then use existing Ptolemy writers with it.

_That said_, it might turn out that this is no net win at all. We'll
see!

## What Does It Look Like?

This doesn't actually work yet, but it will look something like
this. This program will depend on `ptolemy-reader-markdown`,
`ptolemy-writer-html` and `text`:

~~~.haskell
import qualified Data.Text as Text
import qualified Text.Ptolemy.Markdown.Writer as MS
import qualified Text.Ptolemy.HTML.Writer as HTML

main :: IO
main = do
  cs <- Text.getContents
  case MD.readSlackdown MD.defaultOpts cs of
    Left err -> putStrLn err
    Right pt -> Text.putStrLn (HTML.writeHtmlStrict pt)
~~~

## Why The Name?

The name _Ptolemy_ is a Greek name that was given to a number of famous
Greek and Egyptian historical figures, but I chose it for this project
for two reasons:

- Like "Pandoc", "Ptolemy" starts with a P, and
- The Rosetta Stone, the multilingual decree that survived and served
  much later as Europe's key to deciphering Egyptian hieroglyphs, was
  made on the orders of King Ptolemy V.
