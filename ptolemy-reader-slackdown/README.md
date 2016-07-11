# `ptolemy-slackdown-reader`

This module implements a `ptolemy` reader for the limited
Markdown-like language understood by Slack and other chat
services. This understands only a few basic inline markup
constructs:

- `*asterisks*` for *bold text*
- `_underscores_` for _emphasized text_
- `~tildes~` for ~strikethrough text~
- `` `backticks` `` for `inline code`

It also understands two block-level constructs, but these
can be turned off using the `SlackdownOpts` value passed to
the parser:

- A set of lines started with `>` become a blockquote
- A set of lines with triple backticks above and below become
  verbatim code blocks

Among other things, this is a good idea for situations where
the full generality of Markdown is probably unnecessary. You
don't want your blog comments or chat messages to include
H2 headers or horizontal rules, but some bold and italic text
would be fine!

Right now this is largely untested, and it badly needs QuickCheck
or fuzzing, because it should _absolutely_ be the case that every
possible string parses as _something_, even if that something
just doesn't include markup. This is certainly not the case
right now.
