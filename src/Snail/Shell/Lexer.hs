{-
  Definitions

  * Lexer: `Text -> [Text]`
  * Token: The leaves in your AST, e.g. TextLiteral, Number, etc

  Here, we implement a structurally aware lexer that supports one token type
  (text literals) for convenience.
-}
module Snail.Shell.Lexer (
    -- * The parsers you should use
    SExpression (..),
    sExpression,
    sExpressions,

    -- * Exported for testing
    nonQuoteCharacter,
    textLiteral,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Snail.Shell.Characters
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | TODO: 'Void' is the error type but we should use an explicit error type
type Parser = Parsec Void Text

{- | Megaparsec's 'skipLineComment' takes a prefix and skips lines that begin
 with that prefix
-}
skipLineComment :: Parser ()
skipLineComment = L.skipLineComment "--"

{- | Megaparsec's 'skipBlockComment' takes prefix and suffix and skips anything
 in between
-}
skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockCommentNested "{-" "-}"

{- | Generate a parser for whitespace in a language with 'skipLineComment' and
 'skipBlockComment'
-}
spaces :: Parser ()
spaces = L.space space1 skipLineComment skipBlockComment

-- | Parse a 'Text' verbatim
symbol :: Text -> Parser Text
symbol = L.symbol spaces

-- | Parse an S-Expression
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

{- | The list of valid token characters, note that we allow invalid tokens at
 this point
-}
validCharacter :: Parser Char
validCharacter =
    oneOf
        ( initialCharacter
            <> specialInitialCharacter
            <> digitCharacter
            <> specialSubsequentCharacter
        )

{-
    A possibly empty tree of s-expressions

    Technically,
    @
    Token (SourcePos {..}, "hello")
    @

    isn't a valid s-expression. This is,

    @
    SExpression [Token (SourcePos {..}, "hello")]
    @

    and this is also valid,

    @
    SExpression []
    @

    The 'Data.Tree.Tree' type in containers is non-empty which isn't exactly what we are looking for
-}
data SExpression
    = Lexeme (SourcePos, Text)
    | TextLiteral (SourcePos, Text)
    | SExpression (Maybe Char) [SExpression]
    deriving (Eq, Show)

{- | Any 'Text' object that starts with an appropriately valid character. This
 could be an variable or function name. For example, `hello` is a valid
 lexeme in the s-expression below.

 @
 (hello)
 @
-}
lexeme :: Parser SExpression
lexeme = do
    sourcePosition <- getSourcePos
    lexeme' <- some validCharacter
    pure $ Lexeme (sourcePosition, Text.pack lexeme')

-- | Match a parser within quotes
quotes :: Parser a -> Parser a
quotes = between (char '\"') (char '\"')

-- | An escaped quote to support nesting `"` inside a 'textLiteral'
escapedQuote :: Parser Text
escapedQuote = string "\\\""

-- | Matches any non-quote character
nonQuoteCharacter :: Parser Text
nonQuoteCharacter = do
    character <- anySingleBut '\"'
    pure $ Text.singleton character

{- | Matches a literal text and supports nested quotes, e.g.

 @
 ("hello\"")
 @
-}
textLiteral :: Parser SExpression
textLiteral = do
    sourcePosition <- getSourcePos
    text <- quotes (some $ escapedQuote <|> nonQuoteCharacter)
    pure $ TextLiteral (sourcePosition, Text.concat text)

{- | Parse one of the possible structures in 'SExpression'. These are parsed
 recursively separated by 'spaces' in 'sExpression'.
-}
leaves :: Parser SExpression
leaves = lexeme <|> textLiteral <|> sExpression

-- | Parse an 'SExpression'
sExpression :: Parser SExpression
sExpression =
    SExpression
        <$> optional (oneOf parenthesisStartingCharacter)
        <*> parens (leaves `sepEndBy` spaces)

-- | Parse a valid snail file
sExpressions :: Parser [SExpression]
sExpressions = (spaces *> sExpression `sepEndBy1` spaces) <* eof
