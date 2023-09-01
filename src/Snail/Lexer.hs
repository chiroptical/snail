{-
  Definitions

  * Lexer: `Text -> [Text]`
  * Token: The leaves in your AST, e.g. TextLiteral, Number, etc

  Here, we implement a structurally aware lexer that supports one token type
  (text literals) for convenience.
-}
{-# LANGUAGE TupleSections #-}

module Snail.Lexer (
    -- * The parsers you should use
    SnailAst (..),
    Bracket (..),
    sExpression,
    snailAst,

    -- * Exported for testing
    nonQuoteCharacter,
    textLiteral,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Snail.Characters
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

-- | The bracket used to sorround the s-expression
data Bracket
    = Round
    | Square
    | Curly
    deriving (Show, Eq)

roundP :: Parser a -> Parser (Bracket, a)
roundP = fmap (Round,) . between (symbol "(") (symbol ")")

square :: Parser a -> Parser (Bracket, a)
square = fmap (Square,) . between (symbol "[") (symbol "]")

curly :: Parser a -> Parser (Bracket, a)
curly = fmap (Curly,) . between (symbol "{") (symbol "}")

-- | Parse an S-Expression bracketed by 'Bracket'
bracket :: Parser a -> Parser (Bracket, a)
bracket inp = roundP inp <|> square inp <|> curly inp

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
data SnailAst
    = Lexeme (SourcePos, Text)
    | TextLiteral (SourcePos, Text)
    | SExpression (Maybe Char) Bracket [SnailAst]
    deriving (Eq, Show)

{- | Any 'Text' object that starts with an appropriately valid character. This
 could be an variable or function name. For example, `hello` is a valid
 lexeme in the s-expression below.

 @
 (hello)
 @
-}
lexeme :: Parser SnailAst
lexeme = do
    sourcePosition <- getSourcePos
    initial <- oneOf initialCharacter
    rest <- many $ oneOf subsequentCharacter
    pure $ Lexeme (sourcePosition, Text.pack $ initial : rest)

-- | An escaped quote to support nesting `"` inside a 'textLiteral'
escapedQuote :: Parser Text
escapedQuote = string "\\\""

-- | Matches any non-quote character
nonQuoteCharacter :: Parser Text
nonQuoteCharacter = do
    character <- anySingleBut '\"'
    pure $ Text.singleton character

quote :: Parser Char
quote = char '\"'

quotes :: Parser a -> Parser a
quotes = between quote quote

{- | Matches a literal text and supports nested quotes, e.g.

 @
 ("hello\"")
 @
-}
textLiteral :: Parser SnailAst
textLiteral = do
    sourcePosition <- getSourcePos
    mText <- quotes . optional $ some $ escapedQuote <|> nonQuoteCharacter
    notFollowedBy (oneOf subsequentCharacter <|> char '\"')
    pure $ case mText of
        Nothing -> TextLiteral (sourcePosition, "")
        Just text -> TextLiteral (sourcePosition, Text.concat text)

{- | Parse one of the possible structures in 'SnailAst'. These are parsed
 recursively separated by 'spaces' in 'sExpression'.
-}
leaves :: Parser SnailAst
leaves = lexeme <|> textLiteral <|> sExpression

-- | Parse an 'SExpression'
sExpression :: Parser SnailAst
sExpression = do
    startingChar <- optional (oneOf parenthesisStartingCharacter)
    (bracketType, expr) <- bracket (leaves `sepEndBy` spaces)
    pure $ SExpression startingChar bracketType expr

-- | Parse a valid snail file
snailAst :: Parser [SnailAst]
snailAst = (spaces *> sExpression `sepEndBy1` spaces) <* eof
