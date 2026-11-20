{-
  Definitions

  * Lexer: `Text -> [Text]`
  * Token: The leaves in your AST, e.g. TextLiteral, Number, etc

  Here, we implement a structurally aware lexer that supports one token type
  (text literals) for convenience.
-}
{-# LANGUAGE BangPatterns #-}

module Snail.Lexer (
    sExpression,
    snailAst,

    -- * Exported for testing
    nonQuoteCharacter,
    textLiteral,
) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Snail.Ast
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
spaces = do
    _ <- takeWhileP (Just "white space") isSpace
    input <- getInput
    case Text.take 2 input of
        "--" -> skipLineComment *> spaces
        "{-" -> skipBlockComment *> spaces
        _ -> pure ()

-- | Parse a 'Text' verbatim
symbol :: Text -> Parser Text
symbol = L.symbol spaces

openingBracket :: Parser Bracket
openingBracket = (Round <$ symbol "(") <|> (Square <$ symbol "[") <|> (Curly <$ symbol "{")

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
    txt <- takeWhile1P (Just "identifier character") isValidCharacter
    pure $ Lexeme (sourcePosition, Text.copy txt)

-- | Use `\` as an escape character. Capture the next character verbatim.
escapedCharacter :: Parser Text
escapedCharacter = do
    slash <- char '\\'
    character <- anySingle
    pure $ Text.pack [slash, character]

unescapedChunk :: Parser Text
unescapedChunk = takeWhile1P Nothing (\c -> c /= '"' && c /= '\\')

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
    chunks <- quotes $ many (escapedCharacter <|> unescapedChunk)
    notFollowedBy (satisfy isValidCharacter <|> char '\"')
    let !text = case chunks of
            [chunk] -> Text.copy chunk
            _ -> Text.concat chunks
    pure $ TextLiteral (sourcePosition, text)

{- | Parse one of the possible structures in 'SnailAst'. These are parsed
 recursively separated by 'spaces' in 'sExpression'.
-}
leaves :: Parser SnailAst
leaves = (openingBracket >>= listStartingWith Nothing) <|> (lookAhead anySingle >>= leafStartingWith)

leafStartingWith :: Char -> Parser SnailAst
leafStartingWith next
    | next == '"' = textLiteral
    | next `elem` parenthesisStartingCharacter = try sExpression <|> lexeme
    | otherwise = lexeme

listStartingWith :: Maybe Text -> Bracket -> Parser SnailAst
listStartingWith prefix bracket = do
    expr <- leaves `sepEndBy` spaces
    _ <- symbol $ case bracket of
        Round -> ")"
        Square -> "]"
        Curly -> "}"
    pure $ SExpression prefix bracket expr

listPrefix :: Parser Text
listPrefix = Text.copy <$> takeWhile1P (Just "list prefix") (`elem` parenthesisStartingCharacter)

-- | Parse an 'SExpression'
sExpression :: Parser SnailAst
sExpression = do
    prefix <- optional listPrefix
    bracket <- openingBracket
    listStartingWith prefix bracket

-- | Parse a valid snail file
snailAst :: Parser [SnailAst]
snailAst = (spaces *> sExpression `sepEndBy1` spaces) <* eof
