module Snail.ToText (toText) where

import Data.Text
import Snail.Lexer

bracket :: Text -> Bracket -> Text
bracket txt = \case
    Round -> "(" <> txt <> ")"
    Square -> "[" <> txt <> "]"
    Curly -> "{" <> txt <> "}"

toText :: SnailAst -> Text
toText = \case
    TextLiteral (_, txt) -> "\"" <> txt <> "\""
    Lexeme (_, lexeme) -> lexeme
    SExpression Nothing bracketKind exprs ->
        let txt = Data.Text.unwords $ toText <$> exprs
         in bracket txt bracketKind
    SExpression (Just c) bracketKind exprs ->
        let txt = Data.Text.unwords $ toText <$> exprs
         in singleton c <> bracket txt bracketKind
