module Snail.Ast (
    -- * Constructors for AST
    Bracket (..),
    SnailAst (..),

    -- * Utilities for AST
    unwrap,
) where

import Data.Text (Text)
import Text.Megaparsec

-- | The bracket used to surround the s-expression
data Bracket
    = Round
    | Square
    | Curly
    deriving (Show, Eq)

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

{- | Unwrap nested s-expressions, this is very useful when you are converting
'SnailAst' to your own AST.

You'll likely have a function `snailAstToMyAst :: SnailAst -> m MyAst` where
`m` is `ExceptT` or `MonadExcept`. Then, your final case statement should
unwrap nested expressions to their base with,

@
snailAstToMyAst . unwrap . SExpression initialChar bracket $ unwrap <$> exprs`
@

For example, `((read))` will become `SExpression _ _ [Lexeme (_, "read")]`
which your AST converter should handle.
-}
unwrap :: SnailAst -> SnailAst
unwrap = \case
    SExpression _ _ [x] -> x
    x -> x
