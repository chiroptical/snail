module Snail.Characters (initialCharacter, subsequentCharacter, parenthesisStartingCharacter) where

-- | The initial character of any text
initialCharacter :: String
initialCharacter = ['a' .. 'z'] <> ['A' .. 'Z'] <> digitCharacter <> specialInitialCharacter

{- | The subsequent characters allowed in any text

Allows `(-<>)` but not `(<>)`
-}
subsequentCharacter :: String
subsequentCharacter = initialCharacter <> specialSubsequentCharacter

-- | Characters allowed in numbers
digitCharacter :: String
digitCharacter = ['0' .. '9']

-- | Special initial characters
specialInitialCharacter :: String
specialInitialCharacter = "!$%&*/:=?^_~#,'+-.@\\<>"

-- | Additional subsequent characters
specialSubsequentCharacter :: String
specialSubsequentCharacter = "<>"

-- | Characters allowed in front of an s-expression
parenthesisStartingCharacter :: String
parenthesisStartingCharacter = "'`@#,"
