module Snail.Characters (validCharacter, parenthesisStartingCharacter) where

-- | The valid character set of Snail
validCharacter :: String
validCharacter = ['a' .. 'z'] <> ['A' .. 'Z'] <> digitCharacter <> specialInitialCharacter

-- | Characters allowed in numbers
digitCharacter :: String
digitCharacter = ['0' .. '9']

-- | Special initial characters
specialInitialCharacter :: String
specialInitialCharacter = "!$%&*/:=?^_~#,'+-.@\\<>"

-- | Characters allowed in front of an s-expression
parenthesisStartingCharacter :: String
parenthesisStartingCharacter = "'`@#,"
