module Snail.Characters (validCharacter, parenthesisStartingCharacter) where

-- | The valid character set of Snail
validCharacter :: String
validCharacter = ['a' .. 'z'] <> ['A' .. 'Z'] <> digitCharacter <> specialInitialCharacter <> unicodeCharacter

-- | Characters allowed in numbers
digitCharacter :: String
digitCharacter = ['0' .. '9']

-- | Special initial characters
specialInitialCharacter :: String
specialInitialCharacter = "!$%&*/:=?^_~#,'+-.@\\<>"

-- | A deliberately small set of allowed Unicode characters for identifiers
unicodeCharacter :: String
unicodeCharacter = "λαβγδεθμσφω∀∃→←⇒⇐↔⇔∘≤≥≠∧∨¬⊤⊥∈∉⊆⊂∪∩"

-- | Characters allowed in front of an s-expression
parenthesisStartingCharacter :: String
parenthesisStartingCharacter = "'`@#,"
