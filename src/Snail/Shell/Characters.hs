module Snail.Shell.Characters where

-- | ...
initialCharacter :: String
initialCharacter = ['a' .. 'z'] <> ['A' .. 'Z']

-- | ...
specialInitialCharacter :: String
specialInitialCharacter = "!$%&*/:<=>?^_~"

-- | ...
peculiarCharacter :: String
peculiarCharacter = "+-."

-- | ...
digitCharacter :: String
digitCharacter = ['0' .. '9']

-- | ...
specialSubsequentCharacter :: String
specialSubsequentCharacter = "+-.@"
