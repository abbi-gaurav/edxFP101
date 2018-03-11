module GlobRegex
  (
    globToRegex
  , matchesGlob
  )
where

import           Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = fmap (\res -> '^' : res ++ "$") (globToRegex' cs)


-- does not work
globToRegexI :: String -> Either GlobError String
globToRegexI cs = fmap (\res -> "?i^" ++ res ++ "$") (globToRegex' cs)

globToRegex' :: String -> Either GlobError String
globToRegex' ""                   = Right ""
globToRegex' ('*' : cs)           = fmap (\res -> ".*" ++ res) (globToRegex' cs)
globToRegex' ('?' : cs)           = fmap (\res -> '.' : res) (globToRegex' cs)
globToRegex' ('[' : '!' : c : cs) = fmap (\res -> "[^" ++ c : res) (charClass cs)
globToRegex' ('[' : c : cs)       = fmap (\res -> '[' : c : res) (charClass cs)
globToRegex' ('[':_)              = Left "unterminated character class"
globToRegex' (c : cs)             = fmap (\res -> escape c ++ res) (globToRegex' cs)

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']' : cs) = fmap (\res -> ']' : res) (globToRegex' cs)
charClass (c:cs)     = fmap (\res -> c : res) (charClass cs)
charClass []         = Left "unterminated character class"


matchesGlob :: FilePath -> String -> Either GlobError Bool
name `matchesGlob` pat = fmap (\res -> name =~ res) (globToRegex pat)
