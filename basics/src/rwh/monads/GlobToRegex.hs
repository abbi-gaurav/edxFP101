module GlobToRegex where

import           Control.Monad
import           Logger

globToRegex cs =
  globToRegex' cs >>= \ds ->
  return ('^' : ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?' : cs) =
  record "any" >>
  globToRegex' cs >>= \ds ->
  return ('.' : ds)
globToRegex' ('*' : cs) = do
  record "kleene star"
  ds <- globToRegex' cs
  return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) =
  record "character class negative" >>
  charClass cs >>= \ds ->
  return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
  record "character class" >>
  charClass cs >>= \ds ->
  return ("[" ++ c : ds)
globToRegex' ('[': _) =
  fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c
  | c `elem` regexChars = record "escape" >> return ['\\', c]
  | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"

charClassWordy :: String -> Logger String
charClassWordy (']': cs) = do
  record "end character class"
  ds <- globToRegex' cs
  return (']' : ds)
charClassWordy (c:cs) = do
  record "normal character"
  ds <- charClassWordy cs
  return (c:ds)

charClass (']' : cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs)     = (c:) `liftM` charClass cs
