module Defines
  ( module System.FilePath,
    module System.Directory,
    module Text.Printf,
    wmain,
    docs,
    code,
    term,
    chat,
    pics,
    mySpaces,
  )
where

import System.Directory
import System.FilePath
import Text.Printf

wmain, docs, code, term, chat, pics :: String
(wmain, docs, code, term, chat, pics) = ("main", "docs", "code", "term", "chat", "pics")

mySpaces :: [String]
mySpaces = [wmain, docs, code, term, chat, pics, "7", "8", "9"]
