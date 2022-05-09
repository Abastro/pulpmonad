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
    game,
    mySpaces,
  )
where

import System.Directory
import System.FilePath
import Text.Printf

wmain, docs, code, term, chat, pics, game :: String
(wmain, docs, code, term, chat, pics, game) = ("main", "docs", "code", "term", "chat", "pics", "game")

mySpaces :: [String]
mySpaces = [wmain, docs, code, term, chat, pics, game, "8", "9"]
