module Defines
  ( module System.FilePath,
    role,
    leftClick,
    rightClick,
    middleClick,
    wmain,
    docs,
    code,
    term,
    chat,
    pics,
    mySpaces,
    W.shift,
  )
where

import XMonad
import XMonad.StackSet qualified as W
import System.FilePath

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

leftClick, rightClick, middleClick :: Button
(leftClick, rightClick, middleClick) = (button1, button3, button2)

wmain, docs, code, term, chat, pics :: WorkspaceId
(wmain, docs, code, term, chat, pics) = ("main", "docs", "code", "term", "chat", "pics")

mySpaces :: [WorkspaceId]
mySpaces = [wmain, docs, code, term, chat, pics, "7", "8", "9"]
