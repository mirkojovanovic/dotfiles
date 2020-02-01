
import System.IO
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), shiftNextScreen, shiftPrevScreen)

import XMonad
import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ desktopConfig
    { terminal    = myTerminal
    , workspaces  = myWorkspaces
    , modMask     = myModMask
    , manageHook  = manageDocks <+> myManageHook <+> manageHook desktopConfig
    , layoutHook  = myLayoutHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook     = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle  = xmobarColor "#bababa" "" . shorten 50
      , ppCurrent = xmobarColor "#d75f5f" "" . wrap "[" "]"
      }
    } `additionalKeysP` myKeys

myTerminal    = "termite"
myModMask     = mod4Mask

myBorderWidth = 1
myNormalBorderColor = "#1c1c1c"
myFocusedBorderColor = "#f799d7"

myWorkspaces  = ["term", "web", "dev", "chat", "media", "read"] ++ map show [7..9]

myManageHook  = composeAll
  [ className =? "Gimp"        --> doFloat
  , className =? "Arandr"      --> doFloat
  , className =? "keepassx2"   --> doFloat
  , className =? "qutebrowser" --> doShift "web"
  , className =? "discord"     --> doShift "chat"
  , className =? "slack"       --> doShift "chat"
  ]

myKeys =
  [ ("M-S-z"     , spawn "~/.local/bin/lock_screen")

  -- Taking screenshots
  , ("C-<Print>" , spawn "sleep 0.2; scrot -s")
  , ("<Print>"   , spawn "scrot")

  -- Workspaces
  , ("M-l", moveTo Next nonNSP)
  , ("M-h", moveTo Prev nonNSP)
  , ("M-S-l", shiftTo Next nonNSP >> moveTo Next nonNSP)
  , ("M-S-h", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
  ] where nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))
          nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))


myLayoutHook = avoidStruts
             $ smartBorders
             $ onWorkspace "chat" Full
             $ layoutHook desktopConfig
