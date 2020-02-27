
-- Base
import System.IO
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import XMonad
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), shiftNextScreen, shiftPrevScreen)
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

-- Layouts
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier

colorRed = "#d75f5f"

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.xmobar/xmobarrc0"
  xmproc1 <- spawnPipe "xmobar -x 1 ~/.xmobar/xmobarrc1"
  xmonad $ desktopConfig
    { terminal    = myTerminal
    , workspaces  = myWorkspaces
    , modMask     = myModMask
    , manageHook  = myManageHook
    , layoutHook  = myLayoutHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook     = dynamicLogWithPP xmobarPP
      { ppOutput  = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x
      , ppTitle   = xmobarColor "#bababa" "" . shorten 50
      , ppSep     = "<fc=" ++ colorRed ++ "> : </fc>"
      , ppCurrent = xmobarColor colorRed "" . wrap "[" "]"
      , ppVisible = xmobarColor colorRed "" . wrap "(" ")"
      }
    } `additionalKeysP` myKeys

myTerminal    = "termite"
myModMask     = mod4Mask

myBorderWidth = 2
myNormalBorderColor = "#1c1c1c"
myFocusedBorderColor = "#d75f5f"

myWorkspaces  = ["term", "web", "chat", "dev", "media", "read"] ++ map show [7..9]

-- Manage Hook
myManageHook  = composeAll
  [ manageDocks
  , manageHook desktopConfig
  , className =? "Gimp"                    --> doFloat
  , className =? "Arandr"                  --> doFloat
  , className =? "keepassx2"               --> doFloat
  , className =? "mpv"                     --> doFloat
  , className =? "qutebrowser"             --> doShift "web"
  , className =? "discord"                 --> doShift "chat"
  , className =? "slack"                   --> doShift "chat"
  , className =? "sun-awt-X11-XDialogPeer" --> doFloat
  , className =? "sun-awt-X11-XWindowPeer" --> doFloat
  , className =? "sun-awt-X11-XWindowPeer" --> doShift "dev"
  , className =? "jetbrains-studio"        --> doShift "dev"
  ]

-- Key configuration
myKeys =
  [ ("M-S-z"     , spawn "~/.local/bin/lock_screen")

  -- Taking screenshots
  , ("C-<Print>" , spawn "sleep 0.2; scrot -s")
  , ("<Print>"   , spawn "scrot")

  -- Workspaces
  , ("M-S-l", moveTo Next nonNSP)
  , ("M-S-h", moveTo Prev nonNSP)
  , ("M-<Tab>", cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)

  -- Windows
  , ("M-v", windows copyToAll)
  , ("M-S-v", killAllOtherCopies)

  -- Multimedia keys
  , ("<XF86AudioPlay>", spawn "cmus-remote -u")
  , ("<XF86AudioNext>", spawn "cmus-remote -n")
  , ("<XF86AudioPrev>", spawn "cmus-remote -r")

  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")

  , ("<XF86AudioRaiseVolume>", spawn "ponymix increase 5")
  , ("<XF86AudioLowerVolume>", spawn "ponymix decrease 5")
  , ("<XF86AudioMute>", spawn "ponymix toggle")
  ] where nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))

-- Layout Hook
myLayoutHook = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True
             $ smartBorders
             $ avoidStruts
             $ onWorkspace "chat" Full
             $ onWorkspace "web" Full
             $ layoutHook desktopConfig

