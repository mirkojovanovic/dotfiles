
-- Base
import System.IO
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import XMonad
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), shiftNextScreen, shiftPrevScreen)
import XMonad.Actions.CycleRecentWS (cycleRecentWS)
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)

-- Hooks
import XMonad.Hooks.ManageDocks (manageDocks, Direction1D (Next, Prev), avoidStruts)
import XMonad.Hooks.ManageHelpers (doCenterFloat)

-- Utils
import XMonad.Util.EZConfig(additionalKeysP)

-- Layouts
import XMonad.Layout.PerWorkspace(onWorkspaces)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

main = do
  xmonad $ defaultConfig
    { terminal           = myTerminal
    , workspaces         = myWorkspaces
    , modMask            = myModMask
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    } `additionalKeysP` myKeys

myTerminal    = "urxvt"
myModMask     = mod4Mask

-- Border settings
myBorderWidth = 2
myNormalBorderColor = "#120324"
myFocusedBorderColor = "#d75f5f"

myWorkspaces  = ["term", "web", "chat", "dev", "media", "read"] ++ map show [7..9]

-- {{{ MyManageHook

myManageHook  = manageDocks <+> (composeAll . concat)
  [ [className =? c --> doFloat        | c <- classFloat]
  , [className =? c --> doCenterFloat  | c <- classCenterFloat]
  , [appName   =? c --> doCenterFloat  | c <- appNameCenterFloat]
  , [className =? c --> doShift "web"  | c <- classShiftWeb]
  , [className =? c --> doShift "chat" | c <- classShiftChat]
  , [className =? c --> doShift "dev"  | c <- classShiftDev]
  , [appName   =? c --> doShift "dev"  | c <- appNameShiftDev]
  , [className =? c --> doShift "read" | c <- classShiftRead]
  ]
  where
    classFloat =
      [ "Gimp"
      , "Arandr"
      , "mpv"
      ]

    classCenterFloat =
      [ "GParted"
      , "KeePassXC"
      , "Barrier"
      , "Sxiv"
      ]

    appNameCenterFloat =
      [ "sun-awt-X11-XWindowPeer"
      , "sun-awt-X11-XFramePeer"
      ]

    classShiftWeb =
      [ "qutebrowser" ]

    classShiftChat =
      [ "discord"
      , "slack"
      , "ViberPC"
      ]

    classShiftDev =
      [ "jetbrains-studio" ]

    classShiftRead =
      [ "calibre" ]

    appNameShiftDev =
      [ "sun-awt-X11-XDialogPeer"
      , "sun-awt-X11-XWindowPeer"
      , "sun-awt-X11-XFramePeer"
      ]

-- }}}

-- {{{ myKeys

myKeys =
  [ ("M-S-z"     , spawn "~/.local/bin/lock_screen")

  -- Custom dmenu
  , ("M-p", spawn "dmenu_run -fn 'SourceCodePro:style=regular:size=11' -nf '#bababa' -nb '#1c1c1c' -sf '#000' -sb '#d75f5f' -p 'dmenu:'")

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

-- }}}

-- {{{ myLayoutHook

myLayoutHook = smartBorders
             $ spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True
             $ onWorkspaces ["chat", "web"] Full
             $ layoutHook defaultConfig

-- }}}

