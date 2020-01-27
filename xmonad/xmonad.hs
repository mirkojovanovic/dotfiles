import XMonad
import XMonad.Config.Desktop
import System.IO
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)


main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ desktopConfig
    { terminal             = myTerminal
    , manageHook           = manageDocks <+> myManageHook <+> manageHook desktopConfig
    , borderWidth          = myBorderWidth
    , layoutHook           = avoidStruts $ layoutHook desktopConfig
    , logHook              = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle  = xmobarColor "#bababa" "" . shorten 50
      }
    } `additionalKeys` myKeys

myTerminal    = "termite"

myBorderWidth = 1

myManageHook  = composeAll
  [ className =? "Gimp"        --> doFloat
  , className =? "Arandr"      --> doFloat
  , className =? "keepassx2"   --> doFloat
  , className =? "qutebrowser" --> doShift "2"
  , className =? "discord"     --> doShift "3"
  , className =? "slack"       --> doShift "3"
  ]

myKeys =
  [ ((mod1Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
  , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
  , ((0, xK_Print), spawn "scrot")
  ]
