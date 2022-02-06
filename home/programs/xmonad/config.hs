import XMonad

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

main :: IO ()
main = xmonad . ewmh $ def
  { modMask = mod4Mask
  , layoutHook = smartBorders Full
  }
  `additionalKeysP`
  [ ( "M-p", spawn "rofi -modi drun,ssh,window -show drun -show-icons" )
  ]
