import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.EwmhDesktops (ewmh)

main :: IO ()
main = xmonad . ewmh $ def
  { modMask = mod4Mask
  }
  `additionalKeysP`
  [ ( "M-p", spawn "rofi -modi drun,ssh,window -show drun -show-icons" )
  ]
