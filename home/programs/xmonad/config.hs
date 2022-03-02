import System.Exit

import XMonad

import           XMonad.Actions.FloatKeys              (keysAbsResizeWindow, keysResizeWindow)
import           XMonad.Actions.RotSlaves              (rotSlavesUp)

import XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Actions.WithAll                ( killAll )

import XMonad.Util.EZConfig
import XMonad.Util.NamedActions -- ((^++^), addDescrKeys', addName, sendMessage', subtitle)
import XMonad.Util.Ungrab

import qualified XMonad.StackSet                       as W
import qualified XMonad.Util.NamedWindows              as W
import qualified Control.Exception                     as E

main :: IO ()
main = xmonad . ewmh . keybindings $ def
  { terminal = myTerminal
  , modMask  = myModMask
  }
  -- `additionalKeysP`
  -- [ ( "M-p", spawn "rofi -modi drun,ssh,window -show drun -show-icons" )
  -- ]
  where
    keybindings = addDescrKeys' ((myModMask, xK_z), xMessage) myKeys
    myModMask = mod4Mask

myTerminal   = "alacritty"
appLauncher  = "rofi -modi drun,ssh,window -show drun -show-icons"
screenLocker = "betterlockscreen -l dim"

-- myKeys c =
--   (subtitle "Custom Keys":) $ mkNamedKeymap c $
--   [("M-x a", addName "useless message" $ spawn "xmessage foo"),
--     ("M-c", sendMessage' Expand)]
--   ^++^
--   [("<XF86AudioPlay>", spawn "mpc toggle" :: X ()),
--     ("<XF86AudioNext>", spawn "mpc next")]

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf@XConfig{XMonad.modMask = modm} =
  keySet "Launchers"
  [ key "Rofi"        (modm , xK_p )                  $ spawn appLauncher
  , key "Terminal"    (modm .|. shiftMask, xK_Return) $ spawn (XMonad.terminal conf)
  , key "Lock screen" (modm .|. shiftMask, xK_l)      $ spawn screenLocker
  ] ^++^
  keySet "Windows"
  [ key "Close focused"   (modm              , xK_BackSpace) kill
  , key "Close all in ws" (modm .|. shiftMask, xK_BackSpace) killAll
  , key "Refresh size"    (modm              , xK_k        ) refresh
  , key "Focus next"      (modm              , xK_e        ) $ windows W.focusDown
  , key "Focus previous"  (modm              , xK_i        ) $ windows W.focusUp
  , key "Focus master"    (modm              , xK_h        ) $ windows W.focusMaster
  , key "Swap master"     (modm              , xK_Return   ) $ windows W.swapMaster
  , key "Swap next"       (modm .|. shiftMask, xK_u        ) $ windows W.swapDown
  , key "Swap previous"   (modm .|. shiftMask, xK_y        ) $ windows W.swapUp
  , key "Shrink master"   (modm              , xK_n        ) $ sendMessage Shrink
  , key "Expand master"   (modm              , xK_o        ) $ sendMessage Expand
  , key "Switch to tile"  (modm              , xK_a        ) $ withFocused (windows . W.sink)
  , key "Rotate slaves"   (modm .|. shiftMask, xK_Tab      ) rotSlavesUp
  , key "Decrease size"   (modm              , xK_d        ) $ withFocused (keysResizeWindow (-10,-10) (1,1))
  , key "Increase size"   (modm              , xK_c        ) $ withFocused (keysResizeWindow (10,10) (1,1))
  , key "Decr  abs size"  (modm .|. shiftMask, xK_d        ) $ withFocused (keysAbsResizeWindow (-10,-10) (1024,752))
  , key "Incr  abs size"  (modm .|. shiftMask, xK_c        ) $ withFocused (keysAbsResizeWindow (10,10) (1024,752))
  ] ^++^
  keySet "System"
  [ key "Quit"            (modm .|. shiftMask, xK_q     ) $ io exitSuccess
  , key "Restart"         (modm              , xK_q     ) $ spawn "xmonad --recompile && xmonad --restart"
  ] ^++^
  keySet "Switching workspaces"
  [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
  | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
  , (i, k) <- zip (XMonad.workspaces conf) [xK_0 .. xK_9]
  ] ^++^
  keySet "Switching screens"
  [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
  | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shiftMask, "Move client to screen number ")]
  , (key, sc) <- zip [xK_r, xK_s, xK_t] [0..]]

  where
    keySet s ks = subtitle s : ks
    key n k a = (k, addName n a)

   --  [ subtitle "launching and killing programs"
   --  , ((modm .|. shiftMask, xK_Return), addName "Launch Terminal" $ spawn $ XMonad.terminal conf) -- %! Launch terminal
   --  , ((modm,               xK_p     ), addName "Launch dmenu" $ spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- %! Launch dmenu
   --  , ((modm .|. shiftMask, xK_p     ), addName "Launch gmrun" $ spawn "gmrun") -- %! Launch gmrun
   --  , ((modm .|. shiftMask, xK_c     ), addName "Close the focused window" kill) -- %! Close the focused window

   --  , subtitle "changing layouts"
   --  , ((modm,               xK_space ), sendMessage' NextLayout) -- %! Rotate through the available layout algorithms
   --  , ((modm .|. shiftMask, xK_space ), addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

   --  , separator
   --  , ((modm,               xK_n     ), addName "Refresh" refresh) -- %! Resize viewed windows to the correct size

   --  , subtitle "move focus up or down the window stack"
   --  , ((modm,               xK_Tab   ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
   --  , ((modm .|. shiftMask, xK_Tab   ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
   --  , ((modm,               xK_j     ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
   --  , ((modm,               xK_k     ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
   --  , ((modm,               xK_m     ), addName "Focus the master" $ windows W.focusMaster  ) -- %! Move focus to the master window

   --  , subtitle "modifying the window order"
   --  , ((modm,               xK_Return), addName "Swap with the master" $ windows W.swapMaster) -- %! Swap the focused window and the master window
   --  , ((modm .|. shiftMask, xK_j     ), addName "Swap down" $ windows W.swapDown  ) -- %! Swap the focused window with the next window
   --  , ((modm .|. shiftMask, xK_k     ), addName "Swap up"   $ windows W.swapUp    ) -- %! Swap the focused window with the previous window

   --  , subtitle "resizing the master/slave ratio"
   --  , ((modm,               xK_h     ), sendMessage' Shrink) -- %! Shrink the master area
   --  , ((modm,               xK_l     ), sendMessage' Expand) -- %! Expand the master area

   --  , subtitle "floating layer support"
   --  , ((modm,               xK_t     ), addName "Push floating to tiled" $ withFocused $ windows . W.sink) -- %! Push window back into tiling

   --  , subtitle "change the number of windows in the master area"
   --  , ((modm              , xK_comma ), sendMessage' (IncMasterN 1)) -- %! Increment the number of windows in the master area
   --  , ((modm              , xK_period), sendMessage' (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

   --  , subtitle "quit, or restart"
   --  , ((modm .|. shiftMask, xK_q     ), addName "Quit" $ io exitSuccess) -- %! Quit xmonad
   --  , ((modm              , xK_q     ), addName "Restart" $ spawn "xmonad --recompile && xmonad --restart") -- %! Restart xmonad
   --  ]

   --  -- mod-[1..9] %! Switch to workspace N
   --  -- mod-shift-[1..9] %! Move client to workspace N
   --  ++
   --  subtitle "switching workspaces":
   --  [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
   --      | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
   --      , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
   --  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
   --  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
   -- ++
   -- subtitle "switching screens" :
   -- [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
   --      | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shiftMask, "Move client to screen number ")]
   --      , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]


-- showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
-- showKeybindings x = addName "Show Keybindings" . io $
--   E.bracket (spawnPipe $ getAppCommand yad) hClose (\h -> hPutStr h (unlines $ showKm x))
