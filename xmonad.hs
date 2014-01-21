import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import XMonad.Config.Gnome
import XMonad.Actions.CycleWS

myModMask            = mod4Mask       -- changes the mod key to "super"
myTerminal           = "gnome-terminal"
myNormalBorderColor  = "#33335"
myFocusedBorderColor = "#A0A0D0"
myBorderWidth        = 4

myDmenuConfig = " -l 20 -fn 'Ubuntu Mono-10' -nb '#333335' -sb '#A0A0D0' -nf '#CCCCCF' -sf '#15151A' "

myKeys =
  [
      -- open dmenu
      ((myModMask                , xK_p)     , spawn ("dmenu_run" ++ myDmenuConfig))
      -- open terminal
    , ((mod1Mask  .|. controlMask, xK_t)     , spawn (myTerminal))
      -- next workspace
    , ((myModMask .|. controlMask, xK_Right) , nextWS)
    , ((mod1Mask  .|. controlMask, xK_Right) , nextWS)
      -- prev workspace
    , ((myModMask .|. controlMask, xK_Left)  , prevWS)
    , ((mod1Mask  .|. controlMask, xK_Left)  , prevWS)
      -- next window (alt+tab)
    , ((mod1Mask                 , xK_Tab)   , windows W.focusDown)
      -- prev window (alt+shift+tab)
    , ((mod1Mask  .|. shiftMask  , xK_Tab)   , windows W.focusUp)
      -- gnome-session-quit (doesn't work)
    , ((myModMask .|. shiftMask  , xK_q)     , spawn ("gnome-session-quit"))
      -- lock screen
    , ((mod1Mask  .|. shiftMask  , xK_l)     , spawn ("dm-tool lock"))
    , ((mod1Mask                 , xK_F4)    , kill)
      -- shrink current window
    , ((myModMask                , xK_a)     , sendMessage MirrorShrink)
      -- expand current window
    , ((myModMask                , xK_z)     , sendMessage MirrorExpand)
  ]

{-
  -- Xmobar configuration variables. These settings control the appearance
  -- of text which xmonad is sending to xmobar via the DynamicLog hook.
  --
-}

myTitleColor     = "#eeeeee" -- color of window title
myTitleLength    = 80 -- truncate window title to this length
myCurrentWSColor = "#e6744c" -- color of active workspace
myVisibleWSColor = "#c185a7" -- color of inactive workspace
myUrgentWSColor  = "#cc0000" -- color of workspace with 'urgent' window
myCurrentWSLeft  = "[" -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "(" -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft   = "{" -- wrap urgent workspace with these
myUrgentWSRight  = "}"

myManagementHooks :: [ManageHook]
myManagementHooks = [
  ]

{-
  Layout configuration. In this section we identify which xmonad
  layouts we want to use. I have defined a list of default
  layouts which are applied on every workspace, as well as
  special layouts which get applied to specific workspaces.

  Note that all layouts are wrapped within "avoidStruts". What this does
  is make the layouts avoid the status bar area at the top of the screen.
  Without this, they would overlap the bar. You can toggle this behavior
  by hitting "super-b" (bound to ToggleStruts in the keyboard bindings
  in the next section).
-}

-- Define group of default layouts used on most screens, in the
-- order they will appear.
-- "smartBorders" modifier makes it so the borders on windows only
-- appear if there is more than one visible window.
-- "avoidStruts" modifier makes it so that the layout provides
-- space for the status bar at the top of the screen.
defaultLayouts = smartBorders(avoidStruts(
  -- ResizableTall layout has a large master window on the left,
  -- and remaining windows tile on the right. By default each area
  -- takes up half the screen, but you can resize using "super-h" and
  -- "super-l".
  ResizableTall 1 (3/100) (1/2) []

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders Full

  -- Grid layout tries to equally distribute windows in the available
  -- space, increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| Grid

  -- ThreeColMid layout puts the large master window in the center
  -- of the screen. As configured below, by default it takes of 3/4 of
  -- the available space. Remaining windows tile to both the left and
  -- right of the master window. You can resize using "super-h" and
  -- "super-l".
  ||| ThreeColMid 1 (3/100) (3/4)

  -- Circle layout places the master window in the center of the screen.
  -- Remaining windows appear in a circle around it
  ||| Circle))

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ gnomeConfig {
    terminal            = myTerminal
  , modMask             = myModMask
  , normalBorderColor   = myNormalBorderColor
  , focusedBorderColor  = myFocusedBorderColor
  , borderWidth         = myBorderWidth
  , handleEventHook     = fullscreenEventHook
  , layoutHook          = defaultLayouts
  , startupHook         = do
      setWMName "LG3D"
      spawn "~/.xmonad/startup-hook"
  , manageHook          = manageHook defaultConfig
      <+> composeAll myManagementHooks
      <+> manageDocks
  , logHook             = dynamicLogWithPP $ xmobarPP {
        ppOutput  = hPutStrLn    xmproc
      , ppTitle   = xmobarColor  myTitleColor "" . shorten myTitleLength
      , ppCurrent = xmobarColor  myCurrentWSColor ""
          . wrap myCurrentWSLeft myCurrentWSRight
      , ppVisible = xmobarColor  myVisibleWSColor ""
          . wrap myVisibleWSLeft myVisibleWSRight
      , ppUrgent  = xmobarColor  myUrgentWSColor ""
          . wrap myUrgentWSLeft  myUrgentWSRight
    }
} `additionalKeys` myKeys
