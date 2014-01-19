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
    , ((mod1Mask                 , xK_F4)     , kill)
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

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ gnomeConfig {
    terminal            = myTerminal
  , modMask             = myModMask
  , normalBorderColor   = myNormalBorderColor
  , focusedBorderColor  = myFocusedBorderColor
  , borderWidth         = myBorderWidth
  , handleEventHook     = fullscreenEventHook
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
