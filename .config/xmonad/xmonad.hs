import XMonad
import System.IO
import Data.List

import qualified XMonad.StackSet as W
import XMonad.ManageHook

---------------------------------
--        Hook imports         --
---------------------------------
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.OnPropertyChange
import XMonad.Hooks.SetWMName

---------------------------------
--        Util imports         --
---------------------------------
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

---------------------------------
--        Layout imports       --
---------------------------------
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders

---------------------------------
--          Variables          --
---------------------------------

-- Terminal --
_terminal :: String
_terminal = "alacritty"

_terminalCN :: String
_terminalCN = "Alacritty"

-- Browser --
_browserCN :: String
_browserCN = "LibreWolf"
_browser :: String
_browser = "librewolf"

-- Custom command line apps --
_commandApps :: [String]
_commandApps = [ "ncspot", "btop" ]

-- Media --
_mediaCN :: [String]
_mediaCN = [ "FreeTube" ]

-- Social --
_socialCN :: [String]
_socialCN = [ "steam", "discord" ]

-- Games --
_gamesCN :: [String]
_gamesCN = [ "steam_app_", "Starsector", "dwarfort", "factorygame-" ]

-- Border --
_borderWidth :: Dimension
_borderWidth = 0

-- ModMask --
_modMask :: KeyMask
_modMask = mod1Mask

-- Colors --
_visible         = xmobarColor "#b5e278" ""
_hidden          = xmobarColor "#82aaff" ""
_hiddenNoWindows = xmobarColor "#c792ea" ""
_title           = xmobarColor "#ffca73" ""
_urgent          = xmobarColor "#c45500" ""

-- Window count --
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

---------------------------------
--           Layouts           --
---------------------------------
_tiled = renamed [ Replace "* - - -" ]
       $ noBorders
       $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
       $ Tall nmain delta ratio
       where
         nmain = 1
         ratio = 1/2
         delta = 3/100

_mtiled = renamed [ Replace "- * - -" ]
        $ noBorders
        $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
        $ Mirror
        $ Tall nmain delta ratio
        where
          nmain = 1
          ratio = 1/2
          delta = 3/100

_grid = renamed [ Replace "- - * -" ]
      $ noBorders
      $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
      $ Grid

_full = renamed [ Replace "- - - *" ]
      $ noBorders
      $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
      $ Full

_layout = _tiled ||| _mtiled ||| _grid ||| _full

---------------------------------
--         Workspaces          --
---------------------------------
_workspaces :: [String]      
_workspaces = [ "dev", "www", "media", "social", "games", "system" ] ++ map show [ 7..9 ]
      
---------------------------------      
--            Hooks            --      
---------------------------------      
_manageAppHook = composeAll . concat $
  [ [ className =? _terminalCN <&&> _notInCommandApps --> doShift (_workspaces !! 0) ]
  , [ className =? "Emacs"                            --> doShift (_workspaces !! 0) ]
  , [ className =? _browserCN                         --> doShift (_workspaces !! 1) ]
  , [ className =? c                                  --> doShift (_workspaces !! 2) | c <- _mediaCN ]
  , [ title     =? "ncspot"                           --> doShift (_workspaces !! 2) ]
  , [ title     =? "btop"                             --> doShift (_workspaces !! 5) ]
  , [ className =? c                                  --> doShift (_workspaces !! 3) | c <- _socialCN ]
  , [ fmap ( c `isInfixOf` ) className                --> doShift (_workspaces !! 4) | c <- _gamesCN ]
  ]
  where
    _notInCommandApps :: Query Bool
    _notInCommandApps = ask >>= \w -> liftX $ fmap (`notElem` _commandApps) (runQuery title w)

-- ManageHook --
_manageHook =
  _manageAppHook
  <+> manageHook def

-- HandleEventHook --
_eventHook =
  onXPropertyChange "WM_NAME" _manageAppHook
  <+> handleEventHook def

-- StartupHook --
_startupHook = do
  spawnOnce "xrandr --output DP-0 --primary --mode 2560x1440 --dpi 109 \
              \ --output HDMI-0 --mode 1920x1080 --dpi 82 --left-of DP-0"
  spawnOnce "feh --bg-fill $HOME/Pictures/Wallpapers/red_planet.jpg"
  spawnOnce "picom"
  spawnOnce "xbanish"
  spawnOnce "emacs --daemon"

---------------------------------
--         Keybindings         --
---------------------------------
_keys =
  [ ("M-<Return>",   spawn _terminal)
  , ("M-p",          spawn "dmenu_run -c -l 20")
  , ("M-e",          spawn "emacsclient -nc")
  , ("M-S-e",        spawn "reem")
  , ("M-S-<Return>", spawn _browser)
  , ("M-t",          sendMessage $ JumpToLayout "* - - -")
  , ("M-m",          sendMessage $ JumpToLayout "- * - -")
  , ("M-g",          sendMessage $ JumpToLayout "- - * -")
  , ("M-f",          sendMessage $ JumpToLayout "- - - *")
  ]

---------------------------------
--           Config            --
---------------------------------
_config = def
  { modMask           = _modMask
  , terminal          = _terminal
  , borderWidth       = _borderWidth
  , focusFollowsMouse = True
  , XMonad.workspaces = _workspaces
  , startupHook       = _startupHook
  , layoutHook        = _layout
  , manageHook        = _manageHook
  , handleEventHook   = _eventHook
  }
  `additionalKeysP`
  _keys

---------------------------------
--           Xmobar            --
---------------------------------
_xmobarPP :: PP
_xmobarPP             = def
  { ppSep             = "<fc=#686868> | </fc>"
  , ppWsSep           = "   "
  , ppTitleSanitize   = xmobarStrip
  , ppCurrent         = _visible . wrap "[" "]"
  , ppVisible         = _visible
  , ppHidden          = _hidden
  , ppHiddenNoWindows = _hiddenNoWindows
  , ppTitle           = _title . shorten 60
  , ppUrgent          = _urgent . wrap "!" "!"
  , ppExtras          = [ windowCount ]
  , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
  }

---------------------------------
--            Main             --
---------------------------------
main :: IO()
main = xmonad
     . ewmhFullscreen
     . ewmh
     $ withEasySB (statusBarProp "xmobar -x 0 $HOME/.config/xmonad/xmobar/xmobarrc" (pure _xmobarPP)) defToggleStrutsKey
     $ setupInsertPosition Below Newer
     $ _config
