-- Base
import XMonad hiding (Tall(..))
import System.Exit
import System.IO
import qualified XMonad.StackSet as W
import System.Directory (getHomeDirectory)
import Data.Semigroup
import Text.Read
import Data.List (elemIndex)
import Text.Printf

-- Hook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.DynamicProperty

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SimpleFloat
import XMonad.Layout.HintedTile
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.Combo
import XMonad.Layout.Master
import XMonad.Layout.StateFull (focusTracking)
import XMonad.Layout.Renamed

--Utilities
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP, removeKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Loggers
--import XMonad.Util.XProp

-- Actions
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Actions.RotSlaves
import XMonad.Actions.RotateSome
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowBringer

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Window 
import XMonad.Prompt.AppLauncher

import qualified XMonad.Util.ExtensibleState as XS

-- Font to use
myFont :: String
myFont = "xft:Mononoki Nerd Font:pixelsize=12:antialias=true:hinting=true"
  
-- Terminal to use
myTerminal :: String
myTerminal = "alacritty"

-- Focus follows mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Define mod keys
myModMask :: KeyMask
--myModMask = mod4Mask
myModMask = mod4Mask

-- Define volume keys and commands
lowerVolumeCmd = "pactl set-sink-volume @DEFAULT_SINK@ -2%"
raiseVolumeCmd = "pactl set-sink-volume @DEFAULT_SINK@ +2%"
muteVolumeCmd  = "pactl set-sink-mute @DEFAULT_SINK@ toggle"

-- Count windows
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Define workspaces
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- Width of window border
myBorderWidth = 2

-- Border colors
myNormalBorderColor = "#282828"
myFocusedBorderColor = "#B16286"

-- Config for xmonad prompts
myXPConfig = 
    def { font                = myFont
        , bgColor             = "#282828" 
        , fgColor             = "#EBDBB2" 
        , fgHLight            = "#B16286" 
        , bgHLight            = "#282828" 
        , borderColor         = "#B16286"
        , promptBorderWidth   = 2
        , position            = CenteredAt 0.5 0.25
        , height              = 40
        , historySize         = 256
        , defaultText         = ""
        , autoComplete        = Nothing
        , historyFilter       = id
        , showCompletionOnTab = False
        , promptKeymap        = defaultXPKeymap
        }

-- Config for tabs
myTabTheme =
  def { fontName            = myFont
      , activeColor         = "#B16286" 
      , inactiveColor       = "#282828" 
      , activeBorderColor   = "#B16286" 
      , inactiveBorderColor = "#282828"  
      , activeTextColor     = "#282828" 
      , inactiveTextColor   = "#B16286" 
      , decoHeight          = 15
      }

myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "lxsession &"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "imwheel -b 45 &"
    spawnOnce "udiskie &"
    spawnOnce "dunst -conf ~/.config/dunst/dunstrc"
    spawnOnce "emacs --daemon"

-- Config layouts
myLayout = windowNavigation
         $ renamed [CutWordsLeft 1]
         $ spacing 3
	 $ smartBorders
         (masterTab ||| (tabbed shrinkText myTabTheme) ||| tiled Tall ||| noBorders Full) 
  where
      -- tiled = Tall nmaster delta ratio
      tiled = HintedTile 1 0.03 0.5 TopLeft
      -- master and tabbed tiling
      masterTab = renamed [Replace "Master Tab"] $ mastered (1/100) (1/2) $ (focusTracking (tabbed shrinkText myTabTheme))

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "discord"  spawnDiscord findDiscord manageDiscord
                , NS "keepassxc"  spawnKeepass findKeepass manageKeepass
                , NS "qalculate-gtk"  spawnCal findCal manageCal
                , NS "scratch-emacs"  spawnEmacs findEmacs manageEmacs]
    where
        -- Basic terminal
        spawnTerm  = myTerminal ++ " -t terminal"
        findTerm   = title =? "terminal"
        manageTerm = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h
                l = 0.95 -w
        -- Discord 
        spawnDiscord  = "discord"
        findDiscord   = appName =? "discord"
        manageDiscord = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h
                l = 0.95 -w
        -- Keepass
        spawnKeepass  = "keepassxc"
        findKeepass   = appName =? "keepassxc"
        manageKeepass = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h
                l = 0.95 -w
        -- Calculator
        spawnCal  = "qalculate-gtk"
        findCal   = appName =? "qalculate-gtk"
        manageCal = customFloating $ W.RationalRect l t w h
            where
                h = 0.125
                w = 0.1
                t = 0.15 -h
                l = 0.55 -w
        -- Basic emacs
        spawnEmacs  = "emacsclient -c -n -e --eval '(set-frame-name \"scratch-emacs\")'"
        findEmacs   = title =? "scratch-emacs"
        manageEmacs = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h
                l = 0.95 -w

-- Set default display modes for applications
myManageHook = composeAll . concat $
    -- Float fullscreen apps (mostly games)
    [[className =? c --> doFloat | c <- myFloats],
    [isDialog --> doCenterFloat,
     isFullscreen --> doFullFloat,
     className =? "net-runelite-client-RuneLite" --> doFloat,
     className =? "mpv" --> doRectFloat (W.RationalRect 0.55 0.05 0.4 0.4),
     className =? "Steam" --> doFullFloat,
     className =? "Superslicer" --> doFullFloat,
     isInProperty "WM_WINDOW_ROLE" "pop-up" --> doRectFloat (W.RationalRect 0.55 0.05 0.4 0.4),
     namedScratchpadManageHook myScratchPads]]
    where
      myFloats = [
         "MPlayer"
       , "Gimp"
       , "Plasma-desktop"
       , "plasmashell"
       , "krunner"
       , "Klipper"
       , "Keepassx"
       , "latte-dock"
       , "lattedock"
       , "conky-semi"
       , "TeamViewer"
       , "teamviewer"
       , "ksmserver-logout-greeter"]

-- Set dynamic display modes
myEventHook :: Event -> X All
myEventHook = dynamicPropertyChange "WM_NAME" (title =? "scratch-emacs" --> floating)
                  where floating = customFloating $ W.RationalRect (1/6) 0.05 (2/3) 0.9
-- Log hook
myLogHook = historyHook <+> updatePointer (0.5, 0.5) (0, 0)

myKeys :: String -> [([Char], X ())]
myKeys home =
  [
    --------------------------------------------------
    -- Window/Focus Manipulation
    --------------------------------------------------
    -- Rotate through the available layout algorithms
      ("M-<Space>", sendMessage NextLayout)
    -- Shrink the master area
    , ("M-C-h", sendMessage Shrink)
    -- Expand the master area
    , ("M-C-l", sendMessage Expand)
    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)
    -- close focused window
    , ("M-q", kill)
    -- Move focus to the next window.
    , ("M-j", windows W.focusDown)
    -- Move focus to the previous window.
    , ("M-k", windows W.focusUp)
    -- Swap the focused window with the next window.
    , ("M-S-j", windows W.swapDown)
    -- Swap the focused window with the previous window.
    , ("M-S-k", windows W.swapUp)
    -- Swap the focused window with the next window.
    , ("M-C-j", rotSlavesDown)
    -- Swap the focused window with the previous window.
    , ("M-C-k", rotSlavesUp)
    -- Move focus to the master window.
    , ("M-m", windows W.focusMaster)
    -- Swap the focused window and the master window.
    , ("M-S-m", windows W.swapMaster)
    -- Increment number of windows in master
    , ("M-.", sendMessage (IncMasterN 1))
    -- Decrement number of windows in master
    , ("M-,", sendMessage (IncMasterN (-1)))
    -- Swap the focused window and the master window.
    , ("M-b", nextMatch History (return True))
    -- Bring a window to focus.
    , ("M-z", bringMenu)
    -- Remove workspace
    , ("M-r", removeWorkspace)
    -- Rename workspace
    , ("M-S-r", renameWorkspace myXPConfig)
    -- Add or select workspace
    , ("M-a", selectWorkspace myXPConfig)
    -- Toggle Struts 
    , ("M-i", sendMessage ToggleStruts)

    --------------------------------------------------
    -- Basic Utils
    --------------------------------------------------
    -- Spawn terminal
    , ("M-<Return>"  , spawn "alacritty")
    -- Spawn rofi drun
    , ("M-w", spawn "rofi -show window -theme gruvbox-dark-soft -show-icons")
    , ("M-S-w", spawn "rofi -show drun -theme gruvbox-dark-soft -show-icons")

    --------------------------------------------------
    -- Scratchpads
    --------------------------------------------------

    -- Spawn terminal scratchpad
    , ("M-S-<Return>", namedScratchpadAction myScratchPads "terminal")
    -- Spawn discord scratchpad
    , ("M-d", namedScratchpadAction myScratchPads "discord")
    -- Spawn keepass scratchpad
    , ("M-p", namedScratchpadAction myScratchPads "keepassxc")
    -- Spawn calendar scratchpad
    , ("M-c", namedScratchpadAction myScratchPads "qalculate-gtk")
    -- Spawn emacs scratchpad
    , ("M-e", namedScratchpadAction myScratchPads "scratch-emacs")

    --------------------------------------------------
    -- Open Applications
    --------------------------------------------------
    -- Spawn firefox
    , ("M-o b"  , spawn "chromium")
    -- Spawn lutris
    , ("M-o l"  , spawn "lutris")
    -- Spawn steam
    , ("M-o s"  , spawn "steam")
    -- Spawn flameshot
    , ("M-o f"  , spawn "flameshot gui")
    -- Spawn emacs
    , ("M-o e"  , spawn "emacsclient -c -n -e '(switch-to-buffer nil)'")

    --------------------------------------------------
    -- System Utils
    --------------------------------------------------
    -- Recompile and restart xmonad
    , ("M-x r", spawn "xmonad --recompile; xmonad --restart")
    -- Quit xmonad
    , ("M-x q", io (exitWith ExitSuccess))
    -- Start gamemode
    , ("M-x g", spawn "gamemoded -r")
    -- Stop gamemode
    , ("M-x S-g", spawn "killall gamemoded")
    -- Open nvidia-settings 
    , ("M-x n", spawn "nvidia-settings")
    -- mute overall volume
    , ("<XF86AudioMute>", spawn muteVolumeCmd)
    -- raise overall volume
    , ("<XF86AudioRaiseVolume>", spawn raiseVolumeCmd)
    -- lower overall volume
    , ("<XF86AudioLowerVolume>", spawn lowerVolumeCmd)
  ]
  ++
  -- Switch to dynamically created workspace
  zip (["M-<F1>","M-<F2>","M-<F3>","M-<F4>",
        "M-<F5>","M-<F6>","M-<F7>","M-<F8>"]) (map (withNthWorkspace W.greedyView) [10..])
  ++
  -- Shift windows to dynamically created workspace
  zip (["M-S-<F1>","M-S-<F2>","M-S-<F3>","M-S-<F4>",
        "M-S-<F5>","M-S-<F6>","M-S-<F7>","M-S-<F8>"]) (map (withNthWorkspace W.shift) [10..])

-- Remove the default binding for quit xmonad
rmKeys :: String -> [(KeyMask, KeySym)]
rmKeys keys =
  [
    (myModMask .|. shiftMask, xK_q)
  ]

main = do
    home <- getHomeDirectory
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/xmobarrc1"
    -- The monad
    xmonad  
      $ docks
      $ ewmh
      $ ewmhFullscreen
      $ navigation2DP def
                     ("", "h", "", "l")
                     [("M-", screenGo),
                      ("M-S-", screenSwap)]
                     False
      $ def
        {
        -- Simple items
        terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        -- Hooks, Layouts
        layoutHook = avoidStruts $ myLayout,
        manageHook = myManageHook,
        handleEventHook = myEventHook,
        logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc0 x
	    		    >> hPutStrLn xmproc1 x
            , ppCurrent = xmobarColor "#B8BB26" "" . wrap "[" "]"    -- Current workspace in xmobar
            , ppVisible = xmobarColor "#83A598" ""                -- Visible but not current workspace
            , ppHidden = xmobarColor "#83A598" "" . wrap "*" ""   -- Hidden workspaces in xmobar
            , ppHiddenNoWindows= \( _ ) -> ""                                -- Only shows visible workspaces. Useful for TreeSelect.
            , ppTitle = xmobarColor "#EBDBB2" "" . shorten 60     -- Title of active window in xmobar
            , ppSep = "<fc=" ++ "#EBDBB2" ++ "> | </fc>"            -- Separators in xmobar
            , ppUrgent = xmobarColor "#FB2934" "" . wrap "!" "!"             -- Urgent workspace
            , ppExtras = [windowCount]                                       -- # of windows current workspace
            , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]},
        startupHook = myStartupHook
        } `removeKeys` rmKeys home
          `additionalKeysP` myKeys home
