-- Base
import XMonad
import Data.Monoid
import System.Exit
import System.IO 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Directory (getHomeDirectory)

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

-- Utilities
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeysP, removeKeys)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)

-- Actions
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
import XMonad.Actions.Commands

-- Prompt
import XMonad.Prompt

-- Data
import Data.Maybe (isJust)

-- Terminal to use
myTerminal      = "alacritty"

-- Focus follows mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Define mod keys
myModMask = mod4Mask
altMask = mod1Mask

-- Define volume keys and commands
lowerVolumeCmd = "pulseaudio-ctl down 2"
raiseVolumeCmd = "pulseaudio-ctl up 2"
muteVolumeCmd  = "pulseaudio-ctl mute"

-- Count windows
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Define workspaces
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

-- Width of window border
myBorderWidth = 2 

-- Border colors
myNormalBorderColor  = "#ebdbb2"
myFocusedBorderColor = "#d3869b"

-- Prompt theming
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"
yellow = "#504945"
base03 = "#ebdbb2"
active = "#b8bb26"
prompt = 20 

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0 
    , height                = prompt
    , position              = Top 
    }
    
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "lxsession &"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "imwheel -b 45 &"
    spawnOnce "play-with-mpv &"
    
------------------------------------------------------------------------}}}
-- Layouts                                                              {{{
--
-- Alot stolen from https://github.com/altercation/dotfiles-tilingwm 
---------------------------------------------------------------------------

myLayout = windowNavigation $ spacing 2 $ smartBorders (tiled ||| Mirror tiled ||| Full ||| ThreeColMid 1 (3/100) (1/2) ||| tabTwoPane)
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio = 1/2
        -- Percent of screen to increment by when resizing panes
        delta = 2/100
        
        tabTwoPane = renamed [Replace "TwoPane Tabs"] $ combineTwo (TwoPane 0.03 0.5) (tabs) (tabs)
        
        tabs = tabbed shrinkText myTabTheme
               
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "scr-mpv" spawnMpv findMpv manageMpv
                , NS "discord"  spawnDiscord findDiscord manageDiscord ]
    where
        spawnTerm  = myTerminal ++ " -t terminal" 
        findTerm   = title =? "terminal"
        manageTerm = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h 
                l = 0.95 -w
    
        spawnMpv  = "mpv --player-operation-mode=pseudo-gui --title=scr-mpv" 
        findMpv = title =? "scr-mpv" 
        manageMpv = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h 
                l = 0.95 -w

        spawnDiscord  = "discord"
        findDiscord   = appName =? "discord" 
        manageDiscord = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h 
                l = 0.95 -w

myManageHook = composeAll
    -- Float fullscreen apps (mostly games)
    [isDialog --> doCenterFloat,
     isFullscreen --> doFullFloat,
     className =? "Gimp" --> doFullFloat, 
     className =? "Anki" --> doFullFloat, 
     className =? "mpv" --> doRectFloat (W.RationalRect 0.55 0.05 0.4 0.4), 
     className =? "Steam" --> doFullFloat, 
     className =? "microsoft teams - preview" --> doFullFloat, 
     namedScratchpadManageHook myScratchPads]

myEventHook = mempty

myLogHook = return ()

myKeys :: String -> [([Char], X ())]
myKeys home = 
  [
    -------------------------------------------------- 
    -- Window/Focus Manipulation 
    -------------------------------------------------- 
    -- Rotate through the available layout algorithms
      ("M-<Tab>", sendMessage NextLayout)
    -- Move focus to the next window
    , ("M-j", windows W.focusDown)
    -- Move focus to the previous window
    , ("M-k", windows W.focusUp)
    -- Switch focus to next monitor
    , ("M-.", nextScreen)
    -- Switch focus to prev monitor
    , ("M-,", prevScreen)  
    -- Swap the focused window with the next window/tabs
    , ("M-S-j", sequence_ [windows W.swapDown, sendMessage $ Move R])
    -- Swap the focused window with the previous window/tabs
    , ("M-S-k", sequence_ [windows W.swapUp, sendMessage $ Move L])
    -- Move focus to the master window
    , ("M-m", windows W.focusMaster)
    -- Swap the focused window and the master window
    , ("M-c", windows W.swapMaster)
    -- Shrink the master area
    , ("M-h", sendMessage Shrink)
    -- Expand the master area
    , ("M-l", sendMessage Expand)
    -- Copy window to all workspaces 
    , ("M-x c", toggleCopyToAll)
    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)
    -- close focused window
    , ("M-q", kill)

    -------------------------------------------------- 
    -- Basic Utils 
    -------------------------------------------------- 
    -- Spawn terminal 
    , ("M-<Return>"  , spawn "alacritty")
    -- Spawn rofi drun 
    , ("M-w"  , spawn "rofi -show drun")
    -- Spawn rofi window 
    , ("M-S-w", spawn "rofi -show window")
     
    -------------------------------------------------- 
    -- Scratchpads 
    -------------------------------------------------- 
    -- Spawn rofi window 
    , ("M-S-<Return>", namedScratchpadAction myScratchPads "terminal")
    -- Spawn rofi window 
    , ("M-d", namedScratchpadAction myScratchPads "discord")
    -- Spawn rofi window 
    , ("M-v", namedScratchpadAction myScratchPads "scr-mpv")

    -------------------------------------------------- 
    -- Open Applications 
    -------------------------------------------------- 
    -- Spawn firefox 
    , ("M-o b"  , spawn "brave")
    -- Spawn lutris 
    , ("M-o l"  , spawn "lutris")
    -- Spawn steam 
    , ("M-o s"  , spawn "steam")
    -- Spawn flameshot 
    , ("M-o c"  , spawn "flameshot gui")
    -- Spawn emacs 
    , ("M-o e"  , spawn "emacs")

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
    -- mute overall volume
    , ("<XF86AudioMute>", spawn muteVolumeCmd)
    -- raise overall volume
    , ("<XF86AudioRaiseVolume>", spawn raiseVolumeCmd)
    -- lower overall volume
    , ("<XF86AudioLowerVolume>", spawn lowerVolumeCmd)
  ]
  where
    toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
      [] -> windows copyToAll
      _ -> killAllOtherCopies

rmKeys :: String -> [(KeyMask, KeySym)]
rmKeys keys = 
  [
    -- Remove the default quit xmonad bind
    (myModMask .|. shiftMask, xK_q)
  ]

main = do 
    home <- getHomeDirectory
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    --  
    xmonad $ docks $ ewmh $ ewmhFullscreen def
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
            , ppCurrent = xmobarColor "#b8bb26" "" . wrap "[" "]" -- Current workspace in xmobar
            , ppVisible = xmobarColor "#83a598" ""                -- Visible but not current workspace
            , ppHidden = xmobarColor "#83a598" "" . wrap "*" ""   -- Hidden workspaces in xmobar
            , ppHiddenNoWindows= \( _ ) -> ""                     -- Only shows visible workspaces. Useful for TreeSelect.
            , ppTitle = xmobarColor "#ebdbb2" "" . shorten 60     -- Title of active window in xmobar
            , ppSep =  "<fc=#ebdbb2> | </fc>"                     -- Separators in xmobar
            , ppUrgent = xmobarColor "#fb4934" "" . wrap "!" "!"  -- Urgent workspace
            , ppExtras = [windowCount]                           -- # of windows current workspace
            , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]},
        startupHook = myStartupHook 
        } `removeKeys` rmKeys home
          `additionalKeysP` myKeys home
