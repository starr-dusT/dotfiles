-------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------
-- Base
import XMonad
import Data.Monoid
import System.Exit
import System.IO (hPutStrLn)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
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
-- Utilities
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
-- Actions
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
-- Prompt
import XMonad.Prompt

------------------------------------------------------------------------
-- VARIABLES 
------------------------------------------------------------------------
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
    , position              = Bottom 
    }
warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Bottom 
    }
------------------------------------------------------------------------
-- START UP 
------------------------------------------------------------------------
myStartupHook = do
    spawnOnce "nitrogen --restore &"

------------------------------------------------------------------------
-- KEYBINDS 
------------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm,  xK_Return), spawn $ XMonad.terminal conf)
    -- Launch emacs
    , ((modm,  xK_e), spawn "emacsclient -c")
    -- Launch emacs
    , ((modm .|. shiftMask,  xK_e), spawn "emacsclient -e '(save-buffers-kill-emacs)'")
    -- Launch emacs
    , ((modm,  xK_f), spawn "emacsclient -c -a '' --eval '(dired nil)'")
    -- launch rofi with run
    , ((modm, xK_w), spawn "rofi -show drun")
    -- launch rofi with window 
    , ((modm .|. shiftMask, xK_w), spawn "rofi -show window")
    -- mute overall volume
    , ((0, 0x1008ff12), spawn muteVolumeCmd)
    -- raise overall volume
    , ((0, 0x1008ff13), spawn raiseVolumeCmd)
    -- lower overall volume
    , ((0, 0x1008ff11), spawn lowerVolumeCmd)
    -- raise mpd volume
    , ((modm, 0x1008ff13), spawn "mpc volume +2")
    -- lower mpd volume
    , ((modm, 0x1008ff11), spawn "mpc volume -2")
    -- Download youtube audio from clipboard link
    , ((modm, xK_y), spawn "youtube-audio-dl")
    -- switch to project prompt
    , ((modm, xK_p), switchProjectPrompt warmPromptTheme)
    -- move window to project prompt
    , ((modm .|. shiftMask, xK_p), shiftToProjectPrompt warmPromptTheme)
    -- move window to project prompt
    , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
    -- terminal scratchpad
    , ((modm, xK_n), namedScratchpadAction myScratchPads "newsboat")
    -- discord scrathpad 
    , ((modm, xK_d), namedScratchpadAction myScratchPads "discord-canary")
    -- terminal scrathpad 
    , ((modm .|. shiftMask, xK_d), namedScratchpadAction myScratchPads "terminal")
    -- start gamemode 
    , ((modm, xK_g), spawn "gamemoded -r")
    -- start gamemode 
    , ((modm .|. shiftMask, xK_g), spawn "killall gamemoded")
    -- launch firefox
    , ((modm, xK_F1), spawn "brave")
    -- launch discord 
    , ((modm, xK_F2), spawn "discord")
    -- launch lutris
    , ((modm, xK_F3), spawn "lutris")
    -- launch steam 
    , ((modm, xK_F4), spawn "steam")
    -- mpd go to previous track
    , ((modm, xK_F9), spawn "mpc prev")
    -- mpd pause/start track
    , ((modm, xK_F10), spawn "mpc toggle")
    -- mpd go to next track
    , ((modm, xK_F11), spawn "mpc next")
    -- launch vifm
    , ((modm, xK_v), spawn "alacritty -e vifm")
    -- close focused window
    , ((modm, xK_q), kill)
     -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- flameshot gui open
    , ((modm, xK_z), spawn "flameshot gui")
    -- Resize viewed windows to the correct size
    , ((modm .|. shiftMask, xK_z), refresh)
    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp)
    -- Move focus to the master window
    , ((modm, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
    , ((modm, xK_c), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
    , ((modm,xK_h), sendMessage Shrink)
    -- Expand the master area
    , ((modm,xK_l), sendMessage Expand)
    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm, xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modm .|. altMask, xK_q), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm .|. altMask, xK_r), spawn "xmonad --recompile; xmonad --restart")]
    ++
    -- navigate between workspaces 
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
    -- navigate between screens
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_d, xK_f] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))]

------------------------------------------------------------------------
-- LAYOUTS 
------------------------------------------------------------------------
myLayout = spacing 2 $ smartBorders (tiled ||| Mirror tiled ||| Full ||| ThreeCol 1 (3/100) (1/2))
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio = 1/2
        -- Percent of screen to increment by when resizing panes
        delta = 2/100

------------------------------------------------------------------------
-- SCRATCHPADS 
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "newsboat" spawnNews findNews manageNews 
                , NS "discord-canary"  spawnDiscord findDiscord manageDiscord ]
    where
        spawnTerm  = myTerminal ++ " -t terminal" 
        findTerm   = title =? "terminal"
        manageTerm = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h 
                l = 0.95 -w
    
        spawnNews  = myTerminal ++ " -t ncmpcpp -e ncmpcpp" 
        findNews   = title =? "ncmpcpp" 
        manageNews = customFloating $ W.RationalRect l t w h
            where
                h = 0.5
                w = 0.5
                t = 0.25 
                l = 0.25 

        spawnDiscord  = "discord-canary"
        findDiscord   = appName =? "discord" 
        manageDiscord = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h 
                l = 0.95 -w

------------------------------------------------------------------------
-- DYNAMIC PROJECTS 
------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = "desktopConfig"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "alacritty -e nvim ~/.config/xmobar/xmobarrc"
                                           spawn "alacritty -e nvim ~/.bashrc"
                                           spawn "alacritty -e nvim ~/.xmonad/xmonad.hs"
                                           windows W.swapDown
                                           windows W.swapDown
                                           windows W.swapDown
            }

  , Project { projectName      = "pythonDev"
            , projectDirectory = "~/dev/python"
            , projectStartHook = Just $ do spawn "alacritty -e ~/.config/scripts/tmux-python.sh"
            }
  ]

------------------------------------------------------------------------
-- HOOKS
------------------------------------------------------------------------
myManageHook = composeAll
    -- Float fullscreen apps (mostly games)
    [isDialog --> doCenterFloat,
     isFullscreen --> doFullFloat,
     className =? "Gimp" --> doFullFloat, 
     className =? "Anki" --> doFullFloat, 
     className =? "mpv" --> doRectFloat (W.RationalRect 0.55 0.05 0.4 0.4), 
     className =? "Steam" --> doFullFloat, 
     namedScratchpadManageHook myScratchPads]

------------------------------------------------------------------------
-- EVENTS
------------------------------------------------------------------------
myEventHook = mempty 

------------------------------------------------------------------------
-- LOGGING 
------------------------------------------------------------------------
myLogHook = return ()

------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main = do 
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    --
    xmonad $ docks $ ewmh $ dynamicProjects projects $ def
	--ewmhFullscreen
        {
        -- Simple items 
        terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        -- Key bindings
        keys = myKeys,
        mouseBindings = myMouseBindings,
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
        }
