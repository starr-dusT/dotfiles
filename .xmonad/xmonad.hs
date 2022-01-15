-- Base
import XMonad hiding (Tall(..))
import System.Exit
import System.IO
import qualified XMonad.StackSet as W
import System.Directory (getHomeDirectory)
import Data.Semigroup
-- Hooks
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

--Utilities
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP, removeKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.ClickableWorkspaces (clickablePP)
-- Actions
import XMonad.Actions.DynamicProjects (Project (..), dynamicProjects, switchProjectPrompt, shiftToProjectPrompt, switchProject, shiftToProject)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Navigation2D
-- Prompt
import XMonad.Prompt

-- Terminal to use
myTerminal :: String
myTerminal = "alacritty"
-- Focus follows mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
-- Define mod keys
myModMask :: KeyMask
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
myNormalBorderColor  = "#ebdbb2"
myFocusedBorderColor = "#d3869b"
-- Configuration for myNav2D
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Spacing Full", centerNavigation)]
    , unmappedWindowRect        = [("Spacing Full", singleWindowRect)]
    }

myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "lxsession &"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "xmodmap ~/.config/xmodmap/Xmodmap"
    spawnOnce "imwheel -b 45 &"
    spawnOnce "play-with-mpv &"
    spawnOnce "udiskie &"
    spawnOnce "dunst -conf ~/.config/dunst/dunstrc"

projects :: [Project]
projects =
  [ Project { projectName      = "dev"
            , projectDirectory = "~/devel"
            , projectStartHook = Just $ do spawn "emacs"
                                           spawn myTerminal
            },
   Project { projectName      = "game"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  ]

myLayout = windowNavigation $ spacing 2 $ smartBorders (tiled Tall ||| tiled Wide ||| Full ||| simpleFloat ||| Grid)
    where
        -- default tiling algorithm partitions the screen into two panes
        --tiled = Tall nmaster delta ratio
        tiled = HintedTile 1 0.03 0.5 TopLeft
        -- The default number of windows in the master pane
        --nmaster = 1
        -- Default proportion of screen occupied by master pane
        --ratio = 1/2
        -- Percent of screen to increment by when resizing panes
        --delta = 2/100

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "discord"  spawnDiscord findDiscord manageDiscord
                , NS "keepassxc"  spawnKeepass findKeepass manageKeepass
                , NS "gsimplecal"  spawnCal findCal manageCal
                , NS "scratch-emacs"  spawnEmacs findEmacs manageEmacs ]
    where
        spawnTerm  = myTerminal ++ " -t terminal"
        findTerm   = title =? "terminal"
        manageTerm = customFloating $ W.RationalRect l t w h
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

        spawnKeepass  = "keepassxc"
        findKeepass   = appName =? "keepassxc"
        manageKeepass = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h
                l = 0.95 -w

        spawnCal  = "gsimplecal"
        findCal   = appName =? "gsimplecal"
        manageCal = customFloating $ W.RationalRect l t w h
            where
                h = 0.125
                w = 0.1
                t = 0.15 -h
                l = 0.55 -w

        spawnEmacs  = "emacsclient -c -n -e --eval '(set-frame-name \"scratch-emacs\")'"
        findEmacs   = title =? "scratch-emacs"
        manageEmacs = customFloating $ W.RationalRect l t w h
            where
                h = 0.9
                w = 0.9
                t = 0.95 -h
                l = 0.95 -w

-- Set default display modes for applications
myManageHook = composeAll
    -- Float fullscreen apps (mostly games)
    [isDialog --> doCenterFloat,
     isFullscreen --> doFullFloat,
     className =? "Gimp" --> doFullFloat,
     className =? "mpv" --> doRectFloat (W.RationalRect 0.55 0.05 0.4 0.4),
     className =? "Steam" --> doFullFloat,
     className =? "Superslicer" --> doFullFloat,
     isInProperty "WM_WINDOW_ROLE" "pop-up" --> doRectFloat (W.RationalRect 0.55 0.05 0.4 0.4),
     namedScratchpadManageHook myScratchPads]
-- Set dynamic display modes
myEventHook :: Event -> X All
myEventHook = dynamicPropertyChange "WM_NAME" (title =? "scratch-emacs" --> floating)
                  where floating = customFloating $ W.RationalRect (1/6) 0.05 (2/3) 0.9
-- Log hook
myLogHook = updatePointer (0.5, 0.5) (0, 0)

myKeys :: String -> [([Char], X ())]
myKeys home =
  [
    --------------------------------------------------
    -- Window/Focus Manipulation
    --------------------------------------------------
    -- Rotate through the available layout algorithms
      ("M-<Tab>", sendMessage NextLayout)
    -- Shrink the master area
    , ("M-C-h", sendMessage Shrink)
    -- Expand the master area
    , ("M-C-l", sendMessage Expand)
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
    , ("M-w"  , spawn "rofi -show drun -theme gruvbox-dark-soft -show-icons")
    , ("M-S-w"  , spawn "rofi -show run -theme gruvbox-dark-soft")

    --------------------------------------------------
    -- Scratchpads
    --------------------------------------------------

    -- Spawn terminal scratchpad
    , ("M-S-<Return>", namedScratchpadAction myScratchPads "terminal")
    -- Spawn discord scratchpad
    , ("M-d", namedScratchpadAction myScratchPads "discord")
    -- Spawn keepass scratchpad
    , ("M-m", namedScratchpadAction myScratchPads "keepassxc")
    -- Spawn calendar scratchpad
    , ("M-c", namedScratchpadAction myScratchPads "gsimplecal")
    -- Spawn emacs scratchpad
    , ("M-e", namedScratchpadAction myScratchPads "scratch-emacs")

    --------------------------------------------------
    -- Dynamic Projects
    --------------------------------------------------
    --, ("M-p s", switchProjectPrompt projectsTheme)
    --, ("M-p S", shiftToProjectPrompt projectsTheme)
    , ("M-p d", switchProject (projects !! 0))
    , ("M-p S-d", shiftToProject (projects !! 0))
    , ("M-p g", switchProject (projects !! 1))
    , ("M-p S-g", shiftToProject (projects !! 1))

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
    -- Start wireguard
    , ("M-x w", spawn "pkexec sh -c 'wg-quick up wg0 && mount -a'")
    -- Stop wireguard
    , ("M-x S-w", spawn "pkexec sh -c 'umount /run/media/engi && wg-quick down wg0'")
    -- mute overall volume
    , ("<XF86AudioMute>", spawn muteVolumeCmd)
    -- raise overall volume
    , ("<XF86AudioRaiseVolume>", spawn raiseVolumeCmd)
    -- lower overall volume
    , ("<XF86AudioLowerVolume>", spawn lowerVolumeCmd)
  ]
-- Remove the default binding for quit xmonad
rmKeys :: String -> [(KeyMask, KeySym)]
rmKeys keys =
  [
    (myModMask .|. shiftMask, xK_q)
  ]

main = do
    home <- getHomeDirectory
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    --
    xmonad
      $ dynamicProjects projects
      $ docks
      $ ewmhFullscreen
      $ withNavigation2DConfig myNav2DConf
      $ navigation2DP def
                         ("k", "h", "j", "l")
                         [("M-", windowGo),
                          ("M-S-", windowSwap)]
                         False
      $ additionalNav2DKeysP ("", "u", "", "i")
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
            , ppCurrent = xmobarColor "#b8bb26" "" . wrap "[" "]"            -- Current workspace in xmobar
            , ppVisible = xmobarColor "#83a598" ""                           -- Visible but not current workspace
            , ppHidden = xmobarColor "#83a598" "" . wrap "*" ""              -- Hidden workspaces in xmobar
            , ppHiddenNoWindows= \( _ ) -> ""                                -- Only shows visible workspaces. Useful for TreeSelect.
            , ppTitle = xmobarColor "#ebdbb2" "" . shorten 60                -- Title of active window in xmobar
            , ppSep =  "<fc=#ebdbb2> | </fc>"                                -- Separators in xmobar
            , ppUrgent = xmobarColor "#fb4934" "" . wrap "!" "!"             -- Urgent workspace
            , ppExtras = [windowCount]                                       -- # of windows current workspace
            , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]},
        startupHook = myStartupHook
        } `removeKeys` rmKeys home
          `additionalKeysP` myKeys home
