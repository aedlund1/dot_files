-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import System.IO
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86

-- Utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

-- actions
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutModifier

-------------------------------------------------------------------------------
-- Main --
main = do
    xmproc <- spawnPipe "/home/aren/.cabal/bin/xmobar /home/aren/.xmobarrc"
    bottomBar <- spawnPipe "/home/aren/.cabal/bin/xmobar /home/aren/.xmobarrc2"
    hPutStrLn bottomBar ""
    xmonad $ myConfig { logHook = dynamicLogWithPP xmobarPP
                                       { ppOutput = hPutStrLn xmproc
                                       , ppTitle = xmobarColor "green" "" . shorten 50
                                       }
                      }
 
-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = borderWidth'
                         , normalBorderColor = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , terminal = terminal'
                         , keys = keys'
                         , layoutHook = layoutHook'
                         , manageHook = manageHook'
                         , handleEventHook = fullscreenEventHook
                         , startupHook = startup
                         }

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = manageDocks <+> manageHook defaultConfig
                <+> composeAll [ isFullscreen                   --> doFullFloat
                               , className =? "Chromium"        --> doShift "2-web" 
                               , className =? "Google-chrome"   --> doShift "2-web" 
                               , className =? "spotify"         --> doShift "4-spotify"
                               , className =? "Mattermost"      --> doShift "5-chat"
                               , className =? "VirtualBox"      --> doFloat
                               , insertPosition Below Newer
                               , transience'
                               ]


-------------------------------------------------------------------------------
-- Looks --
-- bar
--
customPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
                     , ppHidden = xmobarColor "#C98F0A" ""
                     , ppHiddenNoWindows = xmobarColor "#C9A34E" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]" 
                     , ppLayout = xmobarColor "#C9A34E" ""
                     , ppTitle =  xmobarColor "#C9A34E" "" . shorten 80
                     , ppSep = xmobarColor "#429942" "" " | "
                     }


-------------------------------------------------------------------------------
-- GridSelect
myGSConfig = defaultGSConfig { gs_cellwidth = 160 }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 2
normalBorderColor'  = "#333333"
focusedBorderColor' = "#AFAF87"

-- tabs theme
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

-- workspaces
workspaces' = ["1-code", "2-web", "3-spotify", "4-chat", "5", "6", "7", "8", "9"]

-- layouts
layoutHook' = avoidStruts (tile ||| mtile ||| tab ||| full ||| rt)
    where
        rt = ResizableTall 1 (2/100) (1/2) []
        tile = renamed [Replace "[]="] $ smartBorders rt
        mtile = renamed [Replace "M[]="] $ smartBorders $ Mirror rt
        tab = renamed [Replace "T"] $ noBorders $ tabbed shrinkText tabTheme1
        full = renamed [Replace "[]"] $ noBorders Full 

-- startup Programs
startup :: X ()
startup = do
    spawn "/home/aren/Work/dot_files/setup_monitors.sh"
    spawnOn "1-code" "urxvt"
    spawnOn "2-web" "google-chrome-stable"
    setWMName "LG3D"

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "urxvt"

-- Dmenu --
cmd_dmenu = "dmenu_run -b"

-- Locking --
cmd_lock = "slock"

-- Multimedia --
cmd_volDown = "amixer set Master 2-"
cmd_volUp = "amixer set Master 2+"
cmd_spotifyPrevious = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous > /dev/null"
cmd_spotifyNext = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next > /dev/null" 
cmd_spotifyPlayPause = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause > /dev/null" 


-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- keys
--toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
--toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
      -- launching and killing programs
      [ ((modMask,               xK_Return), spawn (XMonad.terminal conf)) 
      , ((modMask,               xK_p     ), spawn cmd_dmenu) 

      -- utility
      , ((modMask .|. shiftMask, xK_z), spawn cmd_lock)
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0, xK_Print), spawn "scrot")
        
      -- multimedia
      , ((modMask .|. shiftMask, xK_Up),     spawn cmd_volUp)
      , ((modMask .|. shiftMask, xK_Down),   spawn cmd_volDown)
      , ((modMask .|. shiftMask, xK_Left),   spawn cmd_spotifyPrevious)
      , ((modMask .|. shiftMask, xK_Right),  spawn cmd_spotifyNext)
      , ((modMask .|. shiftMask, xK_p),      spawn cmd_spotifyPlayPause)
 
      -- grid
      , ((modMask,               xK_g     ), goToSelected myGSConfig)

      -- layouts
      , ((modMask,               xK_space ), sendMessage NextLayout)
      , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
      , ((modMask,               xK_b     ), sendMessage ToggleStruts)

      -- floating layer stuff
      , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

      -- refresh
      , ((modMask,               xK_n     ), refresh)

      -- focus
      , ((modMask,               xK_Tab   ), windows W.focusDown)
      , ((modMask,               xK_j     ), windows W.focusDown)
      , ((modMask,               xK_k     ), windows W.focusUp)
      , ((modMask,               xK_m     ), windows W.focusMaster)

      -- swapping
      , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
      , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
      , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

      -- increase or decrease number of windows in the master area
      , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
      , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

      -- resizing
      , ((modMask,               xK_h     ), sendMessage Shrink)
      , ((modMask,               xK_l     ), sendMessage Expand)
      , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
      , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

      -- quit, or restart
      , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
      , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
      ]
      ++
      -- mod-[1..9] %! Switch to workspace N
      -- mod-shift-[1..9] %! Move client to workspace N
      [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
      ++
      -- mod-[w,e] %! switch to twinview screen 1/2
      -- mod-shift-[w,e] %! move window to screen 1/2
      [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
