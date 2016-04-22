import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


cmd_dmenu = "dmenu_run -b"
cmd_lock = "slock"
cmd_volDown = "amixer set Master 2-"
cmd_volUp = "amixer set Master 2+"
cmd_spotifyPrevious = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous > /dev/null"
cmd_spotifyNext = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next > /dev/null" 
cmd_spotifyPlayPause = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause > /dev/null" 

main = do
    xmproc <- spawnPipe "/home/aren/.cabal/bin/xmobar /home/aren/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn cmd_lock)
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask,               xK_p), spawn cmd_dmenu)
        , ((mod4Mask .|. shiftMask, xK_Up), spawn cmd_volUp)
        , ((mod4Mask .|. shiftMask, xK_Down), spawn cmd_volDown)
        , ((mod4Mask .|. shiftMask, xK_Left), spawn cmd_spotifyPrevious)
        , ((mod4Mask .|. shiftMask, xK_Right), spawn cmd_spotifyNext)
        , ((mod4Mask .|. shiftMask, xK_p), spawn cmd_spotifyPlayPause)
        ]
