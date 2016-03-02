import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


cmd_lock = "slock"
cmd_volDown = "amixer set Master 2-"
cmd_volUp = "amixer set Master 2+"
cmd_volMute = "amixer set Master toggle"

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
        , ((mod4Mask .|. shiftMask, xK_Up), spawn cmd_volUp)
        , ((mod4Mask .|. shiftMask, xK_Down), spawn cmd_volDown)
        , ((mod4Mask .|. shiftMask, xk_Left), spawn cmd_volMute)
        ]
