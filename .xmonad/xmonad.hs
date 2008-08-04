import XMonad
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

dmenuExec = "exe=`dmenu_path | dmenu -b -nb black -nf white -sb gray -sf black` && eval \"exec $exe\""

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_p     ), spawn dmenuExec)
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_n     ), refresh)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_Right ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask,               xK_Left  ), windows W.focusUp  )
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_Right ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modMask .|. shiftMask, xK_Left  ), windows W.swapUp    )
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask              , xK_b     ),
          modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i
                             in if n == x then (0,0,0,0) else x))
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
 
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myLayout = avoidStruts (tall ||| Mirror tall ||| Full)
  where
     tall   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ] <+> manageDocks

myLogHook h = dynamicLogWithPP $ defaultPP {
              ppCurrent         = dzenColor "black" "white" . pad
            , ppVisible         = dzenColor "white" "black" . pad
            , ppHidden          = dzenColor "white" "black" . pad
            , ppHiddenNoWindows = dzenColor "#777777" "black" . pad
            , ppUrgent          = dzenColor "red" "yellow"
            , ppWsSep           = ""
            , ppSep             = dzenColor "white" "black" (fill "^r(3x3)" 4)
            , ppLayout          = dzenColor "white" "black" .
                                  (\ x -> fill (case x of
                                                  "Tall"               -> icon "tall.xbm"
                                                  "Mirror Tall"        -> icon "mtall.xbm"
                                                  "Full"               -> icon "full.xbm"
                                                  _                    -> pad x) 4)
            , ppTitle           = ("^fg(white) " ++) . dzenEscape
            , ppOutput          = hPutStrLn h
            }
    where
      icon h = "^i(/home/djoyner/.xmonad/dzen_bitmaps/" ++ h ++ ")"
      fill :: String -> Int -> String
      fill h i = "^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++ ")"

myStatusBar = "dzen2 -bg 'black' -fg 'white' -h 16 -w 1280 -ta l"

myWorkspaces = ["1:main", "2:perforce", "3:web"] ++ map show [4..9]

main = do din <- spawnPipe myStatusBar
          xmonad $ defaultConfig { 
          terminal           = "xterm",
          focusFollowsMouse  = True,
          borderWidth        = 1,
          modMask            = mod1Mask,
          numlockMask        = mod2Mask,
          workspaces         = myWorkspaces,
          normalBorderColor  = "#a0a0a0",
          focusedBorderColor = "#ff0000",
          keys               = myKeys,
          mouseBindings      = myMouseBindings,
          layoutHook         = myLayout,
          manageHook         = myManageHook,
          logHook            = myLogHook din
          }
