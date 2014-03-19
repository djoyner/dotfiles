import qualified Data.Map as M
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

main=xmonad $ gnomeConfig
  { normalBorderColor = "#222222"
  , focusedBorderColor = "#ef2929"
  , layoutHook = myLayout
  , manageHook = myManageHook
  , modMask = myModMask
  , keys = myKeys
  }

-- custom layout
myLayout = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = toRational (2 / (1 + sqrt(5) :: Double))
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- custom window management
myManageHook = composeAll
  [
    (className =? "Gnome-panel" <&&> title =? "Run Application") --> doCenterFloat
  ] <+> manageHook gnomeConfig

-- use the Windows key as the mod key
myModMask = mod4Mask

-- custom key bindings
myKeys conf@(XConfig {modMask = modMask}) = M.fromList $
  [
    -- launching and killing programs 
    ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)        -- launch terminal
  , ((modMask,               xK_p     ), gnomeRun)                            -- launch program
  , ((modMask .|. shiftMask, xK_c     ), kill)                                -- close the focused window
  , ((modMask,               xK_space ), sendMessage NextLayout)              -- rotate through the available layout algorithms
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)  -- reset the layouts on the current workspace to default
  , ((modMask,               xK_n     ), refresh)                             -- resize viewed windows to the correct size

  -- move focus up or down the window stack
  , ((modMask,               xK_Tab   ), windows W.focusDown)                 -- move focus to the next window
  , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)                   -- move focus to the previous window
  , ((modMask,               xK_j     ), windows W.focusDown)                 -- move focus to the next window
  , ((modMask,               xK_k     ), windows W.focusUp)                   -- move focus to the previous window
  , ((modMask,               xK_m     ), windows W.focusMaster)               -- move focus to the master window

  -- modifying the window order
  , ((modMask,               xK_Return), windows W.swapMaster)                -- swap the focused window and the master window
  , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)                  -- swap the focused window with the next window
  , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)                    -- swap the focused window with the previous window

  -- resizing the master/slave ratio
  , ((modMask,               xK_h     ), sendMessage Shrink)                  -- shrink the master area
  , ((modMask,               xK_l     ), sendMessage Expand)                  -- expand the master area

  -- floating layer support
  , ((modMask,               xK_t     ), withFocused $ windows . W.sink)      -- push window back into tiling

  -- dock support
  , ((modMask              , xK_b     ), sendMessage ToggleStruts)            -- toggle docks and status bars

  -- increase or decrease number of windows in the master area
  , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))          -- increment the number of windows in the master area
  , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))       -- deincrement the number of windows in the master area

  -- quit, restart, and lock
  , ((modMask .|. shiftMask, xK_q     ), spawnLogoutDialog)                   -- quit XMonad
  , ((modMask              , xK_q     ), spawnRecompileRestart)               -- restart XMonad
  , ((modMask .|. shiftMask, xK_l     ), spawnLockScreen)                     -- lock screen
  ]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

spawnLogoutDialog = spawn "gnome-session-quit --logout"
spawnRecompileRestart = spawn "xmonad --recompile && xmonad --restart"
spawnLockScreen = spawn "gnome-screensaver-command -l"

