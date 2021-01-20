{-# LANGUAGE FlexibleContexts #-}
import           XMonad
import           XMonad.Config.Desktop
-- import           XMonad.Hooks.DynamicLog hiding (wrap)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers hiding (CW)
import           XMonad.Hooks.SetWMName
import           XMonad.Util.SpawnOnce (spawnOnce)
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           Graphics.X11.ExtraTypes.XF86


import Text.Regex.Posix ((=~))
import Control.Monad (forM_)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)

import           Data.List
import           System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as        M

-- Configuration variables
myWorkspaces :: [String]
myWorkspaces         = ["emacs","web","term","chat","games","music","logs","idea",""]
myTerminal :: String
-- myTerminal           = "termite"
-- myTerminal           = "alacritty"
myTerminal           = "kitty"
myTerminalTmux :: String
myTerminalTmux       = myTerminal ++ " -e tmux"
myModMask :: KeyMask
myModMask            = mod4Mask
altMask :: KeyMask
altMask = mod1Mask
ctrlMask :: KeyMask
ctrlMask = controlMask
myBorderWidth :: Dimension
myBorderWidth        = 3
myNormalBorderColor :: String
myNormalBorderColor  = powderBlue
myFocusedBorderColor :: String
myFocusedBorderColor = vaporPink
myBrowser :: String
myBrowser            = "firefox"
myEditor :: String
myEditor             = "emacs"

-- Brightness
brightnessCommand :: String -> String
brightnessCommand = ("/home/beltsmith/scripts/brightness_change " ++)

data Direction = Up | Down
monBrightnessChange :: Direction -> String
monBrightnessChange Up = brightnessCommand "up"
monBrightnessChange Down = brightnessCommand "down"

scrot :: String
scrot = "flameshot gui"

passSelect :: String
passSelect = "dmenu-lpass-nu"

-- Colours
winBlack    :: String
slateGrey   :: String
vaporPink   :: String
powderBlue  :: String
coolBlue    :: String
selected    :: String
barBg       :: String
urgentBlue  :: String
red         :: String
currWsFg    :: String
visibleWsFg :: String

urgentBlue = "#010081"
barBg      = "#C0C0C0" -- win95 taskbar
selected   = "#fffffe" -- win95 taskbar
coolBlue   = "#00AEB7"
powderBlue = "#9c9ba9" -- powder blue
vaporPink  = "#ff7faa" -- vapor pink
slateGrey  = "#2d2d2d" -- slate grey? yup
winBlack   = "#010303" -- Win95 font colour
red        = "#fb4934" -- not red? nope it's red

currWsFg    = vaporPink
visibleWsFg = powderBlue

-- Hookers

myLayoutHook = smartBorders . avoidStruts . toggleFull $ tiled ||| Mirror tiled ||| emptyBSP
  where
     toggleFull    = mkToggle $ NOBORDERS ?? FULL ?? EOT
     -- default tiling algorithm partitions the screen into two panes
     tiled   = ResizableTall nmaster delta ratio []
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 2/3
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myFloaterHook :: ManageHook
myFloaterHook = insertPosition End Newer

q ~? x = fmap(=~ x) q

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ isFullscreen                                --> (doF W.focusDown <+> doFullFloat) ]
    , [ role =? "pop-up" <&&>
        fmap ("Developer Tools" `isPrefixOf`) title --> doFloat                        ]
    , [ fmap ("Zoom Meeting" `isPrefixOf`) title    --> doFloat                        ]
    , [ className =? c                              --> doFloat  | c <- myClassFloats  ]
    , [ className =? c                              --> doIgnore | c <- myClassIgnores ]
    , [ className ~? c                              --> doIgnore | c <- myClassMasters ]
    , [ resource =? r                               --> doIgnore | r <- myResourceIgnores ]
    , [ isDialog                                    --> (doF W.shiftMaster <+> doF W.swapDown)]
    , [ resource =? "Closing"                       --> (doF W.shiftMaster <+> doF W.swapDown)]
    , [ role =? "toolbox"                           --> doF W.swapDown]
    ]
    where
        role           = stringProperty "WM_WINDOW_ROLE"
        myClassFloats  = ["Pinentry"] -- for gpg passphrase entry
        myClassIgnores = [] -- ["doomx64.exe", "DOOMx64"]
        myClassMasters = ["*- Doom Emacs", "emacs@*", "twitchui.exe", "battle.net.exe"]
        myResourceIgnores = myClassIgnores

-- myEventHook :: Event -> X All
myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook

-- spawnOnceAsync :: [Char] -> X ()
-- spawnOnceAsync = spawnOnce . (++ " &")

myStartupHook :: X ()
myStartupHook = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
  setWMName "LG3D"
  spawnOnce "xsetroot -cursor_name left_ptr"


polybarColor :: String -> String -> String
polybarColor code = wrap left right
  where left = wrap "%{F" "}" code
        right = "%{F-}"

eventLogHook :: X ()
eventLogHook = do
  winset <- gets windowset
  windowTitle <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let visible = W.visible winset
  let visibleWs = map (W.tag . W.workspace) visible
  let wss = map W.tag $ W.workspaces winset
  let wsStr = unwords $ map (fmt currWs visibleWs) $ sort' wss
  let titleStr = polybarColor coolBlue ">>=   " ++ windowTitle

  io $ appendFile "/tmp/.xmonad-title-log" (titleStr ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs visibleWs ws
          | currWs == ws = polybarColor currWsFg $ wrap "[" "]" ws
          | ws `elem` visibleWs = polybarColor visibleWsFg $ wrap "(" ")" ws
          | otherwise    = ws
        sort' = sortOn wsOrder
        find' = M.findWithDefault (-1)
        wsOrder k = find' k . M.fromList $ zip myWorkspaces [0..]

-- Main xmonad
main :: IO ()
main = xmonad . docks . ewmh $ desktopConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    , keys               = myKeys
    -- , logHook            = dynamicLogWithPP (myLogHook dbus) <+> logHook desktopConfig
    , logHook            = eventLogHook
    , manageHook         = myManageHook <+> myFloaterHook
    , layoutHook         = myLayoutHook
    , handleEventHook    = myEventHook
    , startupHook        = myStartupHook
    }

wrap :: String -> String -> String -> String
wrap h t = (h ++) . (++ t)

-- colourWrapper :: String -> String -> String -> String
-- colourWrapper kind colour = wrap startTag endTag
--   where
--     startTag = wrap "%{" "} " $ kind ++ colour
--     endTag   = wrap " %{" "-}" kind

-- wsPP :: String -> String -> WorkspaceId -> String
-- wsPP bg fg wsId = bgWrapper $ fgWrapper wsId
--   where
--     bgWrapper = colourWrapper "B" bg
--     fgWrapper = colourWrapper "F" fg


-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
-- myLogHook :: D.Client -> PP
-- myLogHook dbus = def
--     { ppOutput  = dbusOutput dbus
--     , ppCurrent = wsPP bgCurrent fgCurrent
--     , ppVisible = wsPP bg fgVisible
--     , ppUrgent  = wsPP bg fgUrgent
--     , ppHidden  = wsPP bg fgHidden
--     , ppWsSep   = ""
--     , ppSep     = " : "
--     , ppTitle   = shorten 40
--     }
--   where
--     fg        = winBlack
--     bg        = barBg
--     fgHidden  = fg
--     fgCurrent = bg
--     fgUrgent  = urgentBlue
--     fgVisible = vaporPink
--     bgCurrent = selected



-- -- Emit a DBus signal on log updates
-- dbusOutput :: D.Client -> String -> IO ()
-- dbusOutput dbus str = do
--     let signal = (D.signal objectPath interfaceName memberName) {
--             D.signalBody = [D.toVariant $ UTF8.decodeString str]
--         }
--     D.emit dbus signal
--   where
--     objectPath    = D.objectPath_ "/org/xmonad/Log"
--     interfaceName = D.interfaceName_ dbusInterface
--     memberName    = D.memberName_ "Update"

-- Rofi
rofiConfig :: String
rofiConfig =
  foldl configify "" configList
  where
    toConfig (k, v) = " -" ++ k ++ " " ++ v
    configify       = \cnf t -> cnf ++ toConfig t
    configList      = [ ("case-sensitive", "")
                      , ("match"         , "fuzzy")
                      , ("theme"         , "Arc-Dark")
                      , ("show-icons"    , "")]

rofi :: String -> String
rofi = ("rofi -show " ++) . (++ rofiConfig)

rofiRun :: String -> String
rofiRun = ("rofi -dmenu " ++) . (++ rofiConfig)

rofiCmd :: String
rofiCmd = rofiRun ""

switchMonitor :: String
switchMonitor = "/home/beltsmith/.screenlayout/$(ls /home/beltsmith/.screenlayout | " ++ rofiCmd ++ ")"

switchWifi :: String
switchWifi = "netctl switch-to $(netctl list | " ++ rofiCmd ++ " | cut -f 2-3 -d '')"

-- Utility functions

shOr :: String -> String -> String
shOr l r = wrap l r " || "

myRecompileCmd :: String
myRecompileCmd = compileCmd `shOr` failureCmd
  where
    compileCommands = [ "xmonad --recompile"
                      , "notify-send 'reloaded xmonad'"
                      , "xmonad --restart"
                      ]
    failureCommands = [ "notify-send 'Failed to reload xmonad' --urgency=critical"
                      , "notify-send <(cat /tmp/xmonad-compile.log) --urgency=critical"
                      ]
    comandify       = wrap "(" ")" . intercalate " && "
    failureCmd      = comandify failureCommands
    compileCmd      = comandify compileCommands

-- I stole these from xmonad

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    -- launch a terminal
    [ ((modm .|. ctrlMask,  xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_Return), spawn myTerminalTmux)
    -- launch a browser
    , ((modm, xK_b), spawn myBrowser)
    , ((modm              , xK_e     ), spawn myEditor)
    -- close focused window
    , ((modm              , xK_q     ), kill)
    , ((modm .|. shiftMask, xK_c     ), kill) -- allow me to kill shit on osx
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- rofi launch
    , ((modm,               xK_space ), spawn $ rofi "run")
    , ((modm,               xK_w     ), spawn $ rofi "window")
    , ((modm,               xK_p     ), spawn passSelect)
    , ((0, xF86XK_Display            ), spawn switchMonitor)
    , ((0, xF86XK_Tools              ), spawn switchWifi)
    -- , ((mod4Mask,           xK_e     ), spawn $ rofi "run")
    -- rofi switch
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_Tab   ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_Tab   ), sendMessage $ Toggle FULL)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Shrink the master area
    , ((modm .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    -- Expand the master area
    , ((modm .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Fullscreen that shit motherfucker
    -- , ((modm .|. shiftMask, xK_f     ), setLayout fullscreenFull)

    -- Increment the number of windows in the master area
    , ((modm .|. shiftMask, xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm .|. shiftMask, xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r     ), spawn myRecompileCmd)
    ]

    ++

    -- BSP keybinds

    [ ((modm .|. altMask,               xK_l     ), sendMessage $ ExpandTowards R)
    , ((modm .|. altMask,               xK_h     ), sendMessage $ ExpandTowards L)
    , ((modm .|. altMask,               xK_j     ), sendMessage $ ExpandTowards D)
    , ((modm .|. altMask,               xK_k     ), sendMessage $ ExpandTowards U)
    , ((modm .|. altMask .|. ctrlMask , xK_l     ), sendMessage $ ShrinkFrom R)
    , ((modm .|. altMask .|. ctrlMask , xK_h     ), sendMessage $ ShrinkFrom L)
    , ((modm .|. altMask .|. ctrlMask , xK_j     ), sendMessage $ ShrinkFrom D)
    , ((modm .|. altMask .|. ctrlMask , xK_k     ), sendMessage $ ShrinkFrom U)
    , ((modm,                           xK_r     ), sendMessage Rotate)
    , ((modm,                           xK_s     ), sendMessage Swap)
    , ((modm,                           xK_n     ), sendMessage FocusParent)
    , ((modm .|. ctrlMask,              xK_n     ), sendMessage SelectNode)
    , ((modm .|. shiftMask,             xK_n     ), sendMessage MoveNode)
    ]

    ++
-- <XF86MonBrightnessUp>
-- <XF86MonBrightnessDown>
    [ ((0, xF86XK_MonBrightnessUp  ), spawn $ monBrightnessChange Up)
    , ((0, xF86XK_MonBrightnessDown), spawn $ monBrightnessChange Down)
    , ((0, xK_Print                ), spawn scrot)
    ]

    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- --
    -- -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- --
    [((m .|. modm , key), screenWorkspace screen >>= flip whenJust (windows . f))
        | (key, screen) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, controlMask), (W.shift, controlMask .|. shiftMask)]]
