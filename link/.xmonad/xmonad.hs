import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Actions.PhysicalScreens
import XMonad.Actions.FocusNth
import XMonad.Layout.Tabbed
import XMonad.Layout.Column
import XMonad.Layout.ResizableTile
import XMonad.Actions.TopicSpace
import XMonad.Layout.PerWorkspace

import qualified Data.Map as M

myTopics :: [Topic]
myTopics =
  [
    "dashboard1",
    "dashboard2",
    "web",
    "file",
    "term"
  ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig {
  topicActions = M.fromList $ [
    ("web", spawn "chromium-browser"),
    ("file", spawn "pcmanfm"),
    ("term", sequence_ [spawn "lxterminal", spawn "lxterminal", spawn "lxterminal"] )
  ]
}

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

main = do
 -- checkTopicConfig myTopics myTopicConfig
  xmonad $ defaultConfig {
    workspaces = myTopics,
    manageHook = manageDocks <+> manageHook defaultConfig,
    modMask = mod4Mask,     -- Rebind Mod to the Windows key
    layoutHook =
      onWorkspace "web" (
        simpleTabbed
        ||| Mirror(Column 1)
        ||| ResizableTall 1 (3/100) (1/2) []
        ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
      ) $
      onWorkspace "term" (
        (Mirror(Column 1))
        ||| Mirror (ResizableTall 3 (3/100) (1/2) [])
      ) $
      onWorkspace "file" (Mirror (ResizableTall 1 (3/100) (1/2) [])) $ 
      simpleTabbed
      ||| Mirror(Column 1)
      ||| ResizableTall 1 (3/100) (1/2) []
      ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
  }
   `additionalKeys`
   [
    ((0, xK_F1), sequence_ [ viewScreen 0, focusNth 0 ]),
    ((0, xK_F2), sequence_ [ viewScreen 0, focusNth 1 ]),
    ((0, xK_F3), sequence_ [ viewScreen 0, focusNth 2 ]),
    ((0, xK_F4), sequence_ [ viewScreen 0, focusNth 3 ]),
    ((0, xK_F5), sequence_ [ viewScreen 1, focusNth 0 ]),
    ((0, xK_F6), sequence_ [ viewScreen 1, focusNth 1 ]),
    ((0, xK_F7), sequence_ [ viewScreen 1, focusNth 2 ]),
    ((0, xK_F8), sequence_ [ viewScreen 1, focusNth 3 ]),
    --((mod1Mask .|. controlMask, xK_t), spawn $ XMonad.terminal (XConfig {XMonad.modMask = mod1Mask})),
    ((mod1Mask .|. controlMask, xK_t), spawn "lxterminal"),
    ((mod4Mask, xK_t), goto "term" ),
    --((mod1Mask ,xK_c), spawn $ XMonad.terminal conf),
    ((mod1Mask .|. controlMask, xK_b), spawn "chromium-browser"),
    ((mod4Mask, xK_w), goto "web" ),
    ((mod1Mask .|. controlMask, xK_w), spawn "pcmanfm"),
    ((mod4Mask, xK_f), goto "file" )
    --((mod1Mask, xK_a), currentTopicAction myTopicConfig)
    --((mod1Mask, xK_b), moveTo "web")
    --((modMask, xK_b), windows $ W.greedyView "1")
    --((shiftMask .|. modm, xK_b), windows $ W.shift "1")
   ]
