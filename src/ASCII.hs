module ASCII where 

import DrawLib
import RaceTrack
import Color
import Ticket
import Text.Printf (printf)
import Data.Array (assocs)
-- import Data.Array

crazyCamelString :: [[Char]]
crazyCamelString = [ "▀█ ▅   "
                   , " ▀███▋ "
                   , "  █ █  "
                   ]

camelString :: [[Char]]
camelString =[ "   ▅ █▀"
             , " ▐███▀ "
             , "  █ █  "
             ]


tileString :: [[Char]]
tileString =  [ "/=====\\"
              ]

tileStringLeft :: [[Char]]
tileStringLeft = [ "<<<<<<<"
                 ]

tileStringRight :: [[Char]]
tileStringRight = [ ">>>>>>>"
                  ]

card :: Int -> [[Char]]
card val = [ printf "<━━━(%d)━━━>" val
           ]

type OutputColor = Maybe Camel

instance Show (Pixel OutputColor) where
    show (Pixel c color) = getCode color ++ [c] ++ getCode Nothing

getCode :: OutputColor -> String
getCode Nothing = "\ESC[0m"
getCode (Just camel) = getCodeCamel camel

getCodeCamel :: Camel -> String 
getCodeCamel (Normal nColor) = getCodeNormal nColor
getCodeCamel (Crazy cColor) = getCodeCrazy cColor

emptyCanvas :: Image OutputColor
emptyCanvas = convertToImage [] Nothing

showTile :: Maybe TileStatus -> Image OutputColor
showTile Nothing = shift (11, 0) $ convertToImage tileString Nothing
showTile (Just (HasCamel camels)) = foldl tag (showTile Nothing) $ zip [8, 6, 4, 2, 0] camels
    where tag image (x, camel) = overlay image $ shift (x, 0) $ convertToImage thisCamelString (Just camel)
            where thisCamelString = case camel of
                                      Normal _ -> camelString
                                      Crazy  _ -> crazyCamelString

showTile (Just (HasSpectator spectator _)) = overlay (showTile Nothing) $ shift (10, 0) $ convertToImage specImage Nothing
    where specImage = case spectator of
                        Push -> tileStringRight
                        Pull -> tileStringLeft

trackToImage :: RaceTrack -> Image OutputColor
trackToImage trackMap = foldr (tileToLeft . showTile) emptyCanvas (trackToList trackMap)
    where tileToLeft :: Image a -> Image a -> Image a
          tileToLeft tile canvas = overlay tile (shift (0, 7) canvas)

showDeck :: Color -> [Int] -> Image OutputColor
showDeck normalColor = overlayAll . zipWith tag [4,3..] . reverse
    where color = Just (Normal normalColor)
          tag :: Int -> Int -> Image OutputColor
          tag x val = shift (x, 0) $ convertToImage (card val) color

overlayAll :: [Image OutputColor] -> Image OutputColor
overlayAll = foldr overlay emptyCanvas 

ticketHallToImage :: TicketHall -> Image OutputColor
ticketHallToImage = foldr (deckToLeft . uncurry showDeck) emptyCanvas . assocs
    where deckToLeft :: Image a -> Image a -> Image a
          deckToLeft deck canvas = overlay deck (shift (0, 11) canvas)

-- 👑
-- ▀█ ▅
--  ▀███▋ 
--   █ █ 
-- ------

-- cursorUpLine 10000
-- draw the shit
-- prompt for move

-- ▀█ ▅
--  ▀███▋ 
-- ▀██▅█
--  ▀███▋ 
--   █ █ 

-- ╭──╮╭─╮
-- ╰─╮╰╯ ╰─╮
--   ╰╮╭─╮╭╯
--    ╰╯ ╰╯
-- ╭──┐┌─┐
-- ╰─┐└┘ └─┐
--   └┐┌─┐┌┘
--    └┘ └┘

-- ╭──┐┌─┐
-- ╰─┐└┘ └─┐
-- ╭─┴┐┌─┐┌┘
-- ╰─┐└┘ └┴┐
--   └┐┌─┐┌┘
--    └┘ └┘

-- U+2580 	▀ 	Upper half block
-- U+2581 	▁ 	Lower one eighth block
-- U+2582 	▂ 	Lower one quarter block
-- U+2583 	▃ 	Lower three eighths block
-- U+2584 	▄ 	Lower half block
-- U+2585 	▅ 	Lower five eighths block
-- U+2586 	▆ 	Lower three quarters block
-- U+2587 	▇ 	Lower seven eighths block
-- U+2588 	█ 	Full block
-- U+2589 	▉ 	Left seven eighths block
-- U+258A 	▊ 	Left three quarters block
-- U+258B 	▋ 	Left five eighths block
-- U+258C 	▌ 	Left half block
-- U+258D 	▍ 	Left three eighths block
-- U+258E 	▎ 	Left one quarter block
-- U+258F 	▏ 	Left one eighth block
-- U+2590 	▐ 	Right half block
-- U+2591 	░ 	Light shade
-- U+2592 	▒ 	Medium shade
-- U+2593 	▓ 	Dark shade
-- U+2594 	▔ 	Upper one eighth block
-- U+2595 	▕ 	Right one eighth block
-- U+2596 	▖ 	Quadrant lower left
-- U+2597 	▗ 	Quadrant lower right
-- U+2598 	▘ 	Quadrant upper left
-- U+2599 	▙ 	Quadrant upper left and lower left and lower right
-- U+259A 	▚ 	Quadrant upper left and lower right
-- U+259B 	▛ 	Quadrant upper left and upper right and lower left
-- U+259C 	▜ 	Quadrant upper left and upper right and lower right
-- U+259D 	▝ 	Quadrant upper right
-- U+259E 	▞ 	Quadrant upper right and lower left
-- U+259F 	▟ 	Quadrant upper right and lower left and lower right 

--  	0 	1 	2 	3 	4 	5 	6 	7 	8 	9 	A 	B 	C 	D 	E 	F
-- U+250x 	─ 	━ 	│ 	┃ 	┄ 	┅ 	┆ 	┇ 	┈ 	┉ 	┊ 	┋ 	┌ 	┍ 	┎ 	┏
-- U+251x 	┐ 	┑ 	┒ 	┓ 	└ 	┕ 	┖ 	┗ 	┘ 	┙ 	┚ 	┛ 	├ 	┝ 	┞ 	┟
-- U+252x 	┠ 	┡ 	┢ 	┣ 	┤ 	┥ 	┦ 	┧ 	┨ 	┩ 	┪ 	┫ 	┬ 	┭ 	┮ 	┯
-- U+253x 	┰ 	┱ 	┲ 	┳ 	┴ 	┵ 	┶ 	┷ 	┸ 	┹ 	┺ 	┻ 	┼ 	┽ 	┾ 	┿
-- U+254x 	╀ 	╁ 	╂ 	╃ 	╄ 	╅ 	╆ 	╇ 	╈ 	╉ 	╊ 	╋ 	╌ 	╍ 	╎ 	╏
-- U+255x 	═ 	║ 	╒ 	╓ 	╔ 	╕ 	╖ 	╗ 	╘ 	╙ 	╚ 	╛ 	╜ 	╝ 	╞ 	╟
-- U+256x 	╠ 	╡ 	╢ 	╣ 	╤ 	╥ 	╦ 	╧ 	╨ 	╩ 	╪ 	╫ 	╬ 	╭ 	╮ 	╯
-- U+257x 	╰ 	╱ 	╲ 	╳ 	╴ 	╵ 	╶ 	╷ 	╸ 	╹ 	╺ 	╻ 	╼ 	╽ 	╾ 	╿

