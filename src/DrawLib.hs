-- {-# LANGUAGE OverlappingInstances, FlexibleInstances #-}

module DrawLib where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.List (intercalate)


data Pixel a = Pixel Char a
type Coord = (Int, Int)
type Image a = Array Coord (Maybe (Pixel a))

instance {-# OVERLAPPING #-} Show (Pixel a) => Show (Image a) where
    show image = intercalate "\n" (map (concatMap showMPixel) charLists)
        where showMPixel :: Maybe (Pixel a) -> String
              showMPixel Nothing = " "
              showMPixel (Just px) = show px
              charLists = array2dToList image

array2dToList :: Array Coord a -> [[a]]
array2dToList arr = 
    let ((x1, y1), (x2, y2)) = A.bounds arr
     in do
            x <- [x1..x2]
            return $ do
                y <- [y1..y2]
                return $ arr ! (x, y)

convertToImage :: [[Char]] -> a -> Image a
-- convertToImage [] _ = error "empty list in image parsing"
convertToImage l color = A.listArray bound $ map toPixel (concat l)
    where numRows  = length l
          numCols  = if all ((ncol==) . length) l
                        then ncol
                        else error "non-rectangular image passed"
          ncol = case l of
                   []   -> 0
                   hl:_ -> length hl
          bound = ((0, 0), (numRows - 1, numCols - 1))
          toPixel ' ' = Nothing
          toPixel c = Just $ Pixel c color

getPixel :: Coord -> Image a -> Maybe (Pixel a)
getPixel coord arr
  | A.inRange (A.bounds arr) coord = arr ! coord
  | otherwise                  = Nothing

(^-^) :: Coord -> Coord -> Coord
(x1, y1) ^-^ (x2, y2) = (x1 - x2, y1 - y2)

(^+^) :: Coord -> Coord -> Coord
(x1, y1) ^+^ (x2, y2) = (x1 + x2, y1 + y2)

shift :: Coord -> Image a -> Image a
shift coord image = A.listArray (ul ^+^ coord, dr ^+^ coord) (A.elems image)
    where (ul, dr) = A.bounds image

overlay :: Image a -> Image a -> Image a
overlay src tar = A.array bnd [(i, getValueAt i) | i <- A.range bnd]
    where bnd = ((min xl1 xl2, min yl1 yl2), (max xr1 xr2, max yr1 yr2))
          ((xl1, yl1), (xr1, yr1)) = A.bounds src
          ((xl2, yl2), (xr2, yr2)) = A.bounds tar
          getValueAt i = case (getPixel i src, getPixel i tar) of
                           (Just v, _) -> Just v
                           (_, Just v) -> Just v
                           _           -> Nothing


