module Exercise2 where
import Pictures

black = invertColour white

-- make a cell with any Char
makeCell :: Char -> Picture
makeCell s =
  replicate 6 $ replicate 12 s

-- define black without using white
myWhite = makeCell '.'
myBlack = makeCell '#'


-- make block of white and black cells
fstTryPic = above
         (sideBySide white black)
         (sideBySide black white)

sndTryPic =
  let a = sideBySide white black
      b = invertColour a
  in above a b


-- Generate chessboard 8x8
genChess :: Picture -> Picture -> Picture
genChess wh bl =
  let a = sideBySide wh bl
      b = invertColour a
  in helpBoard . helpBoard $ above a b

-- helper function for chess gen
helpBoard :: Picture -> Picture
helpBoard block =
  let a = sideBySide block block
  in above a a


-- some images of horses
genHorseBlock :: (Picture -> Picture) -> Picture
genHorseBlock f =
  let a = sideBySide horse $ invertColour horse
      b = f a
  in above a b

-- First invertColour
-- Second flipV
-- Third rotate
-- Forth (flipH . invertColour $)
