module Face where
import Data.Word
import Data.Bits

-- Defines the colours of the cube where each colour from White to Yellow
-- has a value from 0 to 5 respectively
data Colour = W | G | R | B | O | Y
  deriving (Eq, Enum, Bounded, Show)

-- Converts a colour to its corresponding value
colourToValue :: Colour -> Word8
colourToValue colour = fromIntegral $ fromEnum colour

-- Converts a value to its corresponding colour
valueToColour :: Word8 -> Colour
valueToColour value = toEnum $ fromIntegral value

-- Defines the Face structure with the centre colour and the facelets
data Face = Face
    { centreFacelet :: Colour
    , facelets :: Word64
    } deriving (Eq)

-- Defines the Show instance for the Face structure
instance Show Face where
  show (Face centreFacelet facelets) = show centreFacelet ++ " " ++ show facelets

-- Takes in an index, a colour value, and a Face and returns a new Face with
-- the facelet at the given index set to the given colour. It wont allow you to set
-- the centre colour.
setFacelet ::  Int -> Word8 -> Face -> Face
setFacelet index newValue (Face centreFacelet facelets)
  | index >= 0 && index <= 7 = Face centreFacelet updatedFacelets
  | otherwise = error "Invalid facelet index"
  where
    -- Clear the bits at the position of the facelet
    clearedFacelets = facelets .&. complement (0xFF `shiftL` (8 * (7 - index)))
    -- Set the bits with the new value
    updatedFacelets = clearedFacelets .|. (fromIntegral newValue `shiftL` (8 * (7 - index)))

-- Takes in a Face and an index and returns the facelet at the given index
getFacelet :: Face -> Int -> Word8
getFacelet (Face centreFacelet facelets) index
  | index >= 0 && index <= 7 = fromIntegral (facelets `shiftR` (8 * (7 - index))) .&. 0xFF
  | index == 8 = colourToValue centreFacelet
  | otherwise = error "Invalid facelet index"

-- Takes in a Face and an index and returns the colour of the facelet at the given index
getFaceletAsColour :: Face -> Int -> Colour
getFaceletAsColour face index = valueToColour $ fromIntegral (getFacelet face index)

-- showRow takes in a Face and an index and returns a string representation of the row
-- with ANSI codes to color the letters according to the colour of the facelet
showRow :: Face -> Int -> String
showRow face index
  | index < 0 || index > 2 = error "Invalid row"
  | otherwise = unwords [showColoredLetter $ getFaceletAsColour face i | i <- rowIndices]
    where
      rowIndices
        | index == 0 = [0, 1, 2]
        | index == 1 = [7, 8, 3]
        | otherwise = [6, 5 ,4]

-- Takes in a Face and an index and returns a string representation of the row
-- without ANSI codes.
showRowWithoutANSICodes :: Face -> Int -> String
showRowWithoutANSICodes face index
  | index < 0 || index > 2 = error "Invalid row"
  | otherwise = unwords [show $ getFaceletAsColour face i | i <- rowIndices]
    where
      rowIndices
        | index == 0 = [0, 1, 2]
        | index == 1 = [7, 8, 3]
        | otherwise = [6, 5 ,4]

-- Takes in a Face and returns a new Face with the facelets rotated clockwise
rotateClockwise :: Face -> Face
rotateClockwise (Face centreFacelet facelets) = Face centreFacelet (shiftL facelets 48 .|. shiftR facelets 16)

-- myFace is a Face with the centre colour set to Red and all the other facelets set to red as well
myFace :: Face
myFace = Face {centreFacelet = R, facelets = 0b0000000000000001000000100000001100000100000001010000000000000001}

-- Takes in a Colour and returns a string representation of the letter with ANSI codes
-- to color the letter according to the colour
showColoredLetter :: Colour -> String
showColoredLetter W = "\x1b[1;30mW\x1b[0m"  -- White
showColoredLetter G = "\x1b[1;32mG\x1b[0m"  -- Green
showColoredLetter R = "\x1b[1;31mR\x1b[0m"  -- Red
showColoredLetter B = "\x1b[1;34mB\x1b[0m"  -- Blue
showColoredLetter O = "\x1b[1;38;5;166mO\x1b[0m"  -- Orange
showColoredLetter Y = "\x1b[1;33mY\x1b[0m"  -- Yellow

{--
Note:
I have indexed these facelets by the order in which they appear in the 64-bit integer.
The left most 8-bit chunk corresponds to index 0, the next 8-bit corresponds to index 1,
and so on till index 7.The left most 8-bit chunk corresponds also represents the top left 
facelet, the next 8-bit chunk represents the top center facelet (here it is G) and so on 
in a clockwise direction; the last 8-bit chunk represents the middle left facelet. 
Index 8 is reserved for the center facelet.
--}