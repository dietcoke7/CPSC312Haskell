{-- cleanup.hs includes correct code that has been refactored out for making the project more concise. --}

-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
-- ********************************************************* START FaceModule.hs ********************************************************* --
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
{--
from FaceModule.hs:

data Colour = W | G | R | B | O | Y
  deriving (Eq)

instance Show Colour where
  show W = "W"
  show G = "G"
  show R = "R"
  show B = "B"
  show O = "O"
  show Y = "Y"

colourToValue :: Colour -> Word8
colourToValue colour
  | colour == W = 0b00000000
  | colour == G = 0b00000001
  | colour == R = 0b00000010
  | colour == B = 0b00000011
  | colour == O = 0b00000100
  | colour == Y = 0b00000101

valueToColour :: Word8 -> Colour
valueToColour value
  | value == 0b00000000 = W
  | value == 0b00000001 = G
  | value == 0b00000010 = R
  | value == 0b00000011 = B
  | value == 0b00000100 = O
  | value == 0b00000101 = Y
  | otherwise = error "Invalid value"

setFacelet :: Face -> Int -> Word8 -> Face
setFacelet (Face centreColour facelets) index newValue
  | index >= 0 && index <= 7 = Face centreColour updatedFacelets
  | otherwise = error "Invalid facelet index"
  where
    -- Clear the bits at the position of the facelet
    clearedFacelets = facelets .&. complement (0xFF `shiftL` (8 * (7 - index)))
    -- Set the bits with the new value
    updatedFacelets = clearedFacelets .|. (fromIntegral newValue `shiftL` (8 * (7 - index)))
-- wont allow you to set the centre colour

showRow :: Face -> Int -> String
showRow face index
  | index == 0 = show (getFaceletAsColour face 0) ++ " " ++ show (getFaceletAsColour face 1) ++ " " ++ show (getFaceletAsColour face 2) 
  | index == 1 = show (getFaceletAsColour face 7) ++ " " ++ show (centreColour face) ++ " " ++ show (getFaceletAsColour face 3)
  | index == 2 = show (getFaceletAsColour face 6) ++ " " ++ show (getFaceletAsColour face 5) ++ " " ++ show (getFaceletAsColour face 4) 
  | otherwise = error "Invalid row"


showRow :: Face -> Int -> String
showRow face index
  | index > 0 || index < 2 = error "Invalid row"
  | otherwise = unwords [show $ getFaceletAsColour face i | i <- rowIndices]
    where
      rowIndices
        | index == 0 = [0, 1, 2]
        | index == 1 = [7, 8, 3]
        | otherwise = [6, 5 ,4]

instance Show Face where
  show (Face centreColour facelets) =
    showRow (Face centreColour facelets) 0 ++ "\n" ++
    showRow (Face centreColour facelets) 1 ++ "\n" ++
    showRow (Face centreColour facelets) 2 ++ "\n"

instance Show Face where
  show (Face centreColour facelets) = unwords [ showRow (Face centreColour facelets) i ++ "\n" | i <- [0, 1, 2]]

rotateAntiClockwise :: Face -> Face
rotateAntiClockwise (Face centreColour facelets) = Face centreColour (shiftL facelets 16 .|. shiftR facelets 48)
--}
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
-- ********************************************************** END FaceModule.hs ********************************************************** --
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --



-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
-- ********************************************************* START CubeModule.hs ********************************************************* --
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
{--
from CubeModule.hs:

instance Show Cube where
  show (Cube uFace dFace rFace lFace fFace bFace) =
    "        +-------+ \n" ++
    "        | " ++ showRow uFace 0 ++ " | \n" ++
    "        | " ++ showRow uFace 1 ++ " | \n" ++
    "        | " ++ showRow uFace 2 ++ " | \n" ++
    "+-------+-------+-------+-------+\n" ++
    "| " ++ showRow lFace 0 ++ " | " ++ showRow fFace 0 ++ " | " ++ showRow rFace 0 ++ " | " ++ showRow bFace 0 ++ " | \n" ++
    "| " ++ showRow lFace 1 ++ " | " ++ showRow fFace 1 ++ " | " ++ showRow rFace 1 ++ " | " ++ showRow bFace 1 ++ " | \n" ++
    "| " ++ showRow lFace 2 ++ " | " ++ showRow fFace 2 ++ " | " ++ showRow rFace 2 ++ " | " ++ showRow bFace 2 ++ " | \n" ++
     "+-------+-------+-------+-------+\n" ++
    "        | " ++ showRow dFace 0 ++ " | \n" ++
    "        | " ++ showRow dFace 1 ++ " | \n" ++
    "        | " ++ showRow dFace 2 ++ " | \n" ++
    "        +-------+ "



(uFace defaultCube) & setFacelet2 4 (getFacelet (lFace defaultCube) 2) & setFacelet2 5 (getFacelet (lFace defaultCube) 3) & setFacelet2 6 (getFacelet (lFace defaultCube) 4)
 setFacelet (setFacelet (setFacelet (uFace defaultCube) 4 (getFacelet (lFace defaultCube) 2)) 5 (getFacelet (lFace defaultCube) 3)) 6 (getFacelet (lFace defaultCube) 4) 
 

f :: Cube -> Cube
f (Cube uFace dFace rFace lFace fFace bFace) =
  Cube
    { uFace = updatedUFace
    , dFace = updatedDFace
    , rFace = updatedRFace
    , lFace = updatedLFace
    , fFace = updatedFFace
    , bFace = bFace
    }
    where
    -- Rotate the front face clockwise
    updatedFFace = rotateClockwise fFace
    -- Update the adjacent faces

    {--
    updatedUFace = setFacelet uFace 4 (getFacelet lFace 2)
    updatedUFace = setFacelet uFace 5 (getFacelet lFace 3)
    updatedUFace = setFacelet uFace 6 (getFacelet lFace 4)
    --}
    updatedUFace = setFacelet (setFacelet (setFacelet uFace 4 (getFacelet lFace 2)) 5 (getFacelet lFace 3)) 6 (getFacelet lFace 4) -- chaining

    {--
    updatedLFace = setFacelet lFace 2 (getFacelet dFace 0)
    updatedLFace = setFacelet lFace 3 (getFacelet dFace 1)
    updatedLFace = setFacelet lFace 4 (getFacelet dFace 2)
    --}
    updatedLFace = setFacelet (setFacelet (setFacelet lFace 2 (getFacelet dFace 0)) 3 (getFacelet dFace 1)) 4 (getFacelet dFace 2) -- chaining

    {--
    updatedDFace = setFacelet dFace 0 (getFacelet rFace 6)
    updatedDFace = setFacelet dFace 1 (getFacelet rFace 7)
    updatedDFace = setFacelet dFace 2 (getFacelet rFace 0)
    --}
    updatedDFace = setFacelet (setFacelet (setFacelet dFace 0 (getFacelet rFace 6)) 1 (getFacelet rFace 7)) 2 (getFacelet rFace 0) -- chaining

    {--
    updatedRFace = setFacelet rFace 6 (getFacelet uFace 4)
    updatedRFace = setFacelet rFace 7 (getFacelet uFace 5)
    updatedRFace = setFacelet rFace 0 (getFacelet uFace 6)
    --}
    updatedRFace = setFacelet (setFacelet (setFacelet rFace 6 (getFacelet uFace 4)) 7 (getFacelet uFace 5)) 0 (getFacelet uFace 6) -- chaining

l :: Cube -> Cube
l (Cube uFace dFace rFace lFace fFace bFace) =
  Cube
    { uFace = updatedUFace
    , dFace = updatedDFace
    , rFace = rFace
    , lFace = updatedLFace
    , fFace = updatedFFace
    , bFace = updatedBFace
    }
    where
    -- rotate the left face clockwise
    updatedLFace = rotateClockwise lFace
    -- update the adjacent faces
    
    {--
    updatedUFace = setFacelet uFace 0 (getFacelet bFace 4)
    updatedUFace = setFacelet uFace 7 (getFacelet bFace 3)
    updatedUFace = setFacelet uFace 6 (getFacelet bFace 2)
    --}
    updatedUFace = setFacelet (setFacelet (setFacelet uFace 0 (getFacelet bFace 4)) 7 (getFacelet bFace 3)) 6 (getFacelet bFace 2) -- chaining

    {--
    updatedFFace = setFacelet fFace 0 (getFacelet uFace 0)
    updatedFFace = setFacelet fFace 7 (getFacelet uFace 7)
    updatedFFace = setFacelet fFace 6 (getFacelet uFace 6)
    --}
    updatedFFace = setFacelet (setFacelet (setFacelet fFace 0 (getFacelet uFace 0)) 7 (getFacelet uFace 7)) 6 (getFacelet uFace 6) -- chaining

    {--
    updatedDFace = setFacelet dFace 0 (getFacelet fFace 0)
    updatedDFace = setFacelet dFace 7 (getFacelet fFace 7)
    updatedDFace = setFacelet dFace 6 (getFacelet fFace 6)
    --}
    updatedDFace = setFacelet (setFacelet (setFacelet dFace 0 (getFacelet fFace 0)) 7 (getFacelet fFace 7)) 6 (getFacelet fFace 6) -- chaining

    {--
    updatedBFace = setFacelet bFace 4 (getFacelet dFace 0)
    updatedBFace = setFacelet bFace 3 (getFacelet dFace 7)
    updatedBFace = setFacelet bFace 2 (getFacelet dFace 6)
    --}
    updatedBFace = setFacelet (setFacelet (setFacelet bFace 4 (getFacelet dFace 0)) 3 (getFacelet dFace 7)) 2 (getFacelet dFace 6) -- chaining

r :: Cube -> Cube
r (Cube uFace dFace rFace lFace fFace bFace) = 
  Cube
    { uFace = updatedUFace
    , dFace = updatedDFace
    , rFace = updatedRFace
    , lFace = lFace
    , fFace = updatedFFace
    , bFace = updatedBFace
    }
    where
    -- rotate the left face clockwise
    updatedRFace = rotateClockwise rFace
    -- update the adjacent faces

    {--
    updatedUFace = setFacelet uFace 2 (getFacelet fFace 2)
    updatedUFace = setFacelet uFace 3 (getFacelet fFace 3)
    updatedUFace = setFacelet uFace 4 (getFacelet fFace 4)
    --}
    updatedUFace = setFacelet (setFacelet (setFacelet uFace 2 (getFacelet fFace 2)) 3 (getFacelet fFace 3)) 4 (getFacelet fFace 4) -- chaining

    {--
    updatedFFace = setFacelet fFace 2 (getFacelet dFace 2)
    updatedFFace = setFacelet fFace 3 (getFacelet dFace 3)
    updatedFFace = setFacelet fFace 4 (getFacelet dFace 4)
    --}
    updatedFFace = setFacelet (setFacelet (setFacelet fFace 2 (getFacelet dFace 2)) 3 (getFacelet dFace 3)) 4 (getFacelet dFace 4) -- chaining

    {--
    updatedDFace = setFacelet dFace 2 (getFacelet bFace 6)
    updatedDFace = setFacelet dFace 3 (getFacelet bFace 7)
    updatedDFace = setFacelet dFace 4 (getFacelet bFace 0)
    --}
    updatedDFace = setFacelet (setFacelet (setFacelet dFace 2 (getFacelet bFace 6)) 3 (getFacelet bFace 7)) 4 (getFacelet bFace 0) -- chaining

    {--
    updatedBFace = setFacelet bFace 0 (getFacelet uFace 4)
    updatedBFace = setFacelet bFace 7 (getFacelet uFace 3)
    updatedBFace = setFacelet bFace 6 (getFacelet uFace 2)
    --}
    updatedBFace = setFacelet (setFacelet (setFacelet bFace 0 (getFacelet uFace 4)) 7 (getFacelet uFace 3)) 6 (getFacelet uFace 2) -- chaining

u :: Cube -> Cube
u (Cube uFace dFace rFace lFace fFace bFace) = 
  Cube
    { uFace = updatedUFace
    , dFace = dFace
    , rFace = updatedRFace
    , lFace = updatedLFace
    , fFace = updatedFFace
    , bFace = updatedBFace
    }
    where
    -- rotate the up face clockwise
    updatedUFace = rotateClockwise uFace
    -- update the adjacent faces

    {--
    updatedFFace = setFacelet fFace 0 (getFacelet rFace 0)
    updatedFFace = setFacelet fFace 1 (getFacelet rFace 1)
    updatedFFace = setFacelet fFace 2 (getFacelet rFace 2)
    --}
    updatedFFace = setFacelet (setFacelet (setFacelet fFace 0 (getFacelet rFace 0)) 1 (getFacelet rFace 1)) 2 (getFacelet rFace 2) -- chaining

    {--
    updatedLFace = setFacelet lFace 0 (getFacelet fFace 0)
    updatedLFace = setFacelet lFace 1 (getFacelet fFace 1)
    updatedLFace = setFacelet lFace 2 (getFacelet fFace 2)
    --}
    updatedLFace = setFacelet (setFacelet (setFacelet lFace 0 (getFacelet fFace 0)) 1 (getFacelet fFace 1)) 2 (getFacelet fFace 2) -- chaining

    {--
    updatedBFace = setFacelet bFace 0 (getFacelet lFace 0)
    updatedBFace = setFacelet bFace 1 (getFacelet lFace 1)
    updatedBFace = setFacelet bFace 2 (getFacelet lFace 2)
    --}
    updatedBFace = setFacelet (setFacelet (setFacelet bFace 0 (getFacelet lFace 0)) 1 (getFacelet lFace 1)) 2 (getFacelet lFace 2) -- chaining

    {--
    updatedRFace = setFacelet rFace 0 (getFacelet bFace 0)
    updatedRFace = setFacelet rFace 1 (getFacelet bFace 1)
    updatedRFace = setFacelet rFace 2 (getFacelet bFace 2)
    --}
    updatedRFace = setFacelet (setFacelet (setFacelet rFace 0 (getFacelet bFace 0)) 1 (getFacelet bFace 1)) 2 (getFacelet bFace 2) -- chaining

d :: Cube -> Cube
d (Cube uFace dFace rFace lFace fFace bFace) = 
    Cube
    { uFace = uFace
    , dFace = updatedDFace
    , rFace = updatedRFace
    , lFace = updatedLFace
    , fFace = updatedFFace
    , bFace = updatedBFace
    }
    where
    -- rotate the down face clockwise
    updatedDFace = rotateClockwise dFace

    -- update the adjacent faces

    {--
    updatedFFace = setFacelet fFace 4 (getFacelet lFace 4)
    updatedFFace = setFacelet fFace 5 (getFacelet lFace 5)
    updatedFFace = setFacelet fFace 6 (getFacelet lFace 6)
    --}
    updatedFFace = setFacelet (setFacelet (setFacelet fFace 4 (getFacelet lFace 4)) 5 (getFacelet lFace 5)) 6 (getFacelet lFace 6) -- chaining

    {--
    updatedLFace = setFacelet lFace 4 (getFacelet bFace 4)
    updatedLFace = setFacelet lFace 5 (getFacelet bFace 5)
    updatedLFace = setFacelet lFace 6 (getFacelet bFace 6)
    --}
    updatedLFace = setFacelet (setFacelet (setFacelet lFace 4 (getFacelet bFace 4)) 5 (getFacelet bFace 5)) 6 (getFacelet bFace 6) -- chaining

    {--
    updatedBFace = setFacelet bFace 4 (getFacelet rFace 4)
    updatedBFace = setFacelet bFace 5 (getFacelet rFace 5)
    updatedBFace = setFacelet bFace 6 (getFacelet rFace 6)
    --}
    updatedBFace = setFacelet (setFacelet (setFacelet bFace 4 (getFacelet rFace 4)) 5 (getFacelet rFace 5)) 6 (getFacelet rFace 6) -- chaining

    {--
    updatedRFace = setFacelet rFace 4 (getFacelet fFace 4)
    updatedRFace = setFacelet rFace 5 (getFacelet fFace 5)
    updatedRFace = setFacelet rFace 6 (getFacelet fFace 6)
    --}
    updatedRFace = setFacelet (setFacelet (setFacelet rFace 4 (getFacelet fFace 4)) 5 (getFacelet fFace 5)) 6 (getFacelet fFace 6) -- chaining

b :: Cube -> Cube
b (Cube uFace dFace rFace lFace fFace bFace) =
    Cube
    { uFace = updatedUFace
    , dFace = updatedDFace
    , rFace = updatedRFace
    , lFace = updatedLFace
    , fFace = fFace
    , bFace = updatedBFace
    }
    where
    -- rotate the down face clockwise
    updatedBFace = rotateClockwise bFace

    -- update the adjacent faces

    {--
    updatedUFace = setFacelet uFace 0 (getFacelet rFace 2)
    updatedUFace = setFacelet uFace 1 (getFacelet rFace 3)
    updatedUFace = setFacelet uFace 2 (getFacelet rFace 4)
    --}
    updatedUFace = setFacelet (setFacelet (setFacelet uFace 0 (getFacelet rFace 2)) 1 (getFacelet rFace 3)) 2 (getFacelet rFace 4) -- chaining

    {--
    updatedRFace = setFacelet rFace 2 (getFacelet dFace 4)
    updatedRFace = setFacelet rFace 3 (getFacelet dFace 5)
    updatedRFace = setFacelet rFace 4 (getFacelet dFace 6)
    --}
    updatedRFace = setFacelet (setFacelet (setFacelet rFace 2 (getFacelet dFace 4)) 3 (getFacelet dFace 5)) 4 (getFacelet dFace 6) -- chaining

    {--
    updatedDFace = setFacelet dFace 4 (getFacelet lFace 6)
    updatedDFace = setFacelet dFace 5 (getFacelet lFace 7)
    updatedDFace = setFacelet dFace 6 (getFacelet lFace 0)
    --}
    updatedDFace = setFacelet (setFacelet (setFacelet dFace 4 (getFacelet lFace 6)) 5 (getFacelet lFace 7)) 6 (getFacelet lFace 0) -- chaining

    {--
    updatedLFace = setFacelet lFace 6 (getFacelet uFace 0)
    updatedLFace = setFacelet lFace 7 (getFacelet uFace 1)
    updatedLFace = setFacelet lFace 0 (getFacelet uFace 2)
    --}
    updatedLFace = setFacelet (setFacelet (setFacelet lFace 6 (getFacelet uFace 0)) 7 (getFacelet uFace 1)) 0 (getFacelet uFace 2) -- chaining
--}
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
-- ********************************************************** END CubeModule.hs ********************************************************** --
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --


-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
-- ********************************************************* START SolveModule.hs ******************************************************** --
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
{--
doMove :: Cube -> String -> Cube
doMove cube move =
  case move of
    "U"  -> u cube
    "U'" -> u' cube
    "U2" -> u2 cube
    "U2'" -> u2 cube
    "R"  -> r cube
    "R'" -> r' cube
    "R2" -> r2 cube
    "R2'" -> r2 cube
    "F"  -> f cube
    "F'" -> f' cube
    "F2" -> f2 cube
    "F2'" -> f2 cube
    "L"  -> l cube
    "L'" -> l' cube
    "L2" -> l2 cube
    "L2'" -> l2 cube
    "B"  -> b cube
    "B'" -> b' cube
    "B2" -> b2 cube
    "B2'" -> b2 cube
    "D"  -> d cube
    "D'" -> d' cube
    "D2" -> d2 cube
    "D2'" -> d2 cube
    _ ->  error "Invalid move"  -- for unknown moves
--}

{--
main :: IO ()
main = do
    let op = d.b.d2.f.l.r.f.u.b'
    let newCube = op defaultCube
    print newCube

    (solvedCube, solution) <- solveCube newCube
    putStrLn "Solved Cube:"
    print solvedCube
    putStrLn "Solution:"
    print solution


main :: IO ()
main = do
  -- putStrLn $ show myFace
  -- putStrLn $ "\n\n\n\n"
  -- putStrLn "this is \n the end"
  print defaultCube

  let newCube = nB (tD (nF (defaultCube)))
  print newCube

  --print $ convertToKociembaString defaultCube
  --print $ convertToKociembaString newCube

  let op = tL.tF.tD.tB.tR.tF.tU.nB.tR.pF
  let newCube2 =  op newCube

  print newCube2

  {--
  print myFace

  let rotatedFace = rotateClockwise myFace
  let rotatedFace2 = rotateAntiClockwise myFace

  print rotatedFace
  print rotatedFace2


  let myFace = Face {centreColour = R, facelets = 0b0000000000000000000000000000000000000000000000000000000000000000}
  print myFace

  let newFace = setFacelet 0 1 myFace
  print newFace

  let newFace = setFacelet 3 2 newFace
  print newFace
  -}



main :: IO ()
main = do
  -- Replace the cube definition string with your actual cube definition
  let cubeDefinition = "RRDUUULLLURRURRLLRFFFFFFBBBDDDDDDLLUULDULDURRFBBFBBFBB"

  -- Call the kociemba library as a shell command
  solution <- readProcess "kociemba" [] cubeDefinition

  -- Print the solution
  putStrLn solution

convertToKociembaString :: Cube -> String
convertToKociembaString (Cube uFace dFace rFace lFace fFace bFace) =
    replaceColours $
    removeSpacesAndLower $
    showRow uFace 0 ++ showRow uFace 1 ++ showRow uFace 2 ++
    showRow rFace 0 ++ showRow rFace 1 ++ showRow rFace 2 ++ 
    showRow fFace 0 ++ showRow fFace 1 ++ showRow fFace 2 ++
    showRow dFace 0 ++ showRow dFace 1 ++ showRow dFace 2 ++
    showRow lFace 0 ++ showRow lFace 1 ++ showRow lFace 2 ++
    showRow bFace 0 ++ showRow bFace 1 ++ showRow bFace 2

    where
      removeSpacesAndLower :: String -> String
      removeSpacesAndLower = map toLower . filter (/= ' ')

      replaceColours :: String -> String
      replaceColours = map substitute
        where
        substitute 'r' = 'U'
        substitute 'w' = 'F'
        substitute 'b' = 'L'
        substitute 'g' = 'R'
        substitute 'y' = 'B'
        substitute 'o' = 'D'
--}
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --
-- ********************************************************** END SolveModule.hs ********************************************************* --
-- *************************************************************************************************************************************** --
-- *************************************************************************************************************************************** --