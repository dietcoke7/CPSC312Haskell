module FileIO where
import Cube

import System.IO
import Face
    ( Face(Face, facelets, centreFacelet), Colour(Y, R, O, G, B, W) )
import Data.Word

-- Saves the cube to the cube.txt file. Uses strict I/O to avoid locks and lazy evaluation.
saveCube :: Cube -> IO ()
saveCube (Cube uFace dFace rFace lFace fFace bFace) = do
    -- Causes file locks and lazy I/O
    -- writeFile "cube.txt" ""
    -- mapM_ (\face -> appendFile "cube.txt" (words (show face) !! 1 ++ "\n")) [uFace, dFace, rFace, lFace, fFace, bFace]

    handle <- openFile "cube.txt" WriteMode
    mapM_ (\face -> hPutStr handle (words (show face) !! 1 ++ "\n")) [uFace, dFace, rFace, lFace, fFace, bFace]
    hFlush handle
    hClose handle
    -- handle does strict I/O compared to writeFile's lazy I/O

-- Loads the cube from the cube.txt file. Uses strict I/O to avoid locks and lazy evaluation.
loadCube :: IO Cube
loadCube = do
    -- Causes file locks and lazy I/O
    --facelets <- readFile "cube.txt"

    handle <- openFile "cube.txt" ReadMode
    !facelets <- hGetContents handle
    hClose handle
    let faceletsArray = words facelets
    let !newCube = Cube {
        uFace = Face { centreFacelet = R, facelets = read (faceletsArray !! 0) :: Word64 }
      , dFace = Face { centreFacelet = O, facelets = read (faceletsArray !! 1) :: Word64 }
      , rFace = Face { centreFacelet = G, facelets = read (faceletsArray !! 2) :: Word64 }
      , lFace = Face { centreFacelet = B, facelets = read (faceletsArray !! 3) :: Word64 }
      , fFace = Face { centreFacelet = W, facelets = read (faceletsArray !! 4) :: Word64 }
      , bFace = Face { centreFacelet = Y, facelets = read (faceletsArray !! 5) :: Word64 }
    }
    return newCube
    -- ! is to avoid the laziness of Haskell in I/O