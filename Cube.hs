module Cube where
import Face

import Data.Function ((&))

-- Defines the Cube structure with the uFace, dFace, rFace, lFace, fFace, and bFace
data Cube = Cube
    { uFace :: Face
    , dFace :: Face 
    , rFace :: Face 
    , lFace :: Face 
    , fFace :: Face
    , bFace :: Face 
    } deriving (Eq)

-- Defines the Show instance for the Cube structure
instance Show Cube where
  show (Cube uFace dFace rFace lFace fFace bFace) =
    "         +-------+ \n" ++
    " " ++ unwords ["        | " ++ showRow uFace i ++ " | \n" | i <- rowIndices] ++
    " +-------+-------+-------+-------+\n" ++
    " " ++ unwords ["| " ++ showRow lFace i ++ " | " ++ showRow fFace i ++ " | " ++ showRow rFace i ++ " | " ++ showRow bFace i ++ " | \n" | i <- rowIndices] ++
    " +-------+-------+-------+-------+\n" ++
    " " ++unwords [ "        | " ++ showRow dFace i ++ " | \n" | i <- rowIndices] ++
    "         +-------+ "
    where rowIndices = [0, 1, 2]

-- Defines the identity function on the Cube
id :: Cube -> Cube
id cube = cube

-- Defines the F move that rotates the Front face clockwise
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

    updatedUFace =  uFace & setFacelet 4 (getFacelet lFace 2)
                          & setFacelet 5 (getFacelet lFace 3)
                          & setFacelet 6 (getFacelet lFace 4)

    updatedLFace =  lFace & setFacelet 2 (getFacelet dFace 0)
                          & setFacelet 3 (getFacelet dFace 1)
                          & setFacelet 4 (getFacelet dFace 2)

    updatedDFace =  dFace & setFacelet 0 (getFacelet rFace 6)
                          & setFacelet 1 (getFacelet rFace 7)
                          & setFacelet 2 (getFacelet rFace 0)

    updatedRFace =  rFace & setFacelet 6 (getFacelet uFace 4)
                          & setFacelet 7 (getFacelet uFace 5)
                          & setFacelet 0 (getFacelet uFace 6)

-- Defines the L move that rotates the Left face clockwise
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
    
    updatedUFace =  uFace & setFacelet 0 (getFacelet bFace 4)
                          & setFacelet 7 (getFacelet bFace 3)
                          & setFacelet 6 (getFacelet bFace 2)

    updatedFFace =  fFace & setFacelet 0 (getFacelet uFace 0)
                          & setFacelet 7 (getFacelet uFace 7)
                          & setFacelet 6 (getFacelet uFace 6)

    updatedDFace =  dFace & setFacelet 0 (getFacelet fFace 0)
                          & setFacelet 7 (getFacelet fFace 7)
                          & setFacelet 6 (getFacelet fFace 6)

    updatedBFace =  bFace & setFacelet 4 (getFacelet dFace 0)
                          & setFacelet 3 (getFacelet dFace 7)
                          & setFacelet 2 (getFacelet dFace 6)

-- Defines the R move that rotates the Right face clockwise
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

    updatedUFace =  uFace & setFacelet 2 (getFacelet fFace 2)
                          & setFacelet 3 (getFacelet fFace 3)
                          & setFacelet 4 (getFacelet fFace 4)

    updatedFFace =  fFace & setFacelet 2 (getFacelet dFace 2)
                          & setFacelet 3 (getFacelet dFace 3)
                          & setFacelet 4 (getFacelet dFace 4)

    updatedDFace =  dFace & setFacelet 2 (getFacelet bFace 6)
                          & setFacelet 3 (getFacelet bFace 7)
                          & setFacelet 4 (getFacelet bFace 0)

    updatedBFace =  bFace & setFacelet 0 (getFacelet uFace 4)
                          & setFacelet 7 (getFacelet uFace 3)
                          & setFacelet 6 (getFacelet uFace 2)

-- Defines the U move that rotates the Up face clockwise
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

    updatedFFace =  fFace & setFacelet 0 (getFacelet rFace 0)
                          & setFacelet 1 (getFacelet rFace 1)
                          & setFacelet 2 (getFacelet rFace 2)

    updatedLFace =  lFace & setFacelet 0 (getFacelet fFace 0)
                          & setFacelet 1 (getFacelet fFace 1)
                          & setFacelet 2 (getFacelet fFace 2)

    updatedBFace =  bFace & setFacelet 0 (getFacelet lFace 0)
                          & setFacelet 1 (getFacelet lFace 1)
                          & setFacelet 2 (getFacelet lFace 2)

    updatedRFace =  rFace & setFacelet 0 (getFacelet bFace 0)
                          & setFacelet 1 (getFacelet bFace 1)
                          & setFacelet 2 (getFacelet bFace 2)

-- Defines the D move that rotates the Down face clockwise
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

    updatedFFace =  fFace & setFacelet 4 (getFacelet lFace 4)
                          & setFacelet 5 (getFacelet lFace 5)
                          & setFacelet 6 (getFacelet lFace 6) 

    updatedLFace =  lFace & setFacelet 4 (getFacelet bFace 4)
                          & setFacelet 5 (getFacelet bFace 5)
                          & setFacelet 6 (getFacelet bFace 6)

    updatedBFace =  bFace & setFacelet 4 (getFacelet rFace 4)
                          & setFacelet 5 (getFacelet rFace 5)
                          & setFacelet 6 (getFacelet rFace 6)

    updatedRFace =  rFace & setFacelet 4 (getFacelet fFace 4)
                          & setFacelet 5 (getFacelet fFace 5)
                          & setFacelet 6 (getFacelet fFace 6)

-- Defines the B move that rotates the Back face clockwise
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

    updatedUFace =  uFace & setFacelet 0 (getFacelet rFace 2)
                          & setFacelet 1 (getFacelet rFace 3)
                          & setFacelet 2 (getFacelet rFace 4)

    updatedRFace =  rFace & setFacelet 2 (getFacelet dFace 4)
                          & setFacelet 3 (getFacelet dFace 5)
                          & setFacelet 4 (getFacelet dFace 6)

    updatedDFace =  dFace & setFacelet 4 (getFacelet lFace 6)
                          & setFacelet 5 (getFacelet lFace 7)
                          & setFacelet 6 (getFacelet lFace 0)

    updatedLFace =  lFace & setFacelet 6 (getFacelet uFace 0) 
                          & setFacelet 7 (getFacelet uFace 1)
                          & setFacelet 0 (getFacelet uFace 2)

-- Defines the F2 move that rotates the Front face twice
f2 :: Cube -> Cube
f2 = f . f

-- Defines the F' move that rotates the Front face anti-clockwise
f' :: Cube -> Cube
f' = f . f . f

-- Defines the L2 move that rotates the Left face twice
l2 :: Cube -> Cube
l2 = l . l

-- Defines the L' move that rotates the Left face anti-clockwise
l' :: Cube -> Cube
l' = l . l . l

-- Defines the R2 move that rotates the Right face twice
r2 :: Cube -> Cube
r2 = r . r

-- Defines the R' move that rotates the Right face anti-clockwise
r' :: Cube -> Cube
r' = r . r . r

-- Defines the U2 move that rotates the Up face twice
u2 :: Cube -> Cube
u2 = u . u

-- Defines the U' move that rotates the Up face anti-clockwise
u' :: Cube -> Cube
u' = u . u . u

-- Defines the D2 move that rotates the Down face twice
d2 :: Cube -> Cube
d2 = d . d

-- Defines the D' move that rotates the Down face anti-clockwise
d' :: Cube -> Cube
d' = d . d . d

-- Defines the B2 move that rotates the Back face twice
b2 :: Cube -> Cube
b2 = b . b

-- Defines the B' move that rotates the Back face anti-clockwise
b' :: Cube -> Cube
b' = b . b . b

-- Defines the standard state of the cube
defaultCube :: Cube
defaultCube = Cube
  { uFace = Face
    { centreFacelet = R
    , facelets = 0b0000001000000010000000100000001000000010000000100000001000000010
    }
  , dFace = Face
    { centreFacelet = O
    , facelets = 0b0000010000000100000001000000010000000100000001000000010000000100
    }
  , rFace = Face
    { centreFacelet = G
    , facelets = 0b0000000100000001000000010000000100000001000000010000000100000001
    }
  , lFace = Face
    { centreFacelet = B
    , facelets = 0b0000001100000011000000110000001100000011000000110000001100000011
    }
  , fFace = Face
    { centreFacelet = W
    , facelets = 0b0000000000000000000000000000000000000000000000000000000000000000
    }
  , bFace = Face
    { centreFacelet = Y
    , facelets = 0b0000010100000101000001010000010100000101000001010000010100000101
    }
  }