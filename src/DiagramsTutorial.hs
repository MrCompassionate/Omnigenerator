module DiagramsTutorial where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

type DiagramWithState = State Int (Diagram B)

-- X and Y inputs


-- Nodes

-- This function takes an integer and creates a diagram with the integer value displayed.
-- It uses the text function to convert the integer to its string representation.
-- The resulting textual diagram is styled with a font size of 0.2 and a black foreground color.
node :: DiagramWithState
node = do
    a <- get
    put (a+1)
    return $ text (show a) # fontSizeL 0.2 # fc black 
-- Grid Generation

-- horizontal creates a horizontal line of hexagons. 
-- The function works recursively: for each step, it creates a hexagon on the left (using the node function) 
-- and then constructs the remainder of the line on the right.
horizontal :: Int -> DiagramWithState
horizontal 0 = return $ regPoly 0 0  -- Base case: if there are no hexagons left to draw, return an empty diagram.
horizontal x = do
    leftNode <- node                 -- Draw the leftmost hexagon.
    rightGrid <- horizontal (x-1)    -- Recursively draw the remainder of the line.
    -- Combine the left hexagon with the remainder of the line.
    -- The hexagon is rotated appropriately to align its flat side at the top.
    return $ (leftNode <> (regPoly 6 1 # rotateBy (0.5/6))) ||| rightGrid 

-- vertical creates a vertical column of hexagon lines. 
-- It also works recursively: for each step, it creates a horizontal line of hexagons at the top 
-- and then constructs the remainder of the column below.

vertical :: Int -> Int -> DiagramWithState
vertical y x
    | y <= 0 = return $ regPoly 0 (0 :: Double)  -- Base case: if there are no lines left to draw, return an empty diagram.
    | even y = do
        upperGrid <- horizontal x                  -- Draw the topmost line of hexagons.
        lowerGrid <- vertical (y-1) x              -- Recursively draw the remainder of the column.
                                                   -- Combine the top line with the remainder of the column, shifting the latter to align correctly.
        return $ upperGrid <> (lowerGrid # translate (r2 (-0.86, 1.54)))
    | odd y = do
        upperGrid <- horizontal x                  -- As above, but the alignment shift is in the opposite direction.
        lowerGrid <- vertical (y-1) x
        return $ upperGrid <> (lowerGrid # translate (r2 (0.86, 1.54)))
    | otherwise = return $ regPoly 0 (0 :: Double)  -- Shouldn't reach here due to the above conditions, but added for completeness.

-- main entry point: it draws a vertical column of 8 horizontal lines, each containing 6 hexagons, and starts numbering from 0.

generateMap :: Int -> Int -> IO()
generateMap x y = mainWith (evalState (vertical x y) 1)

-- main = mainWith (evalState (vertical 8 6) 1)