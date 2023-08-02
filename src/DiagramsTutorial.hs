import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

node :: Int -> Diagram B
node x = regPoly x 1 # rotateBy (0.5/6)

horizontal :: Int -> Diagram B
horizontal 0 = node 6
horizontal x = node 6 ||| horizontal (x-1) 

vertical :: Int -> Diagram B
vertical x
    | x <= 0 = node 0
    | even x = horizontal 8 <> vertical (x-1) # translate (r2 (-0.86, 1.54))
    | odd x = horizontal 8 <> vertical (x-1) # translate (r2 (0.86, 1.54))   
    | otherwise = node 6

-- vertical y = horizontal 8 === vertical (y-1) # translate (r2 (0.9, -1))   
-- snugY 0.1 (regPoly x 1) # rotateBy (0.5/6)
example = vertical 8

main = mainWith example 