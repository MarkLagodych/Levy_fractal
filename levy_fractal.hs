{-
    This program generates a Levy fractal and saves it to an .svg file
-}

import System.IO


-- String constants for SVG

beginSVG = "\
\<?xml \n\
\    version = \"1.0\" \n\
\    standalone = \"no\"?>\n\
\\n\
\<svg \n\
\    version = \"1.1\" \n\
\    xmlns = \"http://www.w3.org/2000/svg\"> \n\
\\n\
\<defs>\n\
\      <linearGradient id = \"Gradient1\" x1 = \"0\" x2 = \"1\" y1 = \"0\" y2 = \"0\">\n\
\        <stop offset = \"0%\"   stop-color = \"#00ff00\" />\n\
\        <stop offset = \"100%\" stop-color = \"#0000ff\"  />\n\
\      </linearGradient>\n\
\</defs>\n\
\\n"

beginPoints = "\
\<polyline \n\
\    fill = \"transparent\" \n\
\    stroke = \"url(#Gradient1)\" \n\
\    points = \" "

endPoints = "\" />\n"
        
endSVG = "\n</svg>\n"


-- These functions write point arrays to .svg files

data Point = Point Int Int

writePoints :: Handle -> [Point] -> IO ()

writePoints file [] = return ()
writePoints file ((Point x y):ps) = do
    hPutStr file $ show x
    hPutChar file ' '
    hPutStr file $ show y
    hPutChar file ' '
    writePoints file ps


writeSVG :: FilePath -> [Point] -> IO ()

writeSVG fp ps = do
    file <- openFile fp WriteMode
    
    hPutStr file beginSVG
    hPutStr file beginPoints
    
    writePoints file ps
    
    hPutStr file endPoints
    hPutStr file endSVG
    
    hClose file
    
    
-- Makes a signle Levy fractal generation from a given array of ready points

genLevy1 :: [Point] -> [Point]

genLevy1 [] = []
genLevy1 [p] = [p]
genLevy1 ((Point x1 y1) : (Point x2 y2) : ps) = 
    let hhd = (abs (x2 - x1)) `div` 2 -- Half Horizontal Distance
        hvd = (abs (y2 - y1)) `div` 2 -- Half Vertical Distance
    
        insertPoint = case compare y1 y2 of
            EQ -> case compare x1 x2 of
                LT -> Point (x1 + hhd) (y1 - hhd)
                GT -> Point (x1 - hhd) (y1 + hhd)
                EQ -> Point  x1 y1
                        
            LT -> case compare x1 x2 of
                LT -> Point  x2         y1
                GT -> Point  x1         y2
                EQ -> Point (x1 + hvd) (y1 + hvd)
                
            GT -> case compare x1 x2 of
                LT -> Point  x1         y2
                GT -> Point  x2         y1
                EQ -> Point (x1 - hvd) (y1 - hvd)
            
            
    in (Point x1 y1) : insertPoint : (genLevy1 $ (Point x2 y2) : ps)



-- Make g generations of Levy fractal
-- ps are first two points

genLevy :: Int -> [Point] -> [Point]

genLevy g ps | g < 0 = ps
genLevy 0 ps         = ps
genLevy g ps         = genLevy (pred g) (genLevy1 ps)


-- Moves all points on some distance

movePoints :: Point -> [Point] -> [Point]

movePoints _ [] = []
movePoints (Point x y) ((Point a b):ys) = (Point (a+x) (b+y)) : (movePoints (Point x y) ys)


-- Does all the work
-- Argument 1 is the distance between first two points
-- Argument 2 is the number of iterations

generateLevyFractal :: Int -> Int -> [Point]

generateLevyFractal d g = movePoints (Point (d `div` 2) d) $ genLevy g [Point 0 0, Point d 0]


-- ---------------------------------------------------------------------------------------------
    
main = do
    writeSVG "levy_fractal.svg" $ generateLevyFractal 600 15
