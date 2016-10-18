import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Pages

import Expr
import Data.Maybe

-- | -----------------Part II-----------------------------------------|--
-- Assigment F
-------------------------------------------------------------------------

-- Calculates each point to be drawn on the canvas for the given expr
points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) =
      [(x , calY x) | x <- [0.0 .. fromIntegral width],
      calY x <= fromIntegral height, calY x >= 0]
    where
      -- returns a pixel value of y
      calY x =
        fromIntegral height - realToPix(eval exp (pixToReal x scale)) scale
      --converts a pixel x-coordinate to a real x-coordinate
      pixToReal x s = x * s - 6
      -- converts a real y-coordinate to a pixel y-coordinate
      realToPix y s = (y + 6) / s

-- Assigment G
-------------------------------------------------------------------------
--Height and with of the canvas
canWidth  = 300
canHeight = 300

-- Reads the value from the textbox and draws the graph using the points
-- given in the points function
readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw elem can = do
    input <- getValue elem
    render can $ stroke $ path $
     points (fromJust $ readExpr $ fromJust input) 0.04 (canWidth,canHeight)

--Given code
----------------------------------------------------------------------------
main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13

