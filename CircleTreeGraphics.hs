module CircleTreeGraphics (drawLambda) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import CircleTreeCalculus
import Data.Maybe

drawLambda :: LCalc -> Diagram B
drawLambda = drawLambda' 0

drawLambda' :: Int -> LCalc -> Diagram B
drawLambda' _ (Var name)
    = circle 1 # fc (vcolor name)
drawLambda' n (Lambda name rest)
    = mappend
        (drawLambda' n rest
            # sizedAs (circle 0.8 :: Diagram B)
            # centroidify)
        (circle 1
            # lc (vcolor name)
            # lw veryThick
            # fc (fcolor name))
drawLambda' n (App func var)
        = curveConnecting
            (drawLambda' (2 * n + 1) func # scaleAndAnchorToTop)
            (drawLambda' (2 * n + 2) var # scaleAndAnchorToTop)
    where
    curveConnecting :: Diagram B -> Diagram B -> Diagram B
    curveConnecting left right
            = ((left # named (2 * n + 1)) ||| (right # named (2 * n + 2)))
                # connect'
                    (with
                        & arrowShaft .~ shaft
                        & arrowHead .~ noHead
                        & shaftTexture .~ solid black
                        & shaftStyle %~ lw veryThick)
                    (2 * n + 1) (2 * n + 2)
    shaft = cubicSpline False ( map p2 [(0, 0), (1, 0.4), (2, 0.4)])

centroidify :: Diagram B -> Diagram B
centroidify dia = moveOriginTo (fromJust $ boxCenter $ boundingBox dia) dia

scaleAndAnchor :: Diagram B -> Diagram B
scaleAndAnchor d = d # centroidify # scale (1 / height d)

scaleAndAnchorToTop :: Diagram B -> Diagram B
scaleAndAnchorToTop = translate (r2 (0, -0.5)) . scaleAndAnchor

vcolor, fcolor :: (Floating a, Ord a) => String -> Colour a
vcolor = fst . vfcolor
fcolor = snd . vfcolor

vfcolor :: (Floating a, Ord a) => String -> (Colour a, Colour a)
vfcolor "x" = (red, pink)
vfcolor "y" = (blue, lightblue)
vfcolor "z" = (green, lightgreen)
vfcolor "w" = (purple, mediumpurple)
vfcolor "t" = (yellow, lightyellow)
vfcolor _ = (grey, lightgrey)

