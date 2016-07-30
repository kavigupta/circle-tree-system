module CircleTreeGraphics (drawLambda) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import CircleTreeCalculus

drawLambda :: LCalc -> Diagram B
drawLambda = pad 1.1 . drawLambda' 0

drawLambda' :: Int -> LCalc -> Diagram B
drawLambda' _ (Var name)
    = circle 1 # fc (vcolor name) # lw none
drawLambda' n (Lambda name rest)
    = mappend
        (drawLambda' n rest
            # sizedAs (circle 0.8 :: Diagram B)
            # center)
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
                        & shaftStyle %~ lw thick)
                    (2 * n + 1) (2 * n + 2)
    shaft = cubicSpline False ( map p2 [(0, 0), (1, 0.4), (2, 0.4)])

scaleAndAnchor :: Diagram B -> Diagram B
scaleAndAnchor d = d # center # scale (1 / height d)

scaleAndAnchorToTop :: Diagram B -> Diagram B
scaleAndAnchorToTop = alignT . scaleAndAnchor

vcolor, fcolor :: (Floating a, Ord a) => Variable -> Colour a
vcolor = fst . vfcolor
fcolor = snd . vfcolor

vfcolor :: (Floating a, Ord a) => Variable -> (Colour a, Colour a)
vfcolor X = (red, pink)
vfcolor Y = (green, lightgreen)
vfcolor Z = (blue, lightblue)
vfcolor W = (purple, mediumpurple)
vfcolor T = (yellow, lightyellow)
vfcolor A = (brown, sandybrown)
vfcolor B = (darkorange, orange)
vfcolor C = (darkcyan, cyan)
