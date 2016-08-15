module CircleTreeGraphics (drawCT, DrawCT, Vertical, vert) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import CircleTreeCalculus

newtype Vertical a = Vertical {unVertical :: [a]}

vert :: [a] -> Vertical a
vert = Vertical

class DrawCT a where
    drawCT :: a -> Diagram B

instance (DrawCT x) => DrawCT [x] where
    drawCT = hsep 2 . fmap drawCT

instance (DrawCT x) => DrawCT (Vertical x) where
    drawCT = vsep 0.25 . fmap drawCT . unVertical

instance DrawCT LCalc where
    drawCT = pad 1.1 . center . drawLambda' 0

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
            # lwL 0.05
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
                        & shaftStyle %~ lwL 1)
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
vfcolor B = (darkcyan, cyan)

