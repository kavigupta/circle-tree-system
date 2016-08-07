module CircleTreeOutput((<<<)) where

import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import CircleTreeGraphics

infixr 0 <<<
(<<<) :: (DrawCT x) => String -> x -> IO ()
(<<<) path = renderSVG path (mkWidth 500) . drawCT
