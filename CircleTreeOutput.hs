module CircleTreeOutput(writeToFile) where

import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import CircleTreeCalculus
import CircleTreeGraphics

writeToFile :: String -> Double -> LCalc -> IO ()
writeToFile path renderWidth term = renderSVG path (mkWidth renderWidth) (drawLambda term)