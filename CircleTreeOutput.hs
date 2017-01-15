module CircleTreeOutput((<<<), runOutput) where

import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import Control.Monad.Reader

import System.FilePath
import System.Directory

import CircleTreeGraphics

type CircleTreeOutput = ReaderT FilePath IO ()

runOutput :: FilePath -> CircleTreeOutput -> IO ()
runOutput dir out = do
    createDirectoryIfMissing True dir
    runReaderT out dir

infixr 0 <<<
(<<<) :: (DrawCT x) => String -> x -> CircleTreeOutput
fname <<< toDraw = do
    directory <- ask
    liftIO . renderSVG (directory </> fname) (mkWidth 500) . drawCT $ toDraw
