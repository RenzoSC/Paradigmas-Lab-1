module Dibujos.Grilla (
    grilla,
    interpCoord, 
    paintNums,
    grillaConf
)where

import Dibujo (Dibujo, juntar, apilar, figura)
import FloatingPic (Conf(..), Output)

import Graphics.Gloss ( text, scale, translate )

type Coord = (Float, Float)

interpCoord :: Output Coord
interpCoord coord _ _ _= uncurry translate coord $ scale 0.1 0.1 $ text (show coord)

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar (fromIntegral $ length ds) 1 d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar (fromIntegral $ length ds) 1 d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

paintNums :: Dibujo Coord
paintNums = grilla [[figura (x, y) | y <- [0..7]] | x <- [0..7]]

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla",
    pic = paintNums,
    bas = interpCoord
}