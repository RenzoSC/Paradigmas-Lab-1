module Dibujos.Escher (
    interpBas, testAll,
    escherConf
)where

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blue, red, color, line, pictures )

-- Les ponemos colorcitos para que no sea _tan_ feo
data Color = Azul | Rojo
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Cruz | Triangulo | Efe
    deriving (Show, Eq)

type Basica = (BasicaSinColor, Color)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue
colorear Rojo = color red

interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), y V.+ half w, w, (0,0)]


interpBas :: Output Basica
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

-- Diferentes tests para ver que estén bien las operaciones
figRoja :: BasicaSinColor -> Dibujo Basica
figRoja b = figura (b, Rojo)

figAzul :: BasicaSinColor -> Dibujo Basica
figAzul b = figura (b, Azul)

-- Debería mostrar un rectángulo azul arriba de otro rojo,
-- conteniendo toda la grilla dentro
apilados :: BasicaSinColor -> Dibujo Basica
apilados b = apilar 1 1 (rotar(apilar 1 1 (figAzul Triangulo) (figAzul Triangulo))) (rotar(juntar 1 1 (figAzul Triangulo) (figAzul Triangulo)))

-- -- Supongamos que eligen.
-- type Escher = Bool

-- -- El dibujo u.
-- dibujoU :: Dibujo Escher -> Dibujo Escher
-- dibujoU p = undefined 

-- -- El dibujo t.
-- dibujoT :: Dibujo Escher -> Dibujo Escher
-- dibujoT p = undefined 

-- -- Esquina con nivel de detalle en base a la figura p.
-- esquina :: Int -> Dibujo Escher -> Dibujo Escher
-- esquina n p = undefined

-- -- Lado con nivel de detalle.
-- lado :: Int -> Dibujo Escher -> Dibujo Escher
-- lado n p = undefined

-- -- Por suerte no tenemos que poner el tipo!
-- noneto p q r s t u v w x = undefined

-- -- El dibujo de Escher:
-- escher :: Int -> Escher -> Dibujo Escher
-- escher = undefined

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

testAll :: Dibujo Basica
testAll = grilla [
    [apilados Trianguloxd] [apilados Triangulo] [apilados Triangulo] [apilados Triangulo]
    ]


escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = testAll,
    bas = interpBas
}
