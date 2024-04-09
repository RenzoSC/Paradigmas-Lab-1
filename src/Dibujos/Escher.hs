module Dibujos.Escher (
    interpBas, testAll,
    escherConf
)where

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar, comp)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blue, red, color, line, pictures, blank, green, violet )

-- Les ponemos colorcitos para que no sea _tan_ feo
data Color = Azul | Violeta
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Cruz | Ygriega
    deriving (Show, Eq)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue
colorear Violeta = color violet

--En caso sea ilegal crear Ygriega, habria que Eliminarla, Crearla a partir de otras y Remplazar las ocurrencias
interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Rectangulo x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
interpBasicaSinColor Cruz x y w = pictures [line [x, x V.+ y V.+ w], line [x V.+ y, x V.+ w]]
interpBasicaSinColor Ygriega x y w = pictures [line [x, x V.+ y V.+ w], line [x V.+ half(y V.+ w), x V.+ w]]

-- -- Coloreo de Escher:
-- Tipo de Escher
type Escher = (BasicaSinColor, Color)

interpBas :: Output Escher
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

figVioleta :: BasicaSinColor -> Dibujo Escher
figVioleta b = figura (b, Violeta)

figAzul :: BasicaSinColor -> Dibujo Escher
figAzul b = figura (b, Azul)

-- -- Armado de Escher:

--Encuadra la figura dada
figCuadro :: BasicaSinColor -> Dibujo Escher
figCuadro a = encimar (figVioleta Rectangulo) (figAzul a)

-- -- -- Para que sera esto ????? -- -- --

-- Para que sera esto ?????
-- -- Supongamos que eligen.
-- type Escher = Bool

-- Para que sera esto ?????
-- -- El dibujo P. (De P sale R V X)
-- dibujoP :: Dibujo Escher -> Dibujo Escher
-- dibujoP p = undefined 

-- Para que sera esto ?????
-- -- El dibujo S. (De S sale Q U W)
-- dibujoS :: Dibujo Escher -> Dibujo Escher
-- dibujoS s = undefined 

-- -- Esquina con nivel de detalle en base a la figura P.
-- esquina :: Int -> Dibujo Escher -> Dibujo Escher
-- esquina n p = undefined

-- -- Lado con nivel de detalle en base a la figura S.
-- lado :: Int -> Dibujo Escher -> Dibujo Escher
-- lado n p = undefined

-- Para que sera esto ????? que es un noneto ????
-- -- Por suerte no tenemos que poner el tipo!
-- noneto p q r s t u v w x = undefined

-- Para que sera esto ?????
-- -- El dibujo de Escher:
-- escher :: Int -> Escher -> Dibujo Escher
-- escher = undefined

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- El dibujo P. (De P sale R V X)
dibujoP :: Dibujo Escher
dibujoP = juntar 1 2 (apilar 1 2 (figCuadro Cruz) 
                                 (apilar 1 1 (figCuadro Ygriega) 
                                             (rotar(figCuadro Ygriega))))
                     (apilar 1 2 (juntar 1 1 (figCuadro Ygriega) 
                                             (espejar(figCuadro Ygriega)))
                                 (figCuadro Cruz))

-- El dibujo S. (De S sale Q U W)
dibujoS :: Dibujo Escher
dibujoS = juntar 1 1 (apilar 1 2 (juntar 1 1 (figCuadro Ygriega) 
                                             (espejar(figCuadro Ygriega)))
                                 (figCuadro Ygriega))
                     (apilar 1 2 (juntar 1 1 (figCuadro Ygriega) 
                                             (espejar(figCuadro Ygriega)))
                                 (espejar(figCuadro Ygriega))) 

-- El dibujo T. (Centro del Dibujo)
centroT :: Dibujo Escher
centroT = figCuadro Cruz

-- Esquina con nivel de detalle en base a la figura P.
esquina :: Int -> Dibujo Escher
esquina n = comp n rotar dibujoP

-- Lado con nivel de detalle en base a la figura S.
lado :: Int -> Dibujo Escher
lado n = comp n rotar dibujoS

-- -- Impresion de Escher:

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

testAll :: Dibujo Escher
testAll = grilla [
    [esquina 0, lado 0, esquina 3],[lado 1, centroT, lado 3],[esquina 1, lado 2, esquina 2]
    ]


escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = testAll,
    bas = interpBas
}
