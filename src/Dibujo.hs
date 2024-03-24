module Dibujo (Dibujo,
  comp, figura,
  encimar, apilar,
  juntar, rot45,
  rotar, espejar,
  (^^^),(///),
  (.-.), r90,
  r180, r270,
  encimar4, cuarteto,
  ciclar, mapDib,
  change, foldDib,
    -- agregar las funciones constructoras
    ) where
      
import Data.Monoid (Ap(Ap))


-- nuestro lenguaje 
data Dibujo a = Vacio | Figura a | Rotar (Dibujo a) 
              | Espejar (Dibujo a) | Rot45 (Dibujo a) 
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
              deriving(Eq,Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp 0 _ a = a
comp n f a = f ( comp (n-1) f a)


-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar


espejar :: Dibujo a -> Dibujo a
espejar = Espejar

(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = encimar

(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = apilar 1 1

(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = juntar 1 1 

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = comp 2 rot45

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 r90

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 r90

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dib = (^^^) dib ((^^^) (r90 dib) ((^^^) (r180 dib) (r270 dib))) 

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto x y z w = (.-.) ((///) x y) ((///) z w)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar = undefined

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib = undefined
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change = undefined

-- Principio de recursión para Dibujos.
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib = undefined