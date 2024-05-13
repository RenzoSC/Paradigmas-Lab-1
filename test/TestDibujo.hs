module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit

import Dibujo

data TriORect = Triangulo | Rectangulo deriving (Eq, Show)

testFigura :: Test
testFigura =
    TestCase $ assertEqual "figura" (show $ figura (5 :: Double)) "Figura 5.0"

testRotar :: Test
testRotar =
    TestCase $ assertEqual "rotar" (show $ rotar (figura (5 :: Double))) "Rotar (Figura 5.0)"

-- testApilar :: Test
-- testApilar =
--     TestCase $ assertEqual "apilar" (show $ apilar 1 1 (figura (5 :: Double)) (figura (5 :: Double)))
--                  "Apilar 1.0 1.0 (Figura 5.0) (Figura 5.0)"

-- testEncimar :: Test
-- testEncimar =
--     TestCase $ assertEqual "encimar" (show $ encimar (figura (5 :: Double)) (figura (5 :: Double)))
--                  "Encimar (Figura 5.0) (Figura 5.0)"

testRot45 :: Test
testRot45 =
    TestCase $ assertEqual "rot45" (show $ rot45 (figura (5 :: Double))) "Rot45 (Figura 5.0)"

-- testJuntar :: Test
-- testJuntar =
--     TestCase $ assertEqual "juntar" (show $ juntar 1 1 (figura (5 :: Double)) (figura (5 :: Double)))
--                  "Juntar 1.0 1.0 (Figura 5.0) (Figura 5.0)"

testEspejar :: Test
testEspejar =
    TestCase $ assertEqual "espejar" (show $ espejar (figura (5 :: Double))) "Espejar (Figura 5.0)"

testR90 :: Test
testR90 =
    TestCase $ assertEqual "r90" (show $ r90 (figura (5 :: Double))) "Rot45 (Rotar (Figura 5.0))"

testR180 :: Test
testR180 =
    TestCase $ assertEqual "r180" (show $ r180 (figura (5 :: Double))) "Rot45 (Rot45 (Rotar (Figura 5.0)))"

testR270 :: Test
testR270 =
    TestCase $ assertEqual "r270" (show $ r270 (figura (5 :: Double))) "Rot45 (Rot45 (Rot45 (Rotar (Figura 5.0)))"

testUpUpUp :: Test
testUpUpUp =
    TestCase $ assertEqual "^^^" (show $ figura (5 :: Double) ^^^ figura (5 :: Double))
                 "Encimar (Figura 5.0) (Figura 5.0)"

testSlashSlashSlash :: Test
testSlashSlashSlash =
    TestCase $ assertEqual "///" (show $ figura (5 :: Double) /// figura (5 :: Double))
                 "Juntar 1.0 1.0 (Figura 5.0) (Figura 5.0)"

testDotDotDot :: Test
testDotDotDot =
    TestCase $ assertEqual ".-." (show $ figura (5 :: Double) .-. figura (5 :: Double))
                 "Apilar 1.0 1.0 (Figura 5.0) (Figura 5.0)"

testComp :: Test
testComp =
    TestCase $ assertEqual "comp" (show $ comp 2 rotar (figura (5 :: Double)))
                 "Rotar (Rotar (Figura 5.0))"

testEncimar4 :: Test
testEncimar4 =
    TestCase $ assertEqual "encimar4" (show $ encimar (encimar (figura (5 :: Double)) (figura (5 :: Double))) (encimar (figura (5 :: Double)) (figura (5 :: Double))))
                 "Encimar (Encimar (Encimar (Figura 5.0) (Figura 5.0)) (Figura 5.0)) (Figura 5.0)"

testCuarteto :: Test
testCuarteto =
    TestCase $ assertEqual "cuarteto" (show $ cuarteto (figura (5 :: Double)) (figura (5 :: Double)) (figura (5 :: Double)) (figura (5 :: Double)))
                 "Apilar 1.0 1.0 (Juntar 1.0 1.0 (Figura 5.0) (Figura 5.0)) (Juntar 1.0 1.0 (Figura 5.0) (Figura 5.0))"


tests :: Test
tests = TestList [ TestLabel "testFigura" testFigura
                    , TestLabel "testRotar" testRotar
                    , TestLabel "testApilar" testApilar
                    , TestLabel "testEncimar" testEncimar
                    , TestLabel "testRot45" testRot45
                    , TestLabel "testJuntar" testJuntar
                    , TestLabel "testEspejar" testEspejar
                    , TestLabel "testR90" testR90
                    , TestLabel "testR180" testR180
                    , TestLabel "testR270" testR270
                    , TestLabel "testUpUpUp" testUpUpUp
                    , TestLabel "testSlashSlashSlash" testSlashSlashSlash
                    , TestLabel "testDotDotDot" testDotDotDot
                    , TestLabel "testComp" testComp
                    , TestLabel "testEncimar4" testEncimar4
                    , TestLabel "testCuarteto" testCuarteto
                    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
