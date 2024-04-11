module Main (main) where

import Test.HUnit (Test (..), assertEqual, Counts, runTestTT)
import Dibujo

data TriORect = Triangulo | Rectangulo deriving (Eq, Show)

figuraTest :: Test
figuraTest = TestCase $ assertEqual "for (figura \"test\")," (figura Triangulo) (figura Triangulo)

-- encimarTest :: Test
-- encimarTest = TestCase $ do
--   let dibujo1 = figura Triangulo
--   let dibujo2 = figura Triangulo
--   assertEqual "for (encimar dibujo1 dibujo2)" (Encimar dibujo1 dibujo2) (encimar dibujo1 dibujo2)

-- rotateTest :: Test
-- rotateTest = TestCase $ do
--   let dibujo = figura Triangulo
--   let result = rotar dibujo
--   assertEqual "for (rotar dibujo)," (Rotar dibujo) result

-- espejarTest :: Test
-- espejarTest = TestCase $ do
--   let dibujo = figura Triangulo
--   let result = espejar dibujo
--   assertEqual "for (espejar dibujo)," (Espejar dibujo) result

-- r90Test :: Test
-- r90Test = TestCase $ do
--   let dibujo = figura Triangulo
--   let result = r90 dibujo
--   assertEqual "for (r90 dibujo)," (Rot45 dibujo) result

-- r180Test :: Test
-- r180Test = TestCase $ do
--   let dibujo = figura Triangulo
--   let result = r180 dibujo
--   assertEqual "for (r180 dibujo)," (Rot45 (Rot45 dibujo)) result

-- r270Test :: Test
-- r270Test = TestCase $ do
--   let dibujo = figura Triangulo
--   let result = r270 dibujo
--   assertEqual "for (r270 dibujo)," (Rot45 (Rot45 (Rot45 dibujo))) result



tests :: Test
tests = TestList [TestLabel "figuraTest" figuraTest]

main :: IO Counts
main = runTestTT tests