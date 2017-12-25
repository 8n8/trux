import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU
import Text.Megaparsec
import qualified Parser as P 

main :: IO ()
main = T.defaultMain tests

tests :: T.TestTree
tests = T.testGroup "TestParser"
    [ HU.testCase "single atom" $
        parseMaybe P.parseDocument "a" HU.@?= Just [P.Atom "a"]
    , HU.testCase "simple function" $
        parseMaybe P.parseDocument "$ { b }" HU.@?= Just simpleFunction
    , HU.testCase "nested function" $
        parseMaybe P.parseDocument "$ { b $ { c d } } e" HU.@?= Just nestedFunc
    ]
         
nestedFunc :: [P.Node]
nestedFunc = [P.Function P.InlineMath [P.Atom "b", P.Function P.InlineMath [P.Atom "c", P.Atom "d"]], P.Atom "e"]

simpleFunction :: [P.Node]
simpleFunction = [P.Function P.InlineMath [P.Atom "b"]]
