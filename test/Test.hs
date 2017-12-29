import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU
import Text.Megaparsec
import qualified Parser as P 

main :: IO ()
main = T.defaultMain tests

tests :: T.TestTree
tests = T.testGroup "TestParser"
    [ HU.testCase "basic inline math" $
        parseMaybe P.combinedParser "$ { x }" HU.@?= Just "$x$"
    ]

    -- [ HU.testCase "single word" $
    --     parseMaybe P.combinedParser "a" HU.@?= Just "a"
    -- , HU.testCase "author one letter" $
    --     parseMaybe P.combinedParser "author { b }"
    --         HU.@?= Just "\\author{b}"
    -- , HU.testCase "author two names" $
    --     parseMaybe P.combinedParser "author { John Smith }"
    --         HU.@?= Just "\\author{John Smith}"
    -- , HU.testCase "several ordinary words" $
    --     parseMaybe P.combinedParser "The cat sat on the mat."
    --         HU.@?= Just "The cat sat on the mat."
    -- , HU.testCase "ordinary words with special chars" $
    --     parseMaybe P.combinedParser "%&" HU.@?= Just "\\%\\&"
