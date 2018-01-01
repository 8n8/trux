import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU
import Text.Megaparsec
import qualified Parser as P 

main :: IO ()
main = T.defaultMain tests

tests :: T.TestTree
tests = T.testGroup "TestParser" ok_tests

ok_tests :: [T.TestTree]
ok_tests = [
    HU.testCase "basic inline math" $
        parseMaybe P.parseDocument "$ { x }"
            HU.@?= Just
                "\\documentclass{article}\
                \\\begin{document}\
                \$x$\
                \\\end{document}"
    , HU.testCase "basic display math" $
        parseMaybe P.parseDocument "math { x }"
            HU.@?= Just
                "\\documentclass{article}\
                \\\begin{document}\
                \\\[x\\]\
                \\\end{document}"
    , HU.testCase "math with integral" $
        parseMaybe P.parseDocument "$ { int }"
            HU.@?= Just
                "\\documentclass{article}\
                \\\begin{document}\
                \$\\int $\
                \\\end{document}"
    , HU.testCase "single word" $
        parseMaybe P.parseDocument "`a`"
            HU.@?= Just
                "\\documentclass{article}\
                \\\begin{document}\
                \a\
                \\\end{document}"
    , HU.testCase "author one letter" $
        parseMaybe P.parseDocument "author { `b` }"
            HU.@?= Just
                "\\documentclass{article}\
                \\\author{b}\
                \\\begin{document}\
                \\\maketitle \
                \\\end{document}"
    , HU.testCase "author two names" $
        parseMaybe P.parseDocument "author { `John Smith` }"
            HU.@?= Just
                "\\documentclass{article}\
                \\\author{John Smith}\
                \\\begin{document}\
                \\\maketitle \
                \\\end{document}"
    , HU.testCase "several ordinary words" $
        parseMaybe P.parseDocument "`The cat sat on the mat.`"
            HU.@?= Just
                "\\documentclass{article}\\begin{document}\
                \The cat sat on the mat.\
                \\\end{document}"
    , HU.testCase "ordinary words with special chars" $
        parseMaybe P.parseDocument "`%&`"
            HU.@?= Just
                "\\documentclass{article}\\begin{document}\
                \\\%\\&\
                \\\end{document}"
    , HU.testCase "math followed by words" $
        parseMaybe P.parseDocument "$ { int } `$`" HU.@?= Just
            "\\documentclass{article}\\begin{document}\
            \$\\int $\\$\
            \\\end{document}"
    , HU.testCase "math and words before and after" $
        parseMaybe P.parseDocument "`a` $ { x ^ { 2 } } `b`"
            HU.@?= Just
                "\\documentclass{article}\\begin{document}a$x^{2}$\
                \b\\end{document}"
    ]
