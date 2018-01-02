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
        parseMaybe P.parseDocument "${\nx}"
            HU.@?= Just
                "\\documentclass{article}\
                \\\begin{document}\
                \$x$\
                \\\end{document}"
    , HU.testCase "absolute" $
        parseMaybe P.parseDocument "math{|{x x}}"
            HU.@?= Just
                "\\documentclass{article}\
                \\\begin{document}\
                \\\[\\left|xx\\right|\\]\
                \\\end{document}"
    , HU.testCase "basic display math" $
        parseMaybe P.parseDocument "math{x}"
            HU.@?= Just
                "\\documentclass{article}\
                \\\begin{document}\
                \\\[x\\]\
                \\\end{document}"
    , HU.testCase "math with integral" $
        parseMaybe P.parseDocument "${int}"
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
        parseMaybe P.parseDocument "author{`b`} title{`c`}"
            HU.@?= Just
                "\\documentclass{article}\
                \\\author{b}\
                \\\title{c}\
                \\\begin{document}\
                \\\maketitle \
                \\\end{document}"
    , HU.testCase "author two names" $
        parseMaybe P.parseDocument "author{`John Smith`} title{`d`}"
            HU.@?= Just
                "\\documentclass{article}\
                \\\author{John Smith}\
                \\\title{d}\
                \\\begin{document}\
                \\\maketitle \
                \\\end{document}"
    , HU.testCase "several ordinary words" $
        parseMaybe P.parseDocument "`The cat sat on the mat.`"
            HU.@?= Just
                "\\documentclass{article}\\begin{document}\
                \The cat sat on the mat.\
                \\\end{document}"
    , HU.testCase "checking quote orientation" $
        parseMaybe P.parseDocument "`\"The \'dog\' sat on the cat's mat.\"`"
            HU.@?= Just
                "\\documentclass{article}\\begin{document}\
                \``The `dog\' sat on the cat\'s mat.\"\
                \\\end{document}"
    , HU.testCase "ordinary words with special chars" $
        parseMaybe P.parseDocument "`%&`"
            HU.@?= Just
                "\\documentclass{article}\\begin{document}\
                \\\%\\&\
                \\\end{document}"
    , HU.testCase "math followed by words" $
        parseMaybe P.parseDocument "${int} `$`" HU.@?= Just
            "\\documentclass{article}\\begin{document}\
            \$\\int $\\$\
            \\\end{document}"
    , HU.testCase "math and words before and after" $
        parseMaybe P.parseDocument "`a` ${x ^{2}} `b`"
            HU.@?= Just
                "\\documentclass{article}\\begin{document}a$x^{2}$\
                \b\\end{document}"
    , HU.testCase "long example" $
        parseMaybe P.parseDocument longerExampleInput 
            HU.@?= Just longerExampleOutput
    , HU.testCase "math brackets" $
        parseMaybe P.parseDocument "${( x ) y}" 
            HU.@?= Just 
                "\\documentclass{article}\\begin{document}\
                \$\\left(x\\right)y$\\end{document}"
    ]

longerExampleInput :: String
longerExampleInput =
    "author{`True Ghiassi`}\n\
    \title{`The massive Trux`}\n\
    \`As the sensors read in the measurements, ` ${b P} ` and ` \
    \${b m} ` are continually updated according to these \
    \equations:`\n\
    \math{\n\
    \P _{k | k} = x ^{2} + 2\n\
    \}\n\
    \` and `\n\
    \math{\n\
    \2 x ^{3} + 32 = 233\n\
    \}"

longerExampleOutput :: String
longerExampleOutput =
    "\\documentclass{article}\
    \\\title{The massive Trux}\
    \\\author{True Ghiassi}\
    \\\begin{document}\
    \\\maketitle \
    \As the sensors read in the measurements, $bP$ and $bm$ are \
    \continually updated according to these equations:\
    \\\[P_{k|k}=x^{2}+2\\] and \\[2x^{3}+32=233\\]\\end{document}"
