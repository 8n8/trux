#!/bin/bash
nuweb -rl code.w
latexmk -pdf code.tex
nuweb -rl code.w
latexmk -pdf code.tex
