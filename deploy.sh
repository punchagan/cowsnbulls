#!/bin/bash
elm-make cows_n_bulls.elm --output=output/index.html
cp word_list.txt output/
echo "cowsnbulls.in" > output/CNAME
ghp-import -p output/
