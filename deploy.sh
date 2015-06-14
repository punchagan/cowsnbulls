#!/bin/bash
COMMIT_SHA=`git log --oneline -- word_list.txt|head -n 1|cut -f 1 -d " "`
WORD_LIST="word_list_${COMMIT_SHA}.txt"

echo "Changing word_list.txt to ${WORD_LIST}"
sed -i -e "s/word_list.txt/${WORD_LIST}/g" cows_n_bulls.elm
elm-make cows_n_bulls.elm --output=output/index.html
git checkout -- cows_n_bulls.elm

rm output/word_list*.txt
cp word_list.txt "output/${WORD_LIST}"

echo "cowsnbulls.in" > output/CNAME

ghp-import -p output/
