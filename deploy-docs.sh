#!/bin/sh
git checkout master
stack haddock --no-haddock-deps --fast
doc_root="$(stack path --local-doc-root)"/recursion-schemes-ix
git branch -D gh-pages
git checkout --orphan gh-pages
rm -rf ./*
cp -r "$doc_root"/* .
git add .
git commit -m "Automatic Haddock commit"
git push -fu origin gh-pages
git checkout master
