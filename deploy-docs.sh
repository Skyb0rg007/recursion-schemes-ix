#!/bin/sh
git checkout master
stack haddock --no-haddock-deps --fast
pkg="$(grep "name:" package.yaml | awk '{ print $2 }')"
vsn="$(grep "version:" package.yaml | awk '{ print $2 }')"
doc_root="$(stack path --local-doc-root)/$pkg-$vsn"
if [ ! -d "$doc_root" ]; then echo "Could not find documentation for $pkg" >&2; exit 1; fi
git branch -D gh-pages
git checkout --orphan gh-pages
rm -rf ./src ./test ChangeLog.md LICENSE package.yaml README.md recursion-schemes-ix.cabal Setup.hs stack.yaml stack.yaml.lock
cp -r "$doc_root"/* .
git add .
git commit -m "Automatic Haddock commit"
git push -fu origin gh-pages
git checkout master
