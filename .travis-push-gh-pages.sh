#!/bin/bash

# inspired by https://gist.github.com/domenic/ec8b0fc8ab45f39403dd

mkdir gh-page

cp index.html gh-page/index.html
cp arbtt.js arbtt.css gh-page/
cp -vr doc/users_guide gh-page

echo 'arbtt.nomeata.de' > gh-page/CNAME

# Prepare an empty directory
cd gh-page
git init
git config user.name "arbtt CI"
git config user.email "mail@joachim-breitner.de"
git add .
git commit -m "Deploy to GitHub Pages"
# The diversion to /dev/null is required to keep the GH_TOKEN secret
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master:gh-pages > /dev/null 2>&1


