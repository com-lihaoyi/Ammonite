#!/usr/bin/env bash
set -euxv

# Mangle ssh-key stuff to work with the deploy_key written by python
eval "$(ssh-agent -s)"
chmod 600 deploy_key
ssh-add deploy_key
rm deploy_key

# Set up git so we can actually do things with it
git config --global user.email "haoyi.sg+travis@gmail.com"
git config --global user.name "Ammonite Travis Bot"
git fetch origin gh-pages:gh-pages

# Do all the git-foo to push the readme to the relevant gh-pages folder
git checkout -f gh-pages
cp -r readme/target/scalatex/* .
git add -A
git commit -am $(git rev-parse HEAD)
git push git@github.com:$GITHUB_REPOSITORY.git gh-pages:gh-pages
