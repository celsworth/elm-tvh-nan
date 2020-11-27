#! /bin/bash

while true; do
  echo src/*.elm | entr -d elm make src/Main.elm --output elm.js --optimize
done
