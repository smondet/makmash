#!/bin/sh

directories=`find src -type d`
files=`find src -type f`
for dir in $directories ; do
    mkdir -p _build/$dir
done;
for file in $files ; do
    cp $file _build/$file
done;
cd _build
ocamlfind opt -package omd,sosa,nonstd,higlo.ocaml,ppx_blob -rectypes -linkpkg src/main.ml -o ../sinai-slides

