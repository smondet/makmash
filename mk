#!/bin/sh

findlib_packages=omd,sosa,nonstd,higlo.ocaml,ppx_blob

build () {
    directories=`find src -type d`
    files=`find src -type f`
    for dir in $directories ; do
        mkdir -p _build/$dir
    done;
    for file in $files ; do
        cp $file _build/$file
    done;
    cd _build
    ocamlfind opt -package $findlib_packages -rectypes -linkpkg src/main.ml -o ../makmash
}
merlinize () {
    echo "B _build" > .merlin
    echo "S src" >> .merlin
    for pkg in `echo $findlib_packages | sed 's/,/ /g'` ; do
        echo "PKG $pkg" >> .merlin
    done;
}


case $1 in
    "build" | "" )
        build
        echo Done ;;
    "merlinize" )
        merlinize ;;
    * ) echo "Can't understand $1" ;;
esac
