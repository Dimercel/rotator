#!/bin/sh

sbcl --no-userinit --no-sysinit --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval '(ql:quickload "rotator")' \
     --eval '(ql:write-asdf-manifest-file "rotator-manifest.txt")'

buildapp --manifest-file rotator-manifest.txt --load-system rotator \
         --output rotator-exe \
         --entry rotator:main

rm rotator-manifest.txt
