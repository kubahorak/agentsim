#!/bin/bash

projectdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

#sbcl --load "$projectdir"/run.lisp
clisp -i "$projectdir"/run.lisp
