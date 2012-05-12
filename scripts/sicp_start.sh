#!/bin/bash

DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT=$DIR/../

open http://mitpress.mit.edu/sicp/full-text/book/book.html
cd $ROOT
mvim $ROOT
