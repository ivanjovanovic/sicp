#!/bin/bash

DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

open http://mitpress.mit.edu/sicp/full-text/book/book.html
mvim $DIR/../
