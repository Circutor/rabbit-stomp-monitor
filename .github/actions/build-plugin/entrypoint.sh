#!/usr/bin/env bash

set -e -u -o pipefail

SRC_DIR=$1

TMP_DIR=/tmp/app
cp -r "$SRC_DIR" $TMP_DIR

cd $TMP_DIR
make distclean
DIST_AS_EZS=yes make dist

cp -r $TMP_DIR/plugins "$SRC_DIR"
