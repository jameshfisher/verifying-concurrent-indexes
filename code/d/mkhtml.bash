#!/bin/bash
for v in `find . -name "*.d"`
do
  dir=${v%/*}
  mkdir -p html/$dir
  cat $v | ./mkhtml/mkhtml > html/$v.html
done

