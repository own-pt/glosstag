#!/bin/bash

sbcl --eval "(ql:quickload :glosstag)" --eval "(in-package :glosstag)" --eval '(process-corpus "~/work/wn/glosstag/data/*.plist" "ar.data" "query.csv")' --eval '(sb-ext:exit)'
