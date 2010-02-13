#!/bin/sh

cd tests
rm -f *.hi *.o Benchamrk
ghc --make -O2 -package-conf ../pkg.conf.d Benchmark.hs -o Benchmark
./Benchmark

