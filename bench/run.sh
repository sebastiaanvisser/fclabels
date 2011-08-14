mkdir dist
ghc -i../src -odir dist -hidir dist  --make -O Benchmark.hs -o dist/benchmark -fforce-recomp
dist/benchmark
