mkdir dist
ghc -odir dist -hidir dist  --make -O Benchmark.hs -o dist/benchmark -fforce-recomp
dist/benchmark
