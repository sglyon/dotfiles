#!/usr/bin/env sh

wget -O julia_nightly.tar.gz https://status.julialang.org/download/linux-x86_64
rm -rf $HOME/src/julia*
mkdir -p $HOME/src/julia
tar -C $HOME/src/julia -zxvf julia_nightly.tar.gz --strip-components=1

# prepend julia to path
export PATH=$HOME/src/julia/bin:$PATH
