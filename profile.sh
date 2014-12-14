#!/bin/bash
cabal configure --enable-executable-profiling
cabal build
cabal run -- +RTS -p
less pentago.prof
