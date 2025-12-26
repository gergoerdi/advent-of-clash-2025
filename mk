#!/bin/sh
hpack && cabal build -v0 && cabal exec -v0 -- aoc2025-shake $@
