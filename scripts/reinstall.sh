#!/bin/bash

ghc-pkg unregister web-mongrel2
cabal clean && cabal install

