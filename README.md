diagrams-nanovg
===============

diagrams-nanovg is a nanovg backend for diagrams based on the nanovg haskell bindings. 
Diagrams is a powerful, flexible, declarative domain-specific language for 
creating vector graphics, using the Haskell programming language.
It supports most features defined in [diagrams-lib].

[diagrams-lib]: http://hackage.haskell.org/package/diagrams%2Dlib

# Usage

A simple example that uses _diagrams-nanovg_ to draw a square.

```haskell
import Diagrams.Prelude
import Diagrams.Backend.NanoVG.CmdLine

b1 = square 20 # lw 0.002

main = mainWith (pad 1.1 b1)
```

Save this to file named `Square.hs` and compile this program:

```
ghc --make -threaded Square.hs
```
