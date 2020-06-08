# GA implementation in Haskell

work in progress. see `example.hs`, or the `ghci` session below, for an example of usage.

```
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
Prelude> :l GAMvector.hs
[1 of 2] Compiling GABlade          ( GABlade.hs, interpreted )
[2 of 2] Compiling GAMvector        ( GAMvector.hs, interpreted )
Ok, two modules loaded.
*GAMvector> -- create null vector (E1 + E2)/sqrt(2) + F1 where E1^2 = E2^2 = +1 and F1^2 = -1
*GAMvector> vec = mv [(1/sqrt(2),"E1")] + mv [(1/sqrt(2),"E2")] + mv [(1,"F1")]
*GAMvector> -- `vec` squares to ~0.0
*GAMvector> vec^2
-2.220446049250313e-16
```