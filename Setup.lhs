#! /usr/bin/env runhaskell

> import Distribution.Simple
> main = defaultMainWithHooks simpleUserHooks { postBuild = (const . const . const . const) (putStrLn "mplo") }

