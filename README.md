`unlike-assets` is a build system for arbitrary interdependent assets.

I use this as an alternative to Webpack for my purposes.

```
$ raco pkg install unlike-assets
$ raco doc unlike-assets # To view docs
```

To run tests, run `raco test **/*.test.rkt` in the repository top-level directory.

### TODO

- Pick a better data structure in `inbox.rkt`.
- Port to `typed/racket`.
- Make something like this possible:

```
(require unlike-assets
         (for-syntax "ecmascript.rkt"))
(require (unlike ecmascript->racket-module
                 [client "./client.js"]
                 [server "./server.js"]))

(bundle client)
(minify server)
```
