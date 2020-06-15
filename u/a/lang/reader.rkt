#lang s-exp syntax/module-reader u/a
#:info (λ (k v f)
         (case k
           [(unlike-assets:config) #t]
           [else (f k v)]))
