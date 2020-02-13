2711
((3) 0 () 5 ((q lib "unlike-assets/policy.rkt") (q lib "unlike-assets/logging.rkt") (q lib "unlike-assets/reactive.rkt") (c (? . 4) q unlike-compiler%) (q lib "unlike-assets/main.rkt")) () (h ! (equal) ((c def c (c (? . 0) q file-url?)) q (4198 . 3)) ((c def c (c (? . 0) q block)) q (4641 . 2)) ((c meth c (c (? . 3) q clarify)) q (1348 . 3)) ((c def c (c (? . 1) q <log/cm)) q (2928 . 5)) ((c def c (c (? . 1) q format-clear)) q (3801 . 2)) ((c def c (c (? . 1) q with-report/void)) q (3241 . 3)) ((c def c (c (? . 0) q clarify/multi)) q (3949 . 4)) ((c def c (c (? . 2) q wait-for-u/a-cell!)) q (748 . 4)) ((c def c (c (? . 1) q <log)) q (3049 . 5)) ((c def c (c (? . 1) q show-all-events?)) q (3546 . 2)) ((c def c (c (? . 1) q format-unclear)) q (3873 . 2)) ((c def c (c (? . 0) q build-complete-simple-path)) q (4332 . 5)) ((c def c (c (? . 1) q <info)) q (2642 . 4)) ((c def c (c (? . 0) q rebuild)) q (4666 . 2)) ((c def c (c (? . 2) q start-live-build)) q (120 . 7)) ((c def c (c (? . 2) q maybe-build-u/a-cell!)) q (640 . 4)) ((c def c (? . 3)) q (1289 . 3)) ((c def c (c (? . 4) q clear/c)) q (968 . 3)) ((c def c (c (? . 0) q file-readable?)) q (4266 . 3)) ((c def c (c (? . 2) q u/a-build-system?)) q (0 . 3)) ((c meth c (c (? . 3) q delegate)) q (1439 . 3)) ((c def c (c (? . 1) q with-report)) q (3168 . 3)) ((c def c (c (? . 1) q error-port-levels)) q (3657 . 3)) ((c def c (c (? . 4) q unclear/c)) q (940 . 2)) ((c meth c (c (? . 3) q add!)) q (1963 . 7)) ((c def c (c (? . 1) q <warning)) q (2831 . 4)) ((c def c (c (? . 4) q ripple/c)) q (2275 . 3)) ((c def c (c (? . 1) q unlike-assets-logger)) q (2370 . 2)) ((c def c (c (? . 2) q get-live-build)) q (542 . 4)) ((c def c (c (? . 1) q with-report/counts)) q (3312 . 3)) ((c def c (c (? . 1) q show-level?)) q (3604 . 2)) ((c def c (c (? . 1) q <error)) q (2547 . 4)) ((c def c (c (? . 1) q make-child-logger)) q (2409 . 2)) ((c def c (c (? . 0) q chain)) q (4547 . 4)) ((c def c (c (? . 1) q show-colors?)) q (3438 . 2)) ((c def c (c (? . 1) q prescribed-prefix)) q (3743 . 2)) ((c def c (c (? . 2) q live-build?)) q (63 . 3)) ((c def c (c (? . 2) q procure-u/a!)) q (850 . 4)) ((c def c (c (? . 1) q <debug)) q (2736 . 4)) ((c meth c (c (? . 3) q compile!)) q (1532 . 8)) ((c def c (c (? . 2) q make-u/a-build-system)) q (393 . 3)) ((c meth c (c (? . 3) q lookup)) q (1872 . 3)) ((c def c (c (? . 4) q advance/c)) q (1105 . 4)) ((c def c (c (? . 0) q local-asset-url?)) q (4130 . 3)) ((c def c (c (? . 1) q <fatal)) q (2452 . 4)) ((c def c (c (? . 1) q show-debug?)) q (3385 . 2)) ((c def c (c (? . 4) q fulfilled/c)) q (1063 . 2)) ((c def c (c (? . 1) q show-prefix?)) q (3492 . 2)) ((c def c (c (? . 4) q unlike-asset/c)) q (1233 . 2))))
procedure
(u/a-build-system? p) -> boolean?
  p : any/c
procedure
(live-build? p) -> boolean?
  p : any/c
procedure
(start-live-build  sample-change     
                   respond           
                  [suppress?])   -> live-build?
  sample-change : (-> any/c)
  respond : (-> any/c any/c)
  suppress? : (-> any/c any/c boolean?) = eq?
procedure
(make-u/a-build-system key->live-build) -> u/a-build-system?
  key->live-build : (-> any/c (-> any/c live-build?) live-build?)
procedure
(get-live-build sys u) -> live-build?
  sys : u/a-build-system?
  u : any/c
procedure
(maybe-build-u/a-cell! sys u) -> stateful-cell?
  sys : u/a-build-system?
  u : any/c
procedure
(wait-for-u/a-cell! sys u) -> (-> any/c)
  sys : u/a-build-system?
  u : any/c
procedure
(procure-u/a! sys u) -> any/c
  sys : u/a-build-system?
  u : any/c
value
unclear/c : string?
value
clear/c
 : (or/c string? symbol? url? path? (generic-instance/c gen:equal+hash))
value
fulfilled/c : (not/c procedure?)
value
advance/c
 : (recursive-contract
     (-> clear/c (instanceof/c (subclass?/c unlike-compiler%)) unlike-asset/c))
value
unlike-asset/c : (or/c advance/c fulfilled/c)
class
unlike-compiler% : class?
  superclass: object%
method
(send an-unlike-compiler clarify unclear) -> clear/c
  unclear : unclear/c
method
(send an-unlike-compiler delegate clear) -> unlike-asset/c
  clear : clear/c
method
(send an-unlike-compiler compile! [#:changed changed   
                                   #:removed removed   
                                   #:strict? strict?]) 
 -> (hash/c clear/c fulfilled/c)
  changed : (listof clear/c) = null
  removed : (listof clear/c) = null
  strict? : any/c = #t
method
(send an-unlike-compiler lookup clear) -> unlike-asset/c
  clear : clear/c
method
(send an-unlike-compiler add!  clear               
                              [dependent-clear     
                               ripple])        -> void?
  clear : clear/c
  dependent-clear : (or/c clear/c boolean?) = #f
  ripple : (or/c ripple/c boolean?) = #f
value
ripple/c
 : (-> clear/c clear/c (non-empty-listof unlike-asset/c) unlike-asset/c)
value
unlike-assets-logger : logger?
procedure
(make-child-logger) -> logger?
procedure
(<fatal message-fmt v ...) -> void?
  message-fmt : string?
  v : any/c
procedure
(<error message-fmt v ...) -> void?
  message-fmt : string?
  v : any/c
procedure
(<info message-fmt v ...) -> void?
  message-fmt : string?
  v : any/c
procedure
(<debug message-fmt v ...) -> void?
  message-fmt : string?
  v : any/c
procedure
(<warning message-fmt v ...) -> void?
  message-fmt : string?
  v : any/c
procedure
(<log/cm level message v ...) -> void?
  level : log-level/c
  message : string?
  v : any/c
procedure
(<log level message data) -> void?
  level : log-level/c
  message : string?
  data : any/c
procedure
(with-report proc) -> any/c dict?
  proc : (-> any/c)
procedure
(with-report/void proc) -> void?
  proc : (-> any/c)
procedure
(with-report/counts proc) -> dict?
  proc : (-> any/c)
value
show-debug? : (parameter/c boolean?) = #f
value
show-colors? : (parameter/c boolean?) = #f
value
show-prefix? : (parameter/c boolean?) = #f
value
show-all-events? : (parameter/c boolean?) = #f
value
show-level? : (parameter/c boolean?) = #f
value
error-port-levels : (parameter/c (listof log-level/c))
 = '(fatal error)
value
prescribed-prefix : (parameter/c string?) = ""
value
format-clear : (parameter/c (-> clear/c any/c)) = identity
value
format-unclear : (parameter/c (-> unclear/c any/c)) = identity
procedure
(clarify/multi compiler unclear-names) -> (listof clear/c)
  compiler : (instanceof/c (subclass?/c unlike-compiler%))
  unclear-names : (listof unclear/c)
procedure
(local-asset-url? str) -> boolean?
  str : string?
procedure
(file-url? url-inst) -> boolean?
  url-inst : url?
procedure
(file-readable? path) -> boolean?
  path : path?
procedure
(build-complete-simple-path  path              
                            [relative-to]) -> complete-path?
  path : (or/c string? path?)
  relative-to : (or/c path? boolean?) = #f
procedure
(chain proc args ...) -> procedure?
  proc : procedure?
  args : any/c
value
block : ripple/c
value
rebuild : ripple/c
