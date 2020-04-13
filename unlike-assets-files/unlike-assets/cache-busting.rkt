#lang racket/base

(require unlike-assets/conventions)

(define (compute-name file-path)
  (call-with-input-file file-path
    (λ (port)
      (path-replace-extension (substring (sha1 port) 0 8)
                              (path-get-extension file-path)))))

(define (key->maybe-build key recurse)
  (and (file-exists? key)
       (start-live-build!
        key
        #:suppress? equal?
        #:sample!
        (λ ()
          (with-handlers ([exn:fail:filesystem? values])
            (file-or-directory-modify-seconds key)))
        #:build!
        (λ (mtime-or-exn)
          (if (exn? mtime-or-exn)
              mtime-or-exn
              (asset [input-file-path key]
                     [output-file-path (compute-name key)]
                     [->http-response
                      (response/output #:mime-type #"application/octet-stream"
                                       (λ (out)
                                         (call-with-input-file file-path
                                           (λ (in) (copy-port in out)))))]))))))
