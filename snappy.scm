(import chicken.foreign
        (only chicken.string conc)
        (only chicken.memory.representation number-of-bytes))

(foreign-declare "
#include <snappy-c.h>
")

(define-foreign-type snappy_status int)

(define (check-status x msg data)
  (cond
   ((eq? x (foreign-value "SNAPPY_OK" int)) 'snappy_ok)
   ((eq? x (foreign-value "SNAPPY_INVALID_INPUT" int)) (error msg 'snappy-invalid-input data))
   ((eq? x (foreign-value "SNAPPY_BUFFER_TOO_SMALL" int)) (error msg 'snappy-buffer-too-small data))
   (else (error "unknown snappy status code" x data))))

(define (snappy-uncompressed-length cmp)
  (let-location ((result size_t 0))
    (check-status
     ((foreign-lambda snappy_status "snappy_uncompressed_length"
                      scheme-pointer size_t (c-pointer size_t))
      cmp (number-of-bytes cmp) (location result))
     "could not determine uncompressed length" cmp)
    result))

(define (snappy-uncompress cmp)
  (let-location ((dstlen size_t (snappy-uncompressed-length cmp)))
    (let ((dst (make-string dstlen)))

      (check-status
       ((foreign-lambda snappy_status
                        "snappy_uncompress"
                        scheme-pointer      ;; compressed
                        size_t              ;; compressed_length
                        scheme-pointer      ;; uncompressed
                        (c-pointer size_t)) ;; uncompressed_length
        cmp (number-of-bytes cmp)
        dst (location dstlen))
       "could not uncompress data (snappy_uncompress)" cmp)

      (if (= dstlen (string-length dst))
          dst
          (substring dst 0 dstlen)))))

(define (snappy-max-compressed-length size)
  ((foreign-lambda size_t "snappy_max_compressed_length" size_t)
   size))

(define (snappy-compress raw)
  (let ((rawlen (number-of-bytes raw)))
    (let-location ((dstlen size_t (snappy-max-compressed-length rawlen)))
      (let ((dst (make-string dstlen)))

        (check-status
         ((foreign-lambda snappy_status "snappy_compress"
                          scheme-pointer      ;; input
                          size_t              ;; input_length
                          scheme-pointer      ;; compressed
                          (c-pointer size_t)) ;; compressed_length
          raw rawlen
          dst (location dstlen))
         "could not compress" raw)

        (if (= dstlen (string-length dst))
            dst
            (substring dst 0 dstlen))))))
