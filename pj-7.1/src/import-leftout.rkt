#lang racket/base

;; Import counts / reasons for left-out (omitted) tests.
;;
;; Usage:
;;   racket import-leftout.rkt
;;
;; Prints a bunch of TeX `\item`s.

;; ---

(require
  (only-in "import-tsv.rkt" tex-escape)
  racket/string
  racket/runtime-path)

(define-runtime-path left-out.csv "../../left-out_reason.csv")

(define (comma-split str)
  (string-split str ","))

(define (parse csv)
  (with-input-from-file
    csv
    (lambda ()
      (for/vector ((ln (in-lines)))
        (for/vector ((str (in-list (comma-split ln))))
          (or (string->number str) str))))))

(define (get-reason-totals* tbl**)
  (define titlev (vector-ref tbl** 0))
  (define N (vector-length titlev))
  (define totalv (make-vector N 0))
  (for* ((row-idx (in-range 1 (vector-length tbl**)))
         (col-idx (in-range 1 N)))
    (vector-set! totalv col-idx (+ (vector-ref totalv col-idx)
                                   (vector-ref (vector-ref tbl** row-idx) col-idx)))
    (void))
  (values titlev totalv))

(define (get-reason-totals tbl**)
  (define-values [titlev totalv] (get-reason-totals* tbl**))
  (define N (vector-length titlev))
  (for/hash ((k (in-vector titlev))
             (v (in-vector totalv)))
    (values k v)))

(define (top-reasons tbl0**)
  (define too-small 0)
  (let loop ([tbl** tbl0**])
    (define-values [titlev totalv] (get-reason-totals* tbl**))
    (printf "tbl N ~a~n ~a~n ~a~n" (vector-length tbl**) titlev totalv)
    (define-values [top-title top-total]
      (for/fold ([tt #f]
                 [tn #f])
                ([curr-t (in-vector titlev)]
                 [curr-n (in-vector totalv)])
        (if (or (not tn) (< tn curr-n))
          (values curr-t curr-n)
          (values tt tn))))
    (if (<= top-total too-small)
      (let ((h (for/hash ([curr-t (in-vector titlev)]
                          [curr-n (in-vector totalv)]
                          #:when (< 0 curr-n))
                         (values curr-t curr-n))))
        (newline)
        (pretty-write tbl**)
        #;(for ((kv (in-list (sort-reason-hash h))))
          (printf "  \\item ~a use ~a~n" (cdr kv) (car kv))))
      (let ((top-idx (vindex-of titlev top-title)))
        #;(printf "  \\item ~a use ~a~n" top-total top-title)
        (loop (delete-column
                (delete-row* tbl** (lambda (r)
                                     (define n (vector-ref r top-idx))
                                     (and (real? n) (< 0 n))))
                top-idx))))))

(define (delete-row* tbl delete?)
  (for/vector ((r (in-vector tbl))
               #:unless (delete? r))
    r))

(define (delete-column tbl i)
  (for/vector ((r (in-vector tbl)))
    (vector-remove r i)))

(define (vector-remove v i)
  (build-vector
    (sub1 (vector-length v))
    (lambda (k)
      (vector-ref v (+ k (if (< k i) 0 1))))))

(define (vindex-of v i)
  (for/first ((k (in-range (vector-length v)))
              #:when (equal? i (vector-ref v k)))
    k))

(define (order2 kv0 kv1)
  (or (> (cdr kv0) (cdr kv1))
      (and (= (cdr kv0) (cdr kv1))
           (string<=? (car kv0) (car kv1)))))

(define (sort-reason-hash h)
 (filter (lambda (kv) (< 0 (cdr kv))) (sort (hash->list h) order2)))

(define (print-table h)
  (define kv* (sort-reason-hash h))
  (printf "  \\begin{tabular}{lr}~n")
  (printf "    Filter & Tests Caught \\\\\\hline~n")
  (for ((kv (in-list kv*)))
    (printf "    ~a & ~a \\\\~n" (tex-escape (car kv)) (cdr kv))
    (void))
  (printf "  \\end{tabular}~n")
  (void))

;; ---

(module+
  main
  (require racket/cmdline)
  (unless (file-exists? left-out.csv)
    (raise-argument-error 'import-leftout "file-exists?" left-out.csv))
  (define tbl** (parse left-out.csv))
  #;(print-table (get-reason-totals tbl**))
  (top-reasons tbl**)

  (void))


