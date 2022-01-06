#lang racket/base

(require
  racket/list
  racket/string
  (only-in math/statistics median))

;; > Out Static
;; > “imports in that module that import from another static module”
;; >
;; > Out Nonstatic
;; > “imports in that module that import from a nonstatic module”
;; >
;; > In Static
;; > “imports in other static modules in the codebase that import this
;; > static module,”
;; >
;; > In Nonstatic
;; > “imports in nonstatic modules that import this static module.”
;;
;; Out & In  refer to the direction of a dependency arrow relative to this
;; module, not the location of the import “in” or “out” of this module. So I
;; did label the columns correctly in order; your labels here are backwards
;; from what the columns mean. The “out” columns are import dependencies “out”
;; from the current module to some other module; the “in” ones are import
;; dependencies “in” to this module from some other module.

;; stat
; with generated modes
;    medians
;     Out Static 3
;     Out Nonstatic 12
;     In Static 0
;     In Nonstatic 2
;    sums
;     Out Static 1652
;     Out Nonstatic 8508
;     In Static 1652
;     In Nonstatic 22001
;    top5
;     Out Static (23 22 21 19 18)
;     Out Nonstatic (281 202 165 140 136)
;     In Static (447 438 412 55 26)
;     In Nonstatic (4254 2621 2055 1562 1480)
;    min5
;     Out Static (0 0 0 0 0)
;     Out Nonstatic (0 2 3 3 3)
;     In Static (0 0 0 0 0)
;     In Nonstatic (0 0 0 0 0)
;
; without generated modules
;    medians
;     Out Static 1
;     Out Nonstatic 13
;     In Static 0
;     In Nonstatic 6
;    sums
;     Out Static 419
;     Out Nonstatic 3576
;     In Static 1641
;     In Nonstatic 21314
;    top5
;     Out Static (23 22 21 19 18)
;     Out Nonstatic (281 202 165 140 136)
;     In Static (447 438 412 55 26)
;     In Nonstatic (4254 2621 2055 1562 1480)
;    min5
;     Out Static (0 0 0 0 0)
;     Out Nonstatic (0 2 3 3 3)
;     In Static (0 0 0 0 0)
;     In Nonstatic (0 0 0 1 1)



(define (parse-tsv fn)
  (with-input-from-file
    fn
    (lambda ()
      (define title* (tab-split (string-trim (read-line))))
      (define n**
        (filter values #;(lambda (n*) (not (and (= 3 (car n*)) (= 12 (cadr n*)))))
          (for/list ((ln (in-lines)))
            (map string->number (tab-split (string-trim ln))))))
      (for/list ((t (in-list title*))
                 (i (in-naturals)))
        (cons t (map (lambda (r) (list-ref r i)) n**))))))

(define (tab-split str)
  (string-split str "\t"))

(define (stat fn)
  (define tsv (parse-tsv fn))
  (printf "stat ~a~n" fn)
  (stat-tsv tsv))

(define (stat-tsv tsv)
  (printf "medians~n")
  (void
    (for ((r (in-list tsv)))
      (printf " ~a ~a~n" (car r) (median < (cdr r)))))
  (printf "sums~n")
  (void
    (for ((r (in-list tsv)))
      (printf " ~a ~a~n" (car r) (apply + (cdr r)))))
  (printf "top5~n")
  (void
    (for ((r (in-list tsv)))
      (printf " ~a ~a~n" (car r) (take (sort (cdr r) >) 5))))
  (printf "min5~n")
  (void
    (for ((r (in-list tsv)))
      (printf " ~a ~a~n" (car r) (take (sort (cdr r) <) 5))))
  (void))

(module+ main
  (stat "static-python-imports-analysis.tsv"))

