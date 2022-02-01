#lang racket/base

;; Import .tsv data from this folder.
;; (The data is originally from the SP team.)
;;
;; Usage:
;;  racket import-tsv.rkt <file.tsv>
;;
;; Prints a TeX version of the file.

;; ---

(provide
  tex-escape)

(require
  racket/list
  racket/string)

(define (parse tsv)
  (with-input-from-file
    tsv
    (lambda ()
      (for/list ((ln (in-lines)))
        (tab-split ln)))))

(define (tab-split str)
  (string-split str "\t"))

(define (comma-split str)
  (string-split str ","))

(define (space-join str*)
  (string-join str* " "))

(define (print-tex v**)
  (if (= 2 (length (car v**)))
    (print2 v**)
    (printN+ v**)))

(define (print2 v**)
  (printf "\\begin{tabular}{ll}~n")
  (print-first-row (map colname (car v**)))
  (for ((v* (in-list (cdr v**))))
    (define-values [tt0 tt1] (format-title2 (car v*)))
    (define-values [cm0 cm1]
      (let ((cm (cadr v*))
            (N (string-length "time ./python -X jit -X jit-list-file=Tools/benchmarks/jitlist_richards_static.txt -X jit-enable-jit-list-wildcards")))
        (if (< (string-length cm) N)
          (values (format-cmd cm) "")
          (values (format-cmd (substring cm 0 N))
                  (format-cmd (substring cm N))))))
    (print-row (list tt0 cm0))
    (print-row (list tt1 cm1))
    (void))
  (printf "\\end{tabular}~n")
  (void))

(define (printN+ v**)
  (define max-cols 6)
  (let loop ((v** v**))
    (cond
      [(<= (length (car v**)) max-cols)
       (printN v**)]
      [else
       (define-values [fst* rst*]
         (for/lists (_1 _2)
                    ((v* (in-list v**)))
           (split-at v* max-cols)))
       (printN fst*)
       (newline)
       (loop rst*)])))

(define (printN v**)
  (printf "\\begin{tabular}{~a}~n" (make-string (length (car v**)) #\r))
  (define-values [fst* snd*]
    (for/lists (_1 _2)
               ((v (in-list (car v**))))
      (format-title2 v)))
  (print-row fst*)
  (print-first-row snd*)
  (for ((v* (in-list (cdr v**))))
    (print-row (map format-num v*)))
  (printf "\\end{tabular}~n")
  (void))

(define (colname str)
  (format "\\colname{~a}" str))

(define (format-title str)
  (define-values [kind bm opt*]
    (let* ((m (regexp-match #rx"^([^ ]+) ([^(]*)\\(([^)]*)\\)$" str)))
      (if m
        (values (cadr m)
                (let ((s (string-trim (caddr m))))
                  (and (< 0 (string-length s)) s))
                (map string-trim (comma-split (cadddr m))))
        (raise-arguments-error 'format-title "failed to parse" "str" str))))
  (define k+ (colname (simpl kind)))
  (define o+ (space-join (map colname opt*)))
  (if bm
    (format "~a ~a (~a)" bm k+ o+)
    (format "~a (~a)" k+ o+)))

(define (format-title2 str)
      (let ((s* (string-split (format-title str) " (")))
        (values (car s*) (string-append "(" (cadr s*)))))

(define (format-cmd str)
  (format "\\texttt{~a}" (tex-escape str)))

(define (tex-escape str)
  (string-replace str "_" "\\_"))

(define (format-num str)
  (format "$~a$" str))

(define (simpl str)
  (case str
    (("TypedOpt") "T-Max")
    (("Typed" "TypedMinOpt" "TypedBasic") "T-Min")
    (("TypedBasic2") "T-Min-2")
    (("Original") "Orig")
    (else (raise-argument-error 'simpl "Carl-name" str))))

(define (print-row str* [extra-tail ""])
  (displayln (string-append "  " (string-join str* " & ") " \\\\" extra-tail)))

(define (print-first-row str*)
  (print-row str* "\\hline"))

(module+
  main
  (require racket/cmdline)
  (command-line
    #:program "import-tsv"
    #:args (tsv)
    (print-tex (parse tsv))))
