#lang racket
(require "desugar.rkt")
(require "compile.rkt")
(require redex)

(test-match SP-compiled program-
            (term (compile-program (desugar-program ()))))