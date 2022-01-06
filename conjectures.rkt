#lang racket
(require redex/reduction-semantics)
(require "compile.rkt")
(require "dynamics.rkt")

;; type-safety
(redex-check
 SP-dynamics
 program+
 (implies (judgment-holds (compileo program+ program-))
          ()))
