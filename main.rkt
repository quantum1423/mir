#lang racket
(require "parser.rkt")
(require "modules.rkt")
(require racket/runtime-path)



(match (current-command-line-arguments)
  [(vector "build" fname)
   (build-file fname)])