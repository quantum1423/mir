#lang racket
(require "compile.rkt")

(match (current-command-line-arguments)
  [(vector "build" fname)
   (build-from-file (path->complete-path fname))])