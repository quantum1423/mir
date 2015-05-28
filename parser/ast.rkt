#lang typed/racket/base
(provide (all-defined-out))
(require "types.rkt")

(define-type Statement (U Expression
                          Def))
(define-type Expression (U Identifier
                           Binexp
                           Funcall
                           
                           IntLiteral
                           StrLiteral
                           TupLiteral
                           LstLiteral
                           
                           FunLiteral
                           
                           Block
                           
                           If))

(define-type Binop (U '+ '- '* '/ '++))

(struct Def
  ([start-pos : Position]
   [end-pos : Position]
   [type : Type]
   [left : Identifier]
   [right : Expression])
  #:transparent)

(struct Identifier
  ([start-pos : Position]
   [end-pos : Position]
   [name : Symbol])
  #:transparent)

(struct Program
  ([self-name : (Option String)]
   [import-paths : (Listof String)]
   [body : (Listof Statement)])
  #:transparent)

(struct Binexp
  ([start-pos : Position]
   [end-pos : Position]
   [operator : Binop]
   [left : Expression]
   [right : Expression])
  #:transparent)

(struct Funcall
  ([start-pos : Position]
   [end-pos : Position]
   [function : Expression]
   [arguments : (Listof Expression)])
  #:transparent)


(struct Block
  ([start-pos : Position]
   [end-pos : Position]
   [body : (Listof Statement)])
  #:transparent)

(struct If
  ([start-pos : Position]
   [end-pos : Position]
   [condition : Expression]
   [true-val : Expression]
   [false-val : Expression])
  #:transparent)


(struct StrLiteral
  ([start-pos : Position]
   [end-pos : Position]
   [contents : String])
  #:transparent)

(struct IntLiteral
  ([start-pos : Position]
   [end-pos : Position]
   [contents : Integer])
  #:transparent)

(struct TupLiteral
  ([start-pos : Position]
   [end-pos : Position]
   [contents : (Listof Expression)])
  #:transparent)

(struct LstLiteral
  ([start-pos : Position]
   [end-pos : Position]
   [contents : (Listof Expression)])
  #:transparent)

(struct FunLiteral
  ([start-pos : Position]
   [end-pos : Position]
   [arguments : (Listof (Pair Identifier Type))]
   [body : Expression])
  #:transparent)
