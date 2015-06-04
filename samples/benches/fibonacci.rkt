#lang mir

let fib(x Fixnum) Fixnum = {
  if x < 2 then 1 else fib(x:-1) :+ fib(x:-2)
}

__rsplice__("(time (fib 40))")