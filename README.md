# Snail shell

A core lisp lexer for gastropods.

## Why?

My colleagues and I are going to start working through [Types and Progamming
Languages][tapl]. In the book you implement languages of varying feature sets.
The book implements these languages in OCaml, however I had this Lisp parser
essentially ready for awhile. There are a handful of "Write you a Scheme
Interpreters"-like tutorials and they all use a structural aware parser
relatively similar to this one. However, there are some pretty subtle issues
with most of the ones I have seen. For example, the two examples below parse as
a single lexeme in a lot of examples. Even Haskell's parser has a similar issue
to this one!

```
(1a)
(1 a)
```

## Snail shell "language"

In snail shell, we only parse valid lexemes, text literals, and s-expressions.
The valid lexemes are approximately from R5RS Scheme but this may change in the
future. We also use Haskell's line and block comments. Here is a valid snail
shell input,

```
-- Prints `hello "world"` to the console
(print "hello \"world\"")

-- Prints 3 to the console
(print (+ 1 2))

{-
  Defines a function to add two numbers
  Applies the function to generate 3
  Prints 3 to the console
-}
(let
  (f (lambda (x y) (+ x y)))
  (print (f 2 1)))

(quote hello)

(nil)

(print true)

(print false)

-- end comment
```

## Getting the AST

You can see some examples in 'test/Snail/IOSpec.hs', but you can put your snail
shell into some file, let's say 'hello.snail'. The following Haskell will print
the AST or print a failure,

```haskell
module Main where

import Snail.Shell

main :: IO ()
main = do
  eResults <- readSnailFile "./hello.snail"
  case eResults of
    Right ast -> print ast
    Left failureString -> print failureString
```

## TODO

- [ ] See TODO in ./test/files/fennel-reference.fnl, empty string failure
- [ ] Add some examples from https://learnxinyminutes.com/docs/common-lisp in a test

[tapl]: https://www.cis.upenn.edu/~bcpierce/tapl
