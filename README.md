# Snail

A no-semantics programming language for gastropods.

## Why?

My colleagues and I are going to start working through [Types and Programming
Languages][tapl]. In the book you implement languages of varying feature sets.
The book implements these languages in OCaml, however I had this Lisp parser
essentially ready for awhile. There are a handful of "Write you a Scheme
Interpreters"-like tutorials and they all use a parser relatively similar to
this one. However, there are some pretty subtle issues with most of the ones I
have seen. For example, the two examples below parse as two lexemes in a lot of
examples. Even Haskell's parser has [this issue][haskell-parse-issue]!

```
(1a)
(1 a)
```

## Is this really a programming language?

From the ["Programming language" Wikipedia page][pl-wikipedia],

> A programming language is a system of notation for writing computer programs.

> The description of a programming language is usually split into the two components of syntax (form) and semantics (meaning)

Snail is used for writing interpreters or compilers. However, it doesn't define
**any** semantics. So, maybe?

## Syntax (form)

Snail describes valid lexemes, text literals, and s-expressions. The valid
lexemes are approximately from R5RS Scheme but this may change in the future.
We also use Haskell's line and block comments. Here is a valid snail program,

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

Reminder, this program has no semantics. It is your job to take Snail's
Abstract Syntax Tree (AST) and define the semantics of an interpreter or
compiler.

## Getting the AST

You have two options: `readSnailFile` or `parseSnail`.

`readSnailFile` can be used like this, assuming you have put some valid snail
into a file `./hello.snail`,

```haskell
import Snail

printSnail :: IO ()
printSnail = do
  eResults <- readSnailFile "./hello.snail"
  case eResults of
    Right ast -> print ast
    Left failureString -> print failureString
```

`parseSnail` doesn't require `IO` the only parameter is `Text`. This is useful
for one-line programs, e.g.,

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Snail

example :: Either String [SnailAst]
example = parseSnail "(print false)"
```

## Example Interpreters

1. The `arith` language from [Types and Programming Languages][tapl]: https://github.com/chiroptical/snail-arith/blob/main/src/Lib.hs
2. Languages from [essentials-of-compilation][essentials-of-compilation]: https://github.com/chiroptical/essentials-of-compilation (each chapter is a module)

## Differences from S-Cargot

I was recently reminded of [s-cargot][s-cargot]. From their GitHub,

> S-Cargot is a library for parsing and emitting S-expressions, designed to be
> flexible, customizable, and extensible.

This is quite different from Snail. Snail is not flexible, customizable, or
extensible. I have chosen a syntax and either you use it or you don't.
With S-Cargot, you can build a custom S-expression parser. Snail provides one.

I don't think you can re-write Snail using S-Cargot? It doesn't appear to
support `'(x)` or `(let [x 10] x)`. Please correct me if I missed something.

I tried to mimic their example
(https://github.com/aisamanra/s-cargot/blob/master/example/example.hs) in
./example/Main.hs.

[tapl]: https://www.cis.upenn.edu/~bcpierce/tapl
[haskell-parse-issue]: https://twitter.com/chiroptical/status/1471568781906518018
[pl-wikipedia]: https://en.wikipedia.org/wiki/Programming_language
[essentials-of-compilation]: https://mitpress.mit.edu/9780262047760/essentials-of-compilation
[s-cargot]: https://github.com/aisamanra/s-cargot
