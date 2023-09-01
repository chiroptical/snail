-- Copied a bunch of examples from https://fennel-lang.org/reference

(fn pxy [x y]
  (print (+ x y)))

(fn pxy [x y]
  (print (+ x y)))

(local pxy (fn [x y]
             (print (+ x y))))

(local functions {})

(fn functions.p [x y z]
  (print (* x (+ y z))))

(set functions.p (fn [x y z]
                   (print (* x (+ y z)))))

(lambda [x ?y z]
  (print (- x (* (or ?y 1) z))))

(fn pxy [x y]
  "Print the sum of x and y"
  (print (+ x y)))

(lambda pxyz [x ?y z]
  "Print the sum of x, y, and z. If y is not provided, defaults to 0."
  (print (+ x (or ?y 0) z)))

(fn add [...]
  {:fnl/docstring "Add arbitrary amount of numbers."
   :fnl/arglist [a b & more]}
  (match (values (select :# ...) ...)
    (0) 0
    (1 a) a
    (2 a b) (+ a b)
    (_ a b) (add (+ a b) (select 3 ...))))

(fn foo []
  {:deprecated "v1.9.0"
   :fnl/docstring "*DEPRECATED* use foo2"}
  )

(fn foo2 [x]
  {:added "v2.0.0"
   :fnl/docstring "Incompatible but better version of foo!"}
  x)

(fn some-function [x ...]
  "Docstring for some-function."
  {:fnl/arglist [x & xs]
   :other :metadata}
  (let [xs [...]] ...))

(fn [a b] (+ a b))

(hashfn (+ $1 $2))

#(+ $1 $2)

(partial (fn [x y] (print (+ x y))) 2)

(let [x 89
      y 198]
  (print (+ x y 12)))

(let [(x y z) (unpack [10 9 8])]
  (+ x y z))

(let [[a b c] [1 2 3]]
  (+ a b c))

(let [{:msg message : val} {:msg "hello there" :val 19}]
  (print message)
  val)

(let [[a b & c] [1 2 3 4 5 6]]
  (table.concat c ","))

(local t [1 2 3 4 5 6])
(setmetatable
 t
 {:__fennelrest (fn [t k]
                  (let [res {}]
                    (for [i k (length t)]
                      (tset res (tostring (. t i)) (. t i)))
                  res))})
(let [[a b & c] t]
  c)

(let [{:a a :b b &as all} {:a 1 :b 2 :c 3 :d 4}]
  (+ a b all.c all.d))

(local tau-approx 6.28318)

(case mytable
  59      :will-never-match-hopefully
  [9 q 5] (print :q q)
  [1 a b] (+ a b))

(case mytable
  [a a] (* a 2)
  [a b] (+ a b))

(case mytable
  [a ?b] :maybe-one-maybe-two-values
  [?a ?a] :maybe-none-maybe-two-same-values
  [?a ?b] :maybe-none-maybe-one-maybe-two-values)

(case mytable
  [a _b] :maybe-one-maybe-two-values
  [_a _a] :maybe-none-maybe-one-maybe-two-values
  [_a _b] :maybe-none-maybe-one-maybe-two-values
  _ :no-match)

(case mytable
  {:subtable [a b ?c] :depth depth} (* b depth)
  _ :unknown)

(case (io.open "/some/file")
  (nil msg) (report-error msg)
  f (read-file f))

(case [91 12 53]
  (where [a b c] (= 5 a)) :will-not-match
  (where [a b c] (= 0 (math.fmod (+ a b c) 2)) (= 91 a)) c)

(case [5 1 2]
  (where (or [a 3 9] [a 1 2]) (= 5 a)) "Either [5 3 9] or [5 1 2]"
  _ "anything else")

(case [1 2 3]
  (where (or [a b 2] [a b 3]) (< a 0) (<= b 1))
  :body)

(let [x 1]
  (case [:hello]
    [x] x))

(let [pass :hunter2]
  (case (user-input)
    (where (= pass)) :login
    _ :try-again!))

(let [x 95]
 (match [52 85 95]
   [b a a] :no
   [x y z] :no
   [a b x] :yes))

(let [name nil
      get-input (fn [] "Dave")]
  (match (get-input)
    name (.. "Hello " name)
    ?no-input (.. "Hello anonymous")))

(match [1 2 3]
  (where [a 2 3] (< 0 a)) "new guard syntax"
  ([a 2 3] ? (< 0 a)) "obsolete guard syntax")

(fn handle [conn token]
  (case-try (conn:receive :*l)
    input (parse input)
    (command-name params (= token)) (commands.get command-name)
    command (pcall command (table.unpack params))
    (catch
     (_ :timeout) nil
     (_ :closed) (pcall disconnect conn "connection closed")
     (_ msg) (print "Error handling input" msg))))

(fn handle [conn token]
  (match-try (conn:receive :*l)
    input (parse input)
    (command-name params token) (commands.get command-name)
    command (pcall command (table.unpack params))
    (catch
      (_ :timeout) nil
      (_ :closed) (pcall disconnect conn "connection closed")
      (_ msg) (print "Error handling input" msg))))

(var x 83)

(let [t {:a 4 :b 8}]
  (set t.a 2) t)

(let [tbl {:d 32} field :d]
  (tset tbl field 19) tbl)

(with-open [fout (io.open :output.txt :w) fin (io.open :input.txt)]
  (fout:write "Here is some text!\n")
  ((fin:lines)))

(let [x (math.random 64)]
  (if (= 0 (% x 10))
      "multiple of ten"
      (= 0 (% x 2))
      "even"
      "I dunno, something else"))

(when launch-missiles?
  (power-on)
  (open-doors)
  (fire))

(local out [])
(each [_ value (pairs tbl) &until (< max-len (length out))]
  (table.insert out value))

(var x 0)
(for [i 1 128 &until (maxed-out? x)]
  (set x (+ x i)))

(for [i 1 10 2]
  (log-number i)
  (print i))

(-?> {:a {:b {:c 42}}}
     (. :a)
     (. :missing)
     (. :c))

(-?>> :a
      (. {:a :b})
      (. {:b :missing})
      (. {:c 42}))

(macros {:my-max (fn [x y]
                   `(let [x# ,x y# ,y]
                      (if (< x# y#) y# x#)))})

(eval-compiler
  (each [name (pairs _G)]
    (print name)))
