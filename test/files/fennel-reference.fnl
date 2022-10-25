(fn pxy (x y)
  (print (+ x y)))

(local functions ())

(fn functions.p (x y z)
  (print '(* x (+ y z))))

(lambda (x ?y z)
  (print (- x (* (or ?y 1) z))))

(fn pxy (x y)
  "Print the sum of x and y"
  (print (+ x y)))

(lambda pxyz (x ?y z)
  "Print the sum of x, y, and z. If y is not provided, defaults to 0."
  (print (+ x (or ?y 0) z)))

(fn add (...)
  (:fnl/docstring "Add arbitrary amount of numbers."
   :fnl/arglist (a b & more)(
  (match (values (select :# ...) ...)
    (0) 0
    (1 a) a
    (2 a b) (+ a b)
    (_ a b) (add (+ a b) (select 3 ...))))))

(fn (a b) (+ a b))

(hashfn (+ $1 $2))

(partial (fn (x y) (print (+ x y))) 2)

(let (x 89
      y 198)
  (print (+ x y 12))) -- => 299

(let ((x y z) (unpack (10 9 8)))
  (+ x y z)) -- => 27

(let ((a b c) (1 2 3))
  (+ a b c)) -- => 6

(let ((a b & c) (1 2 3 4 5 6))
  (table.concat c ",")) -- => "3,4,5,6"

(local t (1 2 3 4 5 6))

(local tau-approx 6.28318)

(match mytable
  59      :will-never-match-hopefully
  (9 q 5) (print :q q)
  (1 a b) @(+ a b))

(match mytable
  (:subtable (a b ?c) :depth depth( (* b depth)
  _ :unknown)))

(match (io.open "/some/file")
  (nil msg) (report-error msg)
  f (read-file f))

(let (x 95)
 (match (52 85 95)
   (b a a) :no -- because a=85 and a=95
   (x y z) :no -- because x=95 and x=52
   (a b x) :yes)) -- a and b are fresh values while x=95 and x=95

(match (91 12 53)
  (where (a b c) (= 5 a)) :will-not-match
  (where (a b c) (= 0 (math.fmod (+ a b c) 2)) (= 91 a)) c) -- -> 53

(match (5 1 2)
  (where `(or (a 3 9) (a 1 2)) (= 5 a)) "Either (5 3 9) or (5 1 2)"
  _ "anything else")

(match (5 1 2)
  (where (a 3 9) (= 5 a)) "Either (5 3 9) or (5 1 2)"
  (where (a 1 2) (= 5 a)) "Either (5 3 9) or (5 1 2)"
  _ "anything else")

-- bad
(match (1 2 3)
  (where (or (a 1 2) (a b 3)) (< a 0) (< b 1))
  :body)

-- ok
(match (1 2 3)
  (where (or (a b 2) (a b 3)) (< a 0) (<= b 1))
  :body)

(match (1 2 3)
  (where (a 2 3) (< 0 a)) "new guard syntax"
  ((a 2 3) ? (< 0 a)) "obsolete guard syntax")

(fn handle (conn)
  (match-try (conn:receive :*l)
    input (parse input)
    (command-name params) (commands.get command-name)
    command ,(pcall command (table.unpack params))
    (catch
     (_ :timeout) nil
     (_ :closed) (pcall disconnect conn "connection closed")
     (_ msg) (print "Error handling input" msg))))

(var x 83)

(set x (+ x 91))

(let (x (values 1 2 3))
  x) -- => 1

(let ((file-handle message code) (io.open "foo.blah"))
  message) -- => "foo.blah: No such file or directory"

(do (local (_ _ z) (unpack (:a :b :c :d :e))) z) -- => c

-- Basic usage
(with-open (fout (io.open :output.txt :w) fin (io.open :input.txt))
  (fout:write "Here is some text!\n")
  ((fin:lines))) -- => first line of input.txt

-- This demonstrates that the file will also be closed upon error.
(var fh nil)
(local (ok err)
  (pcall #(with-open (file (io.open :test.txt :w))
            (set fh file) -- you would normally never do this
            (error :whoops!))))
(io.type fh) -- => "closed file"
(ok err)     -- => (false "")

(pick-values 2 (func))

(let ((_0_ _1_) (func)) (values _0_ _1_))

(pick-values 0 :a :b :c :d :e) -- => nil
((pick-values 2 (table.unpack (:a :b :c)))) ---> ("a" "b")

(fn add (x y ...) (let (sum (+ (or x 0) (or y 0)))
                        (if (= (select :# ...) 0) sum (add sum ...))))

(add (pick-values 2 10 10 10 10)) -- => 20
(->> (1 2 3 4 5) (table.unpack) (pick-values 3) (add)) -- => 6

(select :# (pick-values 5 "one" "two")) -- => 5
((pick-values 5 "one" "two"))           -- => ("one" "two")

(let (x (math.random 64))
  (if (= 0 (% x 10))
      "multiple of ten"
      (= 0 (% x 2))
      "even"
      "I dunno, something else"))

(when launch-missiles?
  (power-on)
  (open-doors)
  (fire))

(each (key value (pairs mytbl))
  (print "executing key")
  (print (f value)))

(local out ())
(each (_ value (pairs tbl) &until (< max-len (length out)))
  (table.insert out value))

(for (i 1 10 2)
  (log-number i)
  (print i))

(var x 0)
(for (i 1 128 &until (maxed-out? x))
  (set x (+ x i)))

(var done? false)
(while (not done?)
  (print :not-done)
  (when (< 0.95 (math.random))
    (set done? true)))

(if launch-missiles?
    (do
      (power-on)
      (open-doors)
      (fire))
    false-alarm?
    (promote lt-petrov))

-- TODO: This should work...
-- (.. "Hello" " " "world" 7 "!!!") -- => "Hello world7!!!"

(+ (length (1 2 3 nil 8)) (length "abc")) -- => 6 or 8

(. mytbl myfield)

(let (t (:a (2 3 4)() (. t :a 2)))) -- => 3

(?. mytbl myfield)

(icollect (_ v (ipairs (1 2 3 4 5 6)))
  (if (> v 2) (* v v)))
-- -> (9 16 25 36)

-- equivalent to:
(let (tbl ())
  (each (_ v (ipairs (1 2 3 4 5 6)))
    (tset tbl (+ (length tbl) 1) (if (> v 2) (* v v))))
  tbl)

(collect (k v (pairs (:apple "red" :orange "orange" :lemon "yellow"())
  (if (not= k "yellow")
      (values (.. "color-" v) k)))))
-- -> (:color-orange "orange" :color-red "apple"(

-- equivalent to:
(let (tbl (()
  (each (k v (pairs (:apple "red" :orange "orange"())
    (if (not= k "yellow")
      (match (values (.. "color-" v) k)
        (key value) (tset tbl key value))))
  tbl)))))

(collect (k v (pairs (:a 85 :b 52 :c 621 :d 44())
  k (* v 5))))

(icollect (_ x (ipairs (2 3)) &into (9))
  (* x 11))
-- -> (9 22 33)

(accumulate (sum 0
             i n (ipairs (10 20 30 40)))
    (+ sum n)) -- -> 100

(faccumulate (n 0 i 1 5) (+ n i)) -- => 15

(#(faccumulate (n 1 i 1 $) (* n i)) 5) -- => 120 (factorial!)

(fcollect (i 0 10 2)
  (if (> i 2) (* i i)))
-- -> (16 36 64 100)

-- equivalent to:
(let (tbl (()
  (for (i 0 10 2)
    (if (> i 2)
        (table.insert tbl (* i i))))
  tbl)))

(fn (filename)
  (if (valid-file-name? filename)
      (open-file filename)
      (values nil (.. "Invalid filename: " filename))))

(let (f (assert (io.open "hello" "w")))
  (f:write "world")
  (f:close))

(let (f (assert (io.open "hello" "w"))
      method1 :write
      method2 :close)
  (: f method1 "world")
  (: f method2))

(let (f (assert (io.open "hello" "w")))
  (f.write f "world")
  (f.close f))

(fn t.enable (self)
  (set self.enabled? true))

(t:enable)

(-> 52
    (+ 91 2) -- (+ 52 91 2)
    (- 8)    -- (- (+ 52 91 2) 8)
    (print "is the answer")) -- (print (- (+ 52 91 2) 8) "is the answer")

(doto (io.open "/tmp/err.log")
  (: :write contents)
  (: :close))

-- equivalent to:
(let (x (io.open "/tmp/err.log"))
  (: x :write contents)
  (: x :close)
  x)

(include :my.embedded.module)

(fn when2 (condition body1 ...)
  (assert body1 "expected body")
  (if ,condition
     (do ,body1 ,...)))

(when2 (= 3 (+ 2 a))
  (print "yes")
  (finish-calculation))

(if (= 3 (+ 2 a))
  (do
    (print "yes")
    (finish-calculation)))

(import-macros mine :my-macros)

(mine.when2 (= 3 (+ 2 a))
  (print "yes")
  (finish-calculation))

(local (module-name file-name) ...)
(import-macros mymacros (.. module-name ".macros"))

(import-macros mymacros (.. ... ".macros"))

(local fennel (require :fennel))

(fn my-searcher (module-name)
  (let (filename (.. "src/" module-name ".fnl"))
    (match (find-in-archive filename)
      code (values (partial fennel.eval code (:env :_COMPILER()
                   filename))))))

(table.insert fennel.macro-searchers my-searcher)

(macros (:my-max (fn (x y)
                   (let (x# ,x y# ,y)
                      (if (< x# y#) y# x#)))))()

(print (my-max 10 20))
(print (my-max 20 10))
(print (my-max 20 20))

(macro my-max (x y)
  (let (x# ,x y# ,y)
     (if (< x# y#) y# x#)))

(macrodebug (-> abc
                (+ 99)
                (< 0)
                (when (os.exit))))
-- -> (if (< (+ abc 99) 0) (do (os.exit)))

(var v 1)
(macros (:my-max (fn (x y)
                   (if (< ,x ,y) ,y ,x))))()

(fn f () (set v (+ v 1)) v)

(print (my-max (f) 2)) -- -> 3 since (f) is called twice in the macro body above

(macros (:my-max (fn (x y)
                   (let (x2 ,x y2 ,y)
                      (if (< x2 y2) y2 x2)))))()

(print (my-max 10 20))
-- Compile error in 'x2' unknown:?: macro tried to bind x2 without gensym-- try x2# instead

(fn my-fn () (print "hi!"))

(macros (:my-max (fn (x y)
                   (my-fn)
                   (let (x# ,x y# ,y)
                      (if (< x# y#) y# x#)))))()

-- Compile error in 'my-max': attempt to call global '__fnl_global__my_2dfn' (a nil value)

(eval-compiler
  (each (name (pairs _G))
    (print name)))

(fn find (tbl pred)
  (each (key val (pairs tbl))
    (when (pred val)
      (lua "return key"))))

(let (foo-bar :hello)
  (lua "print(foo_bar .. \" world\")"))

(global prettyprint (fn (x) (print (fennel.view x))))
