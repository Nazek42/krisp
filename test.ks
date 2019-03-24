(let! foo (\ (x) (if x (foo (+ x -1)) x)))
(foo 10000)
