(defmacro! def! (name params body) (let! name (\ params body)))
(defmacro! thunk (val) (\ () val))

(def! bool (p) (if p 1 0))
(def! not (p) (if p 0 1))

(def! end? (iter) (= 1 (len iter)))
(def! next (iter) (if (end? iter) [(head iter)] ((tail iter))))

(def! cons-iter (val iter) [val (thunk iter)])
(def! cat-iter (iter val) (chain iter (once val)))

(def! once (val) [val])

(def! chain (iter1 iter2)
    (if (end? iter1)
        [(head iter1) (thunk iter2)]
        [(head iter1) (thunk (chain (next iter1) iter2))]))

(def! map (f iter)
    (if (end? iter)
        [(f (head iter))]
        (with (val cont) iter
            [(f val) (thunk (map f (cont)))])))

(def! range (a b)
    (if (= a (- b 1))
        [a]
        [a (thunk (range (+ a 1) b))]))

(def! reduce (comb iter) {
    (let! result (head iter))
    (while (not (end? iter)) {
        (let! iter (next iter))
        (let! result (comb result (head iter)))})
    result})

(def! fold (comb init iter)
    (reduce comb [init (thunk iter)]))

(def! collect (iter)
    (fold cat [] iter))

(def! filter (pred iter)
    (next
        (fold
            (\ (acc x)
                (if (pred x)
                    (cat-iter acc x)
                    acc))
            [0]
            iter)))

(def! fib-rec (a b) [a (thunk (fib-rec b (+ a b)))])
(def! fib () (fib-rec 0 1))

(def! ints-from (x) [x (thunk (ints-from (+ x 1)))])
(def! naturals () (ints-from 0))

(def! double (x) (* x 2))
