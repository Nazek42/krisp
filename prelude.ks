(defmacro! def! (name params body) (' (let! name (\ params body))))

(defmacro! thunk (val) (' (\ () val)))

(def! bool (p) (if p 1 0))
(def! not (p) (if p 0 1))

(defmacro! and (e1 e2)
    (' (with (p1) [e1]
        (if p1 e2 p1))))

(defmacro! or (e1 e2)
    (' (with (p1) [e1]
        (if p1 p1 e2))))

(def! xor (p1 p2) (if p1 (not p2) (bool p2)))

(def! = (a b) (not (cmp a b)))
(def! < (a b) (= (cmp a b) -1))
(def! > (a b) (= (cmp a b) 1))
(def! >= (a b) (or (> a b) (= a b)))
(def! <= (a b) (or (< a b) (= a b)))

(def! end? (iter) (= 1 (len iter)))
(def! next (iter) (if (end? iter) [(head iter)] ((tail iter))))

(def! cons-iter (val iter) [val (thunk iter)])
(def! cat-iter (iter val) (chain iter (once val)))

;(def! ~iter (list)

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

(def! all? (iter) {
    (let! result (bool (head iter)))
    (while (not (or (end? iter) (not result))) {
        (let! iter (next iter))
        (let! result (and result (head iter)))})
    (bool result)})

(def! any? (iter) {
    (let! result (bool (head iter)))
    (while (not (or (end? iter) (bool result))) {
        (let! iter (next iter))
        (let! result (or result (head iter)))})
    (bool result)})

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

(def! take (n iter)
    (if (= n 1)
        [(head iter)]
        [(head iter) (thunk (take (- n 1) (next iter)))]))

(def! fib-rec (a b) [a (thunk (fib-rec b (+ a b)))])
(def! fib () (fib-rec 0 1))

(def! fn? (x) (or (native-fn? x) (extern-fn? x)))

(def! ints-from (x) [x (thunk (ints-from (+ x 1)))])
(def! naturals () (ints-from 0))

(def! range (a b) (take (- b a) (ints-from a)))

(def! double (x) (* x 2))
