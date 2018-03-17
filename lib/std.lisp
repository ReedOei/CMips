(def (sum xs)
     (if (empty xs)
       0
       (+ (car xs) (sum (cdr xs)))))

(def (build l h)
     (if (> l h)
       '()
       (cons l (build (+ l 1) h))))

(def (pow x n)
     (if (== n 0)
       1
       (* x (pow x (- n 1)))))

(def (square x) (pow x 2))

(def (map f xs)
     (if (empty xs)
       '()
       (cons (f (car xs)) (map f (cdr xs)))))

(def (filter f xs)
     (if (empty xs)
       '()
       (if (f (car xs))
         (cons (car xs) (filter f (cdr xs)))
         (filter f (cdr xs)))))

(def (divides a b) (== (% b a) 0))

(def (even x) (divides 2 x))

(def (factorial n)
     (if (== n 0)
       1
       (* n (factorial (- n 1)))))

(def (nextfactor x i)
     (if (divides i x)
       i
       (nextfactor x (+ i 1))))

(def (factor x)
     (if (== x 1)
       '()
       (cons (nextfactor x 2)
             (factor (/ x (nextfactor x 2))))))

(def (foldl f x xs)
     (if (empty xs)
       x
       (foldl f (f x (car xs)) (cdr xs))))

(def (gcd a b)
     (if (> b a)
       (gcd b a)
       (if (== b 0)
         a
         (gcd b (% a b)))))

(def (lcm a b)
     (/ (* a b) (gcd a b)))

(def (take n xs)
     (if (== n 0)
       '()
       (if (empty xs)
         '()
         (cons (car xs) (take (- n 1) (cdr xs))))))

(def (drop n xs)
     (if (== n 0)
       xs
       (if (empty xs)
         '()
         (drop (- n 1) (cdr xs)))))

(def (primecheck x i)
     (if (== x 1)
       0
       (if (> (* i i) x)
         1
         (if (divides i x)
           0
           (primecheck x (+ i 1))))))
(def (prime x) (primecheck x 2))

(def (genwhile f n)
     (if (== 0 (f n))
       '()
       (cons n (genwhile f (+ n 1)))))

(def (intersperse e xs)
     (if (empty xs)
       '()
       (cons (car xs) (cons e (intersperse e (cdr xs))))))

(def (limit n x) (if (< x n) 1 0))

(def (printhelper len x)
     (if (== 1 len)
       (printf "%d" x)
       (printf "%d," x)))

(def (printmain xs)
     (printhelper (length xs) (car xs))
     (if (== 1 (length xs))
       0
       (printmain (cdr xs))))

(def (printlist xs)
     (printf "[")
     (printmain xs)
     (printf "]"))

(def (concat as bs)
     (if (empty as)
       bs
       (cons (car as) (concat (cdr as) bs))))

(def (fibgen limit a b)
     (if (< a limit)
       (cons a (fibgen limit b (+ a b)))
       '()))

(def (fibs limit) (fibgen limit 0 1))

(def (main)
     (printlist (filter even (fibs 4000000)))
     (printf "\n")
     (printf "The sum is %d\n" (sum (filter even (fibs 4000000))))
     (printlist (filter prime (build 1 100))))

