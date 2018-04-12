(def (build l h)
     (if (> l h)
       '()
       (cons l (build (+ l 1) h))))

(def (filter f xs)
     (if (empty xs)
       '()
       (if (f (car xs))
         (cons (car xs) (filter f (cdr xs)))
         (filter f (cdr xs)))))

(def (divides a b) (== (% b a) 0))

(def (primecheck x i)
     (if (== x 1)
       0
       (if (> (* i i) x)
         1
         (if (divides i x)
           0
           (primecheck x (+ i 1))))))
(def (prime x) (primecheck x 2))

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

(def (id x) x)

(def (main)
     (printlist (append 1 '()))
     (printlist '(1 2 3))
     (printlist (filter prime (build 1 100))))

