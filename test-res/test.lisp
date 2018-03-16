(def (sum xs)
     (if (empty xs)
       0
       (+ (car xs) (sum (cdr xs)))))

(def (build l h)
     (if (== l h)
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

(def (length xs)
     (if (empty xs)
       0
       (+ 1 (length (cdr xs)))))

(def (intersperse e xs)
     (if (empty xs)
       '()
       (cons (car xs) (cons e (intersperse e (cdr xs))))))

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

(def (main)
    (printf "%d" (sum (map square (filter even (build 1 100))))))

