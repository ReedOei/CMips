(def (map f xs)
     (if (empty xs)
       '()
       (cons (f (car xs)) (map f (cdr xs)))))

(def (sum xs)
     (if (empty xs)
       0
       (+ (car xs) (sum (cdr xs)))))

(def (plus a x)
     (+ a x))

(def (printhelper len x)
     (if (== 1 len)
       (printf "%d" x)
       (printf "%d," x)))

(def (printmain xs)
     (printhelper (length xs) (car xs))
     (if (== 1 (length xs))
       (printf "")
       (printmain (cdr xs))))

(def (printlist xs)
     (printf "[")
     (printmain xs)
     (printf "]"))

(def (build l h)
     (if (> l h)
       '()
       (cons l (build (+ l 1) h))))

(def (main) (printlist (map (plus 2) (build 1 10))))

