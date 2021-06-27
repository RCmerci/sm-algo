(ns sm-5)

(def learning-fraction
  "any number between 0 and 1 (the greater it is the faster the changes of the OF matrix)"
  0.5)

(defn- get-of [of-matrix n ef]
  (or (get-in of-matrix [n ef])
      (if (<= n 1)
        4
        ef)))

(defn- set-of [of-matrix n ef of]
  (->>
   (format "%.3f" of)
   (read-string)
   (assoc-in of-matrix [n ef])))

(defn- interval
  [n ef of-matrix]
  (if (<= n 1)
    (get-of of-matrix 1 ef )
    (* (get-of of-matrix n ef )
       (interval (- n 1) ef of-matrix))))

(defn- next-ef
  [ef quality]
  (let [ef* (+ ef (- 0.1 (* (- 5 quality) (+ 0.08 (* 0.02 (- 5 quality))))))]
    (if (< ef* 1.3) 1.3 ef*)))

(defn- next-of-matrix
  [of-matrix n quality fraction ef]
  (let [of (get-of of-matrix n ef)
        of* (* of (+ 0.72 (* quality 0.07)))
        of** (+ (* (- 1 fraction) of ) (* of* fraction))]
    (set-of of-matrix n ef of**)))

(defn next-interval
  "return [next-interval repeats next-ef of-matrix]"
  [last-interval repeats ef quality of-matrix]
  (assert (and (<= quality 5) (>= quality 0)))
  (let [ef (or ef 2.5)
        last-interval (if (<= last-interval 0) 1 last-interval)
        next-interval (interval repeats ef of-matrix)
        next-ef (next-ef ef quality)
        next-of-matrix (next-of-matrix of-matrix repeats quality learning-fraction ef)]

    (if (< quality 3)
      ;; If the quality response was lower than 3
      ;; then start repetitions for the item from
      ;; the beginning without changing the E-Factor
      [-1 1 ef next-of-matrix]
      [next-interval (+ 1 repeats) next-ef next-of-matrix])))
