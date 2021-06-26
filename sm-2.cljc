(ns sm-2)

(defn next-internal
  "return [next-internal repeats next-ef]"
  [last-interval repeats ef quality]
  (assert (and (<= quality 5) (>= quality 0)))
  (let [ef (or ef 2.5)
        last-interval (if (<= last-interval 0) 1 last-interval)
        next-interval
        (cond
          (<= repeats 1) 1
          (= repeats 2) 6
          :else (* last-interval ef))
        next-ef
        (+ ef (- 0.1 (* (- 5 quality) (+ 0.08 (* 0.02 (- 5 quality))))))
        next-ef* (if (< next-ef 1.3) 1.3 next-ef)]
    (if (< quality 3)
      ;; If the quality response was lower
      ;; than 3 then start repetitions for the item from
      ;; the beginning without changing the E-Factor
      [-1 1 ef]
      [next-interval (+ 1 repeats) next-ef*])))
