(ns aoc-19.d1
  (:gen-class)
  (:require [aoc-19.base :as b]))

(defn calc-fuel
  [mass]
  (let [fuel (-> mass (/ 3) int (- 2))]
    (if-not (neg? fuel)
      fuel
      0)))

(defn calc-recursive-fuel
  ([mass]
   (calc-recursive-fuel 0 mass))
  ([total-fuel mass]
   (let [fuel (calc-fuel mass)
         total-fuel (+ total-fuel fuel)]
     (if (> fuel 8)
       (recur total-fuel fuel)
       total-fuel))))

(defn process
  [input xf]
  (let [get-fuel (comp (map b/parse-int) xf)]
    (transduce get-fuel + input)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [input (b/get-split-input 1)
        process-input #(process input %)
        ;; get-fuel (comp (map b/parse-int) (map calc-fuel))
        ;; fuel-sum (transduce get-fuel + input)
        fuel (process-input (map calc-fuel))
        ;; get-actual-fuel (comp (map b/parse-int) (map calc-recursive-fuel))
        ;; actual-fuel-sum (transduce get-actual-fuel + input)
        actual-fuel (process-input (map calc-recursive-fuel))]
    [fuel actual-fuel]))

