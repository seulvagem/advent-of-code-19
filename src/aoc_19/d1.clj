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
     (if (> fuel 0)
       (recur total-fuel fuel)
       total-fuel))))

(defn process-sum
  [input xf]
  (let [process-line (comp (map b/parse-int) xf)]
    (transduce process-line + input)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [input (b/get-split-input 1)
        process-input #(process-sum input %)
        ;; get-fuel (comp (map b/parse-int) (map calc-fuel))
        ;; fuel-sum (transduce get-fuel + input)
        fuel (process-input (map calc-fuel))
        ;; get-actual-fuel (comp (map b/parse-int) (map calc-recursive-fuel))
        ;; actual-fuel-sum (transduce get-actual-fuel + input)
        actual-fuel (process-input (map calc-recursive-fuel))]
    [fuel actual-fuel]))

