(ns aoc-19.d2
  (:gen-class)
  (:require [aoc-19.base :as b]
            [aoc-19.ic :as ic]))

(defn run-code
  [intcode noun verb]
  (let [altered-ic (assoc intcode
                          1 noun
                          2 verb)]
    (ic/run altered-ic)))

(defn find-noun-verb-for-answer
  [intcode answer]
  (let [possible-ints (range 0 100)
        run (partial run-code intcode)]
    (first (for [noun possible-ints
                 verb possible-ints
                 :let [res (first (run noun verb))]
                 :when (= answer res)]
             [noun verb]))))

(defn -main
  []
  (let [intcode (ic/read-intcode-str (b/get-input 2))
        result-ic (run-code intcode 12 2)
        res1 (first result-ic)
        [noun verb] (find-noun-verb-for-answer intcode 19690720)
        res2 (+ (* 100 noun) verb)]
    [res1 res2]))
