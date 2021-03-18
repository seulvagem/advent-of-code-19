(ns aoc-19.core
  (:gen-class)
  (:require [aoc-19.base :as base]
            [aoc-19.d1 :as d1]
            [aoc-19.d2 :as d2]))

(defmacro run-print-day
  [d]
  `(do
     (println ~(str "\nday " d ":"))
     (println (~(symbol (str "d" d) "-main")))))

(defmacro doseq-macro
  [macroname & args]
  `(do
     ~@(map (fn [arg] (list macroname arg)) args)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (doseq-macro run-print-day 1 2)
  ;; (run-print-day 1)
  ;; (run-print-day 2)
  ;; (run-print-day 3)
  ;; (run-print-day 4)
  ;; (run-print-day 5)
  ;; (run-print-day 6)
  )

