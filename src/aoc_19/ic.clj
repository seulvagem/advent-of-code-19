(ns aoc-19.ic
  (:gen-class)
  (:require [clojure.spec.alpha :as s]))

;;intcode computer

(s/def ::intcode (s/and vector? (s/+ integer?)))

(defn run
  "takes an intcode and runs from the start"
  [intcode]
  {:pre [(s/valid? ::intcode intcode)]}
  )

(s/fdef run
  :args #(s/valid? ::intcode (first %))
  :ret #(s/valid? ::intcode %))