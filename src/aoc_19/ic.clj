(ns aoc-19.ic
  (:gen-class)
  (:require [clojure.spec.alpha :as s]))

;;intcode computer

(s/def ::intcode (s/+ integer?))

