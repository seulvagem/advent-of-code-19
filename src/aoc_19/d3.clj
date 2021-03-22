(ns d3
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as test]
            [clojure.spec.gen.alpha :as gen]
            [clojure.set :as set]))

(s/def ::loc (s/tuple integer? integer?))

(s/def ::w-id keyword?)

(s/def ::w-path (s/every ::loc :kind set?))

(s/def ::c-board (s/map-of ::w-id ::w-path))

(defn get-intersections
  [& w-paths]
  (apply set/intersection w-paths))

