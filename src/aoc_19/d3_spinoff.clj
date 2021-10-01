(ns aoc-19.d3-spinoff
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [aoc-19.base :as b]))

(s/def ::coordinate int?)
(s/def ::coordinate-range (s/tuple int? int?))

(s/def ::x ::coordinate)
(s/def ::y ::coordinate)

(s/def ::x-range ::coordinate-range)
(s/def ::y-range ::coordinate-range)

(s/def ::h-line (s/keys :req-un [::x-range ::y]))
(s/def ::v-line (s/keys :req-un [::x ::y-range]))

(defn is-h
  [line]
  (contains? line :y))

(def is-v (comp not is-h))

(defn ->h
  [y x1 x2]
  {:y y
   :x-range [x1 x2]})

(defn ->v
  [x y1 y2]
  {:x x
   :y-range [y1 y2]})

(defn intersects?
  "takes a h-line and a v-line, checks if they intersect at all"
  [{[hxi hxf] :x-range, hy :y} {vx :x, [vyi vyf] :y-range}]
  (and (<= hxi vx hxf)
       (<= vyi hy vyf)))

(defn get-intersection
  "takes a h-line and a v-line ALREADY known to intersect, gets the intersection point"
  [{y :y} {x :x}]
  [x y])