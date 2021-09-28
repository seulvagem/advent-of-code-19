(ns aoc-19.d3
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.set :as set]
            [aoc-19.base :as b]
            [clojure.string :as str]))

(def base-loc-spec (s/spec (s/cat :x integer? :y integer?)))

(s/def ::loc (s/spec base-loc-spec
                     :gen  #(gen/fmap (partial into [])
                                      (s/gen base-loc-spec))))

(s/def ::dir-matrix (s/spec ::loc))

(s/def ::w-id keyword?)

;; (s/def ::w-path (s/every ::loc :kind set?))

;; (s/def ::c-board (s/map-of ::w-id ::w-path))

;; (s/def ::move-instr (s/cat :dir-matrix ::dir-matrix :moves-quantity integer?))

(defn m+
  ([matx]
   matx)
  ([matx1 matx2]
   (mapv + matx1 matx2))
  ([matx1 matx2 & matxs]
   (apply m+ (m+ matx1 matx2) matxs)))

(def dir->matx {\U [0 1]
                \R [1 0]
                \D [0 -1]
                \L [-1 0]})

(defn instr->steps
  [instr-str]
  (let [dir (first instr-str)
        move-matx (dir->matx dir)
        n-moves (b/parse-int (re-find #"(?=\w)\d+" instr-str))]
    (take n-moves (repeat move-matx))))

(defn abs [n]
  (if (neg? n)
    (- n)
    n))

(defn loc-dist [loc]
  (transduce (map abs) + loc))

(defn closest-loc-dist
  [locs]
  (apply min (map #(loc-dist %) locs)))

(defn get-intersections
  [boards]
  (let [path-sets (map #(into #{} (keys %)) boards)]
    (apply set/intersection path-sets)))

(defn move-on-board
  "reducing function that accumulates a board state [board current-pos] with a indexed move [index move-matx], the completing arity returns just the board"
  ([[board pos] [i move]]
   (let [npos (m+ pos move)
         nboard (update board npos (b/upd-if-not i))]
     [nboard npos]))
  ([[board pos]]
   board))

(defn instr-str->board
  [instr-str]
  (let [instructions (str/split instr-str #",")
        xf (comp (mapcat instr->steps)
                 (map-indexed #(identity [%1 %2])))]
    (transduce xf move-on-board [{} [0 0]] instructions)))

(defn get-step-counts
  "takes a boards coll and a location [x y], returns the corresponding step-count for each board"
  [boards loc]
  (map #(% loc) boards))

(defn -main [input]
  (let [
        ;; wire-path-strs (b/get-split-input 3)
        wire-path-strs (str/split-lines input)
        boards (map instr-str->board wire-path-strs)
        intersections (get-intersections boards)
        res1 (closest-loc-dist intersections)
        xf (comp (map (partial get-step-counts boards))
                 (map #(apply + %))
                 )
        res2 (+ (count boards) (first (into (sorted-set) xf intersections)))]
    [res1 res2]))