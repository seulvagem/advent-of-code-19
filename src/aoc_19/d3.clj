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


(defn m+
  ([matx]
   matx)
  ([matx1 matx2]
   (mapv + matx1 matx2))
  ([matx1 matx2 & matxs]
   (reduce m+ (conj matxs matx1 matx2))))

(defn abs [n]
  (if (neg? n)
    (- n)
    n))

(defn parse-move-string
  [s]
  (next (re-find #"([^\d\W]+)(\d+)" s)))

(def dir->matx {"U" [0 1]
                "R" [1 0]
                "D" [0 -1]
                "L" [-1 0]})

(defn loc-dist [loc]
  (transduce (map abs) + loc))

(def x-loc-center-dist (map #((juxt loc-dist identity) %)))

(def min-by-first (partial min-key first))

(defn get-closest-loc-by
  "takes a coll of locs, returns a tuple of the closest dist, given by the xf first result (must return a coll), and the corresponding loc"
  [xf locs]
  (transduce xf min-by-first [##Inf] locs))

(defn prepare-input 
  [input]
  (->> input
       str/split-lines
       (map list)))

(def x-instr->moves (mapcat #(str/split % #",")))

(defn instr->steps
  "takes an instruction string containing a direction and step count (e.g. R3), returns a sequence of length step count of the direction matrix "
  [instr-str]
  (let [[dir moves-str] (parse-move-string instr-str)
        move-matx (dir->matx dir)
        n-moves (b/parse-int moves-str)]
    (take n-moves (repeat move-matx))))

(defn acc-move-on-board
  "reducing function that accumulates a board state [board current-pos] with a indexed move [index move-matx], the completing 1-arity returns just the board"
  ([[board pos] [i move]]
   (let [npos (m+ pos move)
         nboard (update board npos (b/upd-if-not i))]
     [nboard npos]))
  ([[board pos]]
   board)
  ([]
   [{} [0 0]]))

(defn get-step-counts
  "takes a boards coll and a location [x y], returns the corresponding step-count for each board"
  [boards loc]
  (map #(inc (% loc)) boards))

(defn get-boards-intersections
  [boards]
  (apply set/intersection (map #(set (keys %)) boards)))

(defn get-results
  [input]
  (let [instructions-lists (prepare-input input)

        x-instr->indexed-steps (comp x-instr->moves
                                     (mapcat instr->steps)
                                     (map-indexed #(vector %1 %2)))
        instr-str->board #(transduce x-instr->indexed-steps acc-move-on-board  %)

        boards (map instr-str->board instructions-lists)
        intersections (get-boards-intersections boards)

        x-step-count-sum (comp (map (juxt (partial get-step-counts boards) identity))
                               (map (juxt #(apply + (first %)) second)))

        res1 (get-closest-loc-by x-loc-center-dist intersections)
        res2 (get-closest-loc-by x-step-count-sum intersections)]
         
    [res1 res2]))

(defn -main []
  (get-results (b/get-input 3)))