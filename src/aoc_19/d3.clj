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

(def dir->matx {"U" [0 1]
                "R" [1 0]
                "D" [0 -1]
                "L" [-1 0]})

(defn instr->steps
  "takes an instruction string containing a direction and step count (e.g. R3), returns a sequence of length step count of the direction matrix "
  [instr-str]
  (let [[_ dir moves-str] (re-find #"([^\d\W]+)(\d+)" instr-str)
        move-matx (dir->matx dir)
        n-moves (b/parse-int moves-str)]
    (take n-moves (repeat move-matx))))

(defn abs [n]
  (if (neg? n)
    (- n)
    n))

(defn loc-dist [loc]
  (transduce (map abs) + loc))

(defn acc-move-on-board
  "reducing function that accumulates a board state [board current-pos] with a indexed move [index move-matx], the completing 1-arity returns just the board"
  ([[board pos] [i move]]
   (let [npos (m+ pos move)
         nboard (update board npos (b/upd-if-not i))]
     [nboard npos]))
  ([[board pos]]
   board))

(defn get-step-counts
  "takes a boards coll and a location [x y], returns the corresponding step-count for each board"
  [boards loc]
  (map #(inc (% loc)) boards))    

(defn get-closest-loc
  "takes a coll of locs, returns a tuple of the closest dist and loc"
  [locs]
  (let [x-calc-loc-dist (map #((juxt loc-dist identity) %))
        min-by-dist (partial min-key first)]
    (transduce x-calc-loc-dist min-by-dist [##Inf] locs)))

(defn get-results
  [input]
  (let [wire-path-strs (str/split-lines input)
        instructions-lists (map list wire-path-strs)

       
        x-instr->indexed-steps (comp (mapcat #(str/split % #","))
                                     (mapcat instr->steps)
                                     (map-indexed #(vector %1 %2)))
        

        instr-str->board #(transduce x-instr->indexed-steps acc-move-on-board [{} [0 0]] %)
        boards (map instr-str->board instructions-lists)
        
        intersections (apply set/intersection (map #(set (keys %)) boards))

        x-step-count-sum (comp (map (partial get-step-counts boards))
                               (map #(apply + %)))
        
        res1 (first (get-closest-loc intersections))

        res2  (transduce x-step-count-sum min ##Inf intersections)]
         
    [res1 res2]))

(defn -main []
  (get-results (b/get-input 3)))