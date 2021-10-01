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

(defn acc-pos-to-set
  ([[acc pos] move]
   (let [npos (m+ pos move)
         nacc (conj acc npos)]
     [nacc npos]))
  ([[acc _]]
   acc))
    

(defn get-closest-intersection
  "takes a coll of loc-sets, returns a tuple of the closest dist and loc"
  [intersections]
  (let [x-calc-loc-dist (map #((juxt loc-dist identity) %))
        min-by-dist (partial min-key first)]
    (transduce x-calc-loc-dist min-by-dist [##Inf] intersections)))

(defn get-results
  [input]
  (let [wire-path-strs (str/split-lines input)
        instructions-lists (map list wire-path-strs)

        x-instr->steps (comp (mapcat #(str/split % #","))
                             (mapcat instr->steps))
        
        
        instr-str->set #(transduce x-instr->steps acc-pos-to-set [#{} [0 0]] %)
        wire-path-sets (map instr-str->set instructions-lists)
        intersections (apply set/intersection wire-path-sets)
        
        res1 (first (get-closest-intersection intersections))]
         
    [res1]))

(defn -main []
  (get-results (b/get-input 3)))