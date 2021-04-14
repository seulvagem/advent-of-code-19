(ns aoc-19.d3
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.set :as set]
            [aoc-19.base :as b]
            [clojure.string :as str]))

(def loc-spec (s/cat :x integer? :y integer?))

(s/def ::loc (s/spec loc-spec
                     :gen  #(gen/fmap (partial into [])
                                      (s/gen loc-spec))))

(s/def ::dir-matrix (s/spec ::loc))

;; (s/def ::w-id keyword?)

(s/def ::w-path (s/coll-of ::loc :kind set?))

(s/def ::c-board (s/coll-of ::w-path :count 2))

(s/def ::move-instr (s/cat :dir-matrix ::dir-matrix :moves-quantity integer?))

(defn m+
  ([matrix]
   matrix)
  ([matrix & matrixes]
   (apply mapv + matrix matrixes)))

(defn move
  [path move]
  (let [loc (last path)]
    (conj path (m+ loc move))))


;; way faster than move-line
;; (into [[0 0]] (map (juxt inc (constantly 0))) (range 9000))

(defn move-line
  ([path [move-mat qty]]
   (let [m (fn [acc _] (move acc move-mat))]
     (reduce m path (range qty)))
  ;;  (loop [i (int 0)
  ;;         loc start
  ;;         path []]
  ;;    (if (< i move-qty)
  ;;      (let [nc (m+ c move-matx)
  ;;            nacc (conj acc nc)]
  ;;        (recur (inc i) nc nacc))
  ;;      path))
   ))

(defn get-intersections
  [w-paths]
  (apply set/intersection w-paths))

(def dir->matx {\U [0 1]
                \R [1 0]
                \D [0 -1]
                \L [-1 0]})

(defn step->move-instr
  [step-str]
  (let [dir (first step-str)
        move-matx (dir->matx dir)
        n-moves (b/parse-int (re-find #"(?=\w)\d+" step-str))]
    [move-matx n-moves]))

(defonce id-index (atom 0))

(defn get-id []
  (keyword (str "W" (swap! id-index inc))))

(defn reduce-instrs
  [[locs current-loc] move-instr]
  (let [[nc n-locs] (move move-instr current-loc)]
    [(set/union n-locs locs) nc]))

(defn instrs->locs [move-instrs]
  (let [loc [0 0]
        locs (first (reduce reduce-instrs [#{} loc] move-instrs))]
    locs))

;; (defn ->w-path [str]
;;   (reduce (fn [acc]) (str/split str #",")))
;;   

(defn abs [n]
  (if (neg? n)
    (- n)
    n))

(defn loc-dist [loc]
  (transduce (map abs) + loc))

(defn closest-loc-dist
  [locs]
  (apply min (map #(loc-dist %) locs)))

(defn -main []
  (let [wire-path-strs (b/get-split-input 3)
        stepss (map #(str/split % #",") wire-path-strs)
        xf (map step->move-instr)
        instrss (map #(into [] xf %) stepss)
        paths (map instrs->locs instrss)
        ;; ids (repeatedly (count paths) get-id)
        ;; board (apply hash-map (interleave ids paths))
        intersections (get-intersections paths)]
    (closest-loc-dist intersections)))