(ns aoc-19.d3-spinoff
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [aoc-19.base :as b]
            [aoc-19.d3 :as d3]))

(s/def ::coordinate int?)
(s/def ::coordinate-range (s/tuple int? int?))

(s/def ::x ::coordinate)
(s/def ::y ::coordinate)

(s/def ::x-range ::coordinate-range)
(s/def ::y-range ::coordinate-range)

(s/def ::h-line (s/keys :req-un [::x-range ::y]))
(s/def ::v-line (s/keys :req-un [::x ::y-range]))

(defn h?
  [line]
  (contains? line :y))

(def v? (comp not h?))

(defn ->h
  [y x1 x2]
  {:y y
   :x-range [x1 x2]})

(defn ->v
  [x y1 y2]
  {:x x
   :y-range [y1 y2]})

;; (defn intersects?
;;   "takes a h-line and a v-line, checks if they intersect at all"
;;   [{[hxi hxf] :x-range, hy :y} {vx :x, [vyi vyf] :y-range}]
;;   (and (<= hxi vx hxf)
;;        (<= vyi hy vyf)))

(defn get-intersection
  "takes a h-line and a v-line, gets the intersection point if they have one"
  [{[hxi hxf] :x-range, hy :y} {vx :x, [vyi vyf] :y-range}]
  (when (and (<= hxi vx hxf)
             (<= vyi hy vyf))
    [vx hy]))


(defn ->line
  [fix-k range-k i-pos dist]
  (let [range-key (-> range-k name (str "-range") keyword)
        range-i-val (range-k i-pos)
        range-f-val (+ dist range-i-val)
        range-tuple (sort [range-i-val range-f-val])
        end-pos (assoc i-pos range-k range-f-val)]
    [{fix-k (fix-k i-pos)
      range-key range-tuple}
     end-pos]))

(def dir->line-fn
  {"R" [:h-line #(->line :y :x %1 %2)]
   "L" [:h-line #(->line :y :x %1 (- %2))]
   "U" [:v-line #(->line :x :y %1 %2)]
   "D" [:v-line #(->line :x :y %1 (- %2))]})

(defn acc-lines 
  ([{pos :pos :as board} [[line-type ->line] dist]]
   (let [[nline npos] (->line pos dist)]
     (-> board
         (update line-type conj nline)
         (assoc :pos npos))))
  ([board]
   (dissoc board :pos))
  ([]
   {:pos {:x 0
          :y 0}
    :h-line []
    :v-line []}))

(defn intersecs
  [h-lines v-lines]
  (let [xf (comp (mapcat (map #(get-intersection) h-lines))
                 ())]
    (sequence xf v-lines)))

(defn intersections
  [ref-board boards]
  (let []))

(defn get-intersections
  [boards]
  (loop [board (first boards)
         next-boards (next boards)]
    ()))

(defn get-results
  [input]
  (let [instr-lists (d3/prepare-input input)
        xf (comp d3/x-instr->moves
                 (map d3/parse-move-string)
                 (map (fn [[dir step]]
                        (list (dir->line-fn dir) (b/parse-int step)))))
        boards (map #(transduce xf acc-lines %) instr-lists)
        ]
    (map #(apply + %) (filter identity (mapcat #(map (fn [a] (get-intersection a %)) (:h-line (second boards))) (:v-line (first boards)))))
    )) 

(defn -main
  []
  (get-results (b/get-input 3)))

