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

(def dir->line-config
  {"R" [:h-line #(->line :y :x %1 %2)]
   "L" [:h-line #(->line :y :x %1 (- %2))]
   "U" [:v-line #(->line :x :y %1 %2)]
   "D" [:v-line #(->line :x :y %1 (- %2))]})

(defn acc-lines-to-board 
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

(defn get-line-intersections
  [v-lines h-line]
  (let [xf (comp (map #(get-intersection h-line %))
                 (filter identity))]
    (sequence xf v-lines)))

(defn intersecs
  [h-lines v-lines]
  (mapcat #(get-line-intersections v-lines %) h-lines))


(defn get-boards-intersections
  [{b1-hs :h-line, b1-vs :v-line} {b2-hs :h-line, b2-vs :v-line}]
  (concat (intersecs b1-hs b2-vs)
          (intersecs b2-hs b1-vs)))

(defn get-results
  [input]
  (let [instr-lists (d3/prepare-input input)
        x-instr->line-config (comp d3/x-instr->moves
                                   (map d3/parse-move-string)
                                   (map (fn [[dir step]]
                                          (list (dir->line-config dir) (b/parse-int step)))))
        boards (mapv #(transduce x-instr->line-config acc-lines-to-board %) instr-lists)
        intersections (apply get-boards-intersections boards)
        res1 (d3/get-closest-loc intersections)
        ]
    [res1])) 

(defn -main
  []
  (get-results (b/get-input 3)))

