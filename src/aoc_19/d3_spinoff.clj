(ns aoc-19.d3-spinoff
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [aoc-19.base :as b]
            [aoc-19.d3 :as d3]))

;; specs

(s/def ::coord-key #{:x :y})

(s/def ::coordinate int?)
(s/def ::coordinate-range (s/and (s/tuple int? int?) #(apply < %)))

(s/def ::x ::coordinate)
(s/def ::y ::coordinate)
(s/def ::dist int?)
(s/def ::end-val ::coordinate)

(s/def ::x-range ::coordinate-range)
(s/def ::y-range ::coordinate-range)

(s/def ::pos (s/keys :req-un [::x ::y ::dist]))
(s/def ::end-dist ::dist)

(defn select-random-end-val-in-range
  [range-key]
  (fn
    [line]
    (->> line
         range-key
         rand-nth
         (assoc line :end-val))))

(b/sdef-with-gen ::h-line (s/keys :req-un [::x-range ::y ::end-dist ::end-val])
                 (select-random-end-val-in-range :x-range))
(b/sdef-with-gen ::v-line (s/keys :req-un [::x ::y-range ::end-dist ::end-val])
                 (select-random-end-val-in-range :y-range))

(s/def ::line (s/or :h-line ::h-line
                    :v-line ::v-line))

(s/def ::h-lines (s/every ::h-line))
(s/def ::v-lines (s/every ::v-line))

(s/def ::board (s/keys :req-un [::h-lines ::v-lines]))
(s/def ::step-board (s/keys :req-un [::h-lines ::v-lines ::pos]))

;; specs 

(defn ->h
  [y x1 x2]
  {:y y
   :x-range [x1 x2]})

(defn ->v
  [x y1 y2]
  {:x x
   :y-range [y1 y2]})


(s/fdef ->line
  :args (s/cat :keys (s/alt :xy (s/cat :x #{:x}, :y #{:y})
                            :yx (s/cat :y #{:y}, :x #{:x}))
               :initial-position ::pos
               :oriented-distance ::dist)
  :ret (s/tuple ::line ::pos))

(defn sortv
  [a b]
  (if (< a b)
    [a b]
    [b a]))

(defn ->line
  [fix-k range-k {i-dist :dist,, :as i-pos} dist]
  (let [fix-val (fix-k i-pos)
        range-key (-> range-k name (str "-range") keyword)
        range-i-val (range-k i-pos)
        range-f-val (+ dist range-i-val)
        end-dist (+ i-dist (d3/abs dist))
        range-tuple (sortv range-i-val range-f-val)]
    [{fix-k fix-val
      range-key range-tuple
      :end-dist end-dist
      :end-val range-f-val}
     {fix-k fix-val
      range-k range-f-val
      :dist end-dist}]))

(def dir->line-config
  {"R" [:h-lines #(->line :y :x %1 %2)]
   "L" [:h-lines #(->line :y :x %1 (- %2))]
   "U" [:v-lines #(->line :x :y %1 %2)]
   "D" [:v-lines #(->line :x :y %1 (- %2))]})


(def line-configs
  (set (vals dir->line-config)))

(s/fdef acc-lines-to-board
  :args (s/alt :step-arity (s/cat :board ::step-board
                                  :step-input (s/tuple line-configs ::dist))
               :completing-arity (s/cat :board ::step-board)
               :initial-item-arity (s/cat))
  :ret ::board)

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
          :y 0
          :dist 0}
    :h-lines []
    :v-lines []}))

(s/fdef get-intersection
  :args (s/cat :h-line ::h-line
               :v-line ::v-line)
  :ret (s/alt :intersection ::pos
              :no-intersection nil?))

(defn get-intersection
  "takes a h-line and a v-line, gets the intersection point if they have one"
  [{[hxi hxf] :x-range, hy :y,, :as h-line}
   {vx :x, [vyi vyf] :y-range,, :as v-line}]
  (when (and (<= hxi vx hxf)
             (<= vyi hy vyf))
    ;; [vx hy]
    (let [{h-end-dist :end-dist, hx2 :end-val} h-line
          {v-end-dist :end-dist, vy2 :end-val} v-line
          h-line-intersec-diff (d3/abs (- vy2 hy))
          v-line-intersec-diff (d3/abs (- hx2 vx))
          h-dist (- h-end-dist h-line-intersec-diff)
          v-dist (- v-end-dist v-line-intersec-diff)]
      {:x vx
       :y hy
       :dist (+ h-dist v-dist)})))


(s/fdef get-line-intersections
  :args (s/cat :v-lines ::v-lines
               :h-line ::h-line)
  :ret (s/* ::pos))

(defn get-line-intersections
  [v-lines h-line]
  (let [xf (comp (map #(get-intersection h-line %))
                 (filter identity))]
    (sequence xf v-lines)))

(s/fdef intersecs
  :args (s/cat :h-lines ::h-lines
               :v-lines ::v-lines)
  :ret (s/* ::pos))

(defn intersecs
  [h-lines v-lines]
  (mapcat #(get-line-intersections v-lines %) h-lines))


(s/fdef get-boards-intersections
  :args (s/cat :board-1 ::board
               :board-2 ::board)
  :ret (s/* ::pos))

(defn get-boards-intersections
  [{b1-hs :h-lines, b1-vs :v-lines} 
   {b2-hs :h-lines, b2-vs :v-lines}]
  (concat (intersecs b1-hs b2-vs)
          (intersecs b2-hs b1-vs)))

(defn pos->loc
  [{x :x y :y}]
  [x y])

(defn get-results
  [input]
  (let [instr-lists (d3/prepare-input input)
        x-instr->line-config (comp d3/x-instr->moves
                                   (map d3/parse-move-string)
                                   (map (fn [[dir step]]
                                          (vector (dir->line-config dir) (b/parse-int step)))))
        boards (mapv #(transduce x-instr->line-config acc-lines-to-board %) instr-lists)
        intersections (apply get-boards-intersections boards)
        get-closest-loc-by-center-dist (partial d3/get-closest-loc-by d3/x-loc-center-dist)
        res1 (get-closest-loc-by-center-dist (map pos->loc intersections))
        res2 (apply (partial min-key :dist) intersections)]
    [res1 res2]))

(defn -main
  []
  (get-results (b/get-input 3)))

(stest/instrument)
