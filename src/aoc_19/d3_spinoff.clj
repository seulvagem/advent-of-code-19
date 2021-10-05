(ns aoc-19.d3-spinoff
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [aoc-19.base :as b]
            [aoc-19.d3 :as d3]))

;; specs

(s/def ::coord-key #{:x :y})

(s/def ::coordinate int?)
(s/def ::coordinate-range (s/tuple int? int?))

(s/def ::x ::coordinate)
(s/def ::y ::coordinate)

(s/def ::x-range ::coordinate-range)
(s/def ::y-range ::coordinate-range)

(s/def ::pos (s/keys :req-un [::x ::y]))
(s/def ::h-line (s/keys :req-un [::x-range ::y]))
(s/def ::v-line (s/keys :req-un [::x ::y-range]))
(s/def ::line (s/or :h-line ::h-line
                    :v-line ::v-line))

(s/def ::h-lines (s/every ::h-line))
(s/def ::v-lines (s/every ::v-line))

(s/def ::board (s/keys :req-un [::h-lines ::v-lines]))

;; specs 


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


(s/fdef ->line
  :args (s/cat :fix-key ::coord-key
               :range-key ::coord-key
               :initial-position ::pos
               :oriented-distance int?)
  :ret (s/tuple ::line ::pos))

(defn ->line
  [fix-k range-k i-pos dist]
  (let [range-key (-> range-k name (str "-range") keyword)
        range-i-val (range-k i-pos)
        range-f-val (+ dist range-i-val)
        range-tuple (vec (sort [range-i-val range-f-val]))
        end-pos (assoc i-pos range-k range-f-val)]
    [{fix-k (fix-k i-pos)
      range-key range-tuple}
     end-pos]))

(def dir->line-config
  {"R" [:h-lines #(->line :y :x %1 %2)]
   "L" [:h-lines #(->line :y :x %1 (- %2))]
   "U" [:v-lines #(->line :x :y %1 %2)]
   "D" [:v-lines #(->line :x :y %1 (- %2))]})


(def line-configs
  (set (vals dir->line-config)))

(s/fdef acc-lines-to-board ;; doesn't generate yet, probably need to use the ->line spec or something
  :args (s/alt :step-arity (s/cat :board ::board
                                  :step-input (s/tuple line-configs int?))
               :completing-arity (s/cat :board ::board)
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
          :y 0}
    :h-lines []
    :v-lines []}))

(s/fdef get-intersection
  :args (s/cat :h-line ::h-line
               :v-line ::v-line)
  :ret (s/alt :intersection :aoc-19.d3/loc
              :no-intersection nil?))

(defn get-intersection
  "takes a h-line and a v-line, gets the intersection point if they have one"
  [{[hxi hxf] :x-range, hy :y} {vx :x, [vyi vyf] :y-range}]
  (when (and (<= hxi vx hxf)
             (<= vyi hy vyf))
    [vx hy]))

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
  [{b1-hs :h-lines, b1-vs :v-lines} {b2-hs :h-lines, b2-vs :v-lines}]
  (concat (intersecs b1-hs b2-vs)
          (intersecs b2-hs b1-vs)))

(defn get-results
  [input]
  (let [instr-lists (d3/prepare-input input)
        x-instr->line-config (comp d3/x-instr->moves
                                   (map d3/parse-move-string)
                                   (map (fn [[dir step]]
                                          (vector (dir->line-config dir) (b/parse-int step)))))
        boards (mapv #(transduce x-instr->line-config acc-lines-to-board %) instr-lists)
        intersections (apply get-boards-intersections boards)
        res1 (d3/get-closest-loc-by d3/x-loc-center-dist intersections)]
    [res1 (first boards)]))

(defn -main
  []
  (get-results (b/get-input 3)))

(stest/instrument)
