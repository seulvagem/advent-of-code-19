(ns aoc-19.ic
  (:gen-class)
  (:require [clojure.spec.alpha :as s]))

;;intcode computer

(s/def ::intcode (s/every integer?
                          :kind vector?
                          :min-count 1))

(s/def ::ic-state (s/cat :intcode ::intcode :pointer (s/nilable nat-int?)))

(s/fdef ::operation
  :args ::ic-state
  :ret ::ic-state
  :fn #(let [args (:args %)
             ret (:ret %)
             arg-pointer (:pointer args)
             ret-pointer (:pointer ret)
             arg-ic (:intcode args)
             ret-ic (:intcode ret)]
         (and (> ret-pointer arg-pointer)
              (= (count arg-ic) (count ret-ic)))))

(defmacro sdef-ops
  [spec & ops]
  `(do ~@(map #(list 's/def % (s/spec spec)) ops)))

(defn add
  [intcode first-pos]
  (let [param1-pos-pos first-pos
        param2-pos-pos (inc param1-pos-pos)
        res-pos-pos (inc param2-pos-pos)
        param1-pos (intcode param1-pos-pos)
        param2-pos (intcode param2-pos-pos)
        param1 (intcode param1-pos)
        param2 (intcode param2-pos)
        res-pos (intcode res-pos-pos)
        res (+ param1 param2)]
    [(assoc intcode res-pos res) (inc res-pos-pos)]))

(defn multiply
  [intcode first-pos]
  (let [n-args 3
        last-pos (+ first-pos n-args)
        args-pos (range first-pos last-pos)
        args (map #(intcode %) args-pos)
        ;; args-values (map #(intcode %) args)
        [param1-pos param2-pos res-pos] args
        param1 (intcode param1-pos)
        param2 (intcode param2-pos)
        res (* param1 param2)]
    ;; (println args-values)
    [(assoc intcode res-pos res) last-pos]))

(defn stop
  [intcode _]
  [intcode nil])

(def operations {1 add
                 2 multiply
                 99 stop})

(sdef-ops ::operation add multiply stop cycle)


(defn cycle
  "takes an intcode and a pos, executes the operation cycle from the pos and returns the resulting intcode and pos"
  [intcode pos]
  (let [opcode (nth intcode pos)
        operation (operations opcode)]
    (operation intcode (inc pos))))

(defn run
  "takes an intcode and runs from the start, returning the resulting intcode"
  [intcode]
  {:pre [(s/valid? ::intcode intcode)]}
  (loop [intcode intcode
         pos 0]
    (let [[next-ic next-pos] (cycle intcode pos)]
      (if next-pos
        (recur next-ic next-pos)
        next-ic))))

(defn read-intcode-str
  "takes a string s with integers separated by commas or spaces and return an intcode vector"
  [s]
  (clojure.edn/read-string (str "[" s "]")))



