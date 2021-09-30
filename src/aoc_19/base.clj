(ns aoc-19.base
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input
  "slurps the input from the corresponding day"
  [day]
  (-> (str "inputs/d" day ".txt")
      io/resource
      ;; io/file
      slurp))


;; (defn split-newline
;;   "splits string at newlines (#\\R)"
;;   [input]
;;   (re-seq #".+(?=\R)" input))

(def get-split-input
  (comp str/split-lines get-input))

(defn parse-char [s]
  (nth s 0 nil))

(defn parse-int
  ([s]
   (parse-int s 10))
  ([s rdx]
   (try (Integer/parseInt s rdx)
        (catch Exception e (println e) nil))))

(defn upd-if-not
  "takes a new value, returns a fn that expects an old value, it will return old unless it is falsy, returns new then"
  [new]
  #(if-not %
    new
    %))

(defmacro couple-times
  [times-mult op-mult op]
  `(dotimes [_# ~times-mult]
     (time (dotimes [_# ~op-mult]
             ~op))))
