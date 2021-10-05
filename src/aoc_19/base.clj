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

(defn exercise-first
  [& args]
  (map first (apply clojure.spec.alpha/exercise args)))

(defmacro couple-times
  [times-mult op-mult op]
  `(dotimes [_# ~times-mult]
     (time (dotimes [_# ~op-mult]
             ~op))))

(defmacro sdef-with-gen
  [s-id s-form upd-gen-fn]
  `(let [s-form# ~s-form]
     (clojure.spec.alpha/def
       ~s-id (clojure.spec.alpha/spec
              s-form#
              :gen  #(clojure.spec.gen.alpha/fmap
                      ~upd-gen-fn
                      (clojure.spec.alpha/gen s-form#))))))



;; (defn juxt-args
;;   "kinda like juxt, but the returned function expects the same number of args as the function, applies each arg individually"
;;   [& fns]
;;   (fn [& args] 
;;     (mapv #(%1 %2) fns args)))
