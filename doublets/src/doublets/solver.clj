(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn- one-away? [word1 word2]
  (and (= (count word1) (count word2))
       (= 1 (apply + (map
                      #(if (= %1 %2) 0 1)
                      word1 word2)))))

(defn- start-expansion [word]
  [word])

(defn- expand [dict path]
  (let [word (last path)
        seen (into #{} path)]
    (->> dict
         (filter (complement seen))
         (filter (partial one-away? word))
         (map (fn [w] (conj path w))))))

(defn- expansions [dict word]
  (tree-seq (constantly true)
            (partial expand dict)
            (start-expansion word)))

(defn doublets [word1 word2]
  (let [matches (filter (fn [path] (= word2 (last path))) (expansions words word1))]
    (if-let [shortest  (first (sort-by count matches))]
      shortest
      [])))
