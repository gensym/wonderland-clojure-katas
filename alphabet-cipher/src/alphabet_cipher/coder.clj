(ns alphabet-cipher.coder)

(def alphabet (seq "abcdefghijklmnopqrstuvwxyz"))

(defn- make-lookup-table [row-fn]
  (into {} (map-indexed
            (fn [i row] {row
                         (into {} (map (partial assoc {})
                                       alphabet
                                       (row-fn i)))})
            alphabet)))

(def encode-table (make-lookup-table #(concat (drop % alphabet) (take % alphabet))))
(def decode-table (make-lookup-table #(concat (drop (- (count alphabet) %) alphabet) (take (- (count alphabet) %) alphabet))))

(defn- make-key [keyword len]
  (take len (apply concat (repeat keyword))))

(defn- apply-table [table keyword message]
  (let [key (make-key keyword (count message))]
    (apply str (map (fn [src key] (get-in table [key src])) message key))))

(defn encode [keyword message]
  (apply-table encode-table keyword message))

(defn decode [keyword message]
  (apply-table decode-table keyword message))
