(ns alphabet-cipher.coder)

(def alphabet (seq "abcdefghijklmnopqrstuvwxyz"))


 (def encode-table
      (reduce merge
              (map-indexed (fn [i row] {row
                                        (reduce merge
                                                (map
                                                 (fn [col val] {col val})
                                                 alphabet
                                                 (concat (drop i alphabet) (take i alphabet))))}) alphabet)))
(def decode-table
  (reduce merge
          (map-indexed (fn [i row] {row
                                    (reduce merge
                                            (map
                                             (fn [col val] {col val})
                                             alphabet
                                             (concat  (drop (- 26 i) alphabet) (take (- 26  i) alphabet))))}) alphabet)))


(defn- make-key [keyword len]
  (take len (apply concat (repeat keyword))))

(defn- apply-table [table keyword message]
  (let [key (make-key keyword (count message))]
    (apply str (map (fn [src key] (get-in table [key src])) message key))))

(defn encode [keyword message]
  (apply-table encode-table keyword message))

(defn decode [keyword message]
  (apply-table decode-table keyword message))
