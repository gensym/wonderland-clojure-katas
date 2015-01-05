(ns fox-goose-bag-of-corn.puzzle
  (:refer-clojure :exclude [==])
   (:use clojure.core.logic))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

;; We can represent each place (left bank, river, right bank) as a boolean vector, indicating the presence or absence of each party (corn, goose, boat, etc)

(def parties [:boat :fox :goose :corn :you])

;; So, for example:

(def you-and-goose-in-boat [true false true false true])

;; A couple convenience methods let us convert from the boolean vector
;; back to the list of parties in a place

(defn contains-party? [boolean-place party]
  (get boolean-place (.indexOf parties party )))

(defn boolean-place->parties-list [boolean-place]
  (filter (partial contains-party? boolean-place) parties))

;; (contains-party? you-and-goose-in-boat :boat) => true
;; (contains-party? you-and-goose-in-boat :corn) => false
;; (boolean-place->parties-list you-and-goose-in-boat) => (:goose :you :boat)

;; The point of representing the data in this format is to make it easy to
;; work with core.logic:

(defn boolo [var]
  (conde [(== var true)]
         [(== var false)]))

;; Each place is a list of booleans, 1 for each party

(defn placeo [place boat fox goose corn you]
  (all
   (boolo boat)
   (boolo fox)
   (boolo goose)
   (boolo corn)
   (boolo you)
   (== [boat fox goose corn you] place)))

;; And now we can generate all place combinations

(def valid-and-invalid-place-combinations
  (map boolean-place->parties-list
       (run* [q]
         (fresh [boat fox goose corn you]
           (placeo q boat fox goose corn you)))))

;; Of course, not all combinations are valid. If the goose and the corn are in the same place, you must be in that place as well

(defn goose-eats-corno [boat fox goose corn you]
  (conde
   [(== true you)]
   [(== false goose)]
   [(== false corn)]))

;; ... similarly for the fox and the goose
(defn fox-eats-gooseo [boat fox goose corn you]
  (conde
   [(== true you)]
   [(== false goose)]
   [(== false fox)]))

;; We can't have the fox, corn, or goose in the boat unless you're there with it
(defn boat-needs-piloto [boat fox goose corn you]
  (conde
   [(== false boat)]
   [(== true you)]
   [(== false goose)
    (== false corn)
    (== false fox)]))

;; Don't overload the boat
(defn boat-is-smallo [boat fox goose corn you]
  (conde
   [(== false goose) (== false fox)]
   [(== false goose) (== false corn)]
   [(== false fox) (== false corn)]
   [(== false boat)]))

;; Put it all together
(defn valid-placeo [place]
  (fresh [boat fox goose corn you]
    (placeo place boat fox goose corn you)
    (goose-eats-corno boat fox goose corn you)
    (fox-eats-gooseo boat fox goose corn you)
    (boat-needs-piloto boat fox goose corn you)
    (boat-is-smallo boat fox goose corn you)))


;; Now we have a list of all valid places
(def valid-place-combinations
  (map boolean-place->parties-list
       (distinct
        (run* [q] (valid-placeo q)))))

;; Let's use those valid places to create a list of all valid states.
;; Each state consists of 3 valid locations: left bank, river, right bank,
;; with the river having the boat


;; In each state, each party can be in only one location

(defn one-ofo
  ([coll] (one-ofo false coll))
  ([v coll]
     (conde
      [(emptyo coll)
       (== true v)]
      [(== false v)
       (fresh [h r]
         (conso h r coll)
         (one-ofo h r))]
      [(== true v)
       (fresh [h r]
         (conso h r coll)
         (== false h)
         (one-ofo true r))])))

(defn party-in-exactly-one-placeo [p1 p2 p3]
  (fresh [h1 h2 h3
          t1 t2 t3
          heads]
    (conso h1 t1 p1)
    (conso h2 t2 p2)
    (conso h3 t3 p3)
    (== heads [h1 h2 h3])
    (one-ofo heads)
    (party-in-exactly-one-placeo t1 t2 t3)))


(defn valid-states []
  (run 25 [q]
    (fresh [left river right]
      (== q [left river right])
      (conso true (lvar) river)
      (valid-placeo left)
      (valid-placeo right)
      (valid-placeo river)))
  )

(defn valid-states-print []
  (map (partial map (partial boolean-place->parties-list)) (valid-states)))

(defn river-crossing-plan []
  start-pos)
