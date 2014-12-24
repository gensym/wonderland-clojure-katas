(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn keyfn [[suit rank]]
  (+
   (* 100 (.indexOf ranks rank))
   (.indexOf suits suit)))

(defn play-round [player1-card player2-card]
  (if (> (keyfn player1-card) (keyfn player2-card))
    [[player1-card player2-card] []]
    [[] [player2-card player1-card]]))

(defn next-step [[player1-cards player2-cards]]
  (if (or (empty? player1-cards) (empty? player2-cards))
    nil
    (let [[player1-result player2-result]
          (play-round (first player1-cards) (first player2-cards))]
      [(concat (rest player1-cards) player1-result)
       (concat (rest player2-cards) player2-result)])))

(defn play-game [player1-cards player2-cards]
  (let [game-steps (take-while identity (iterate next-step [player1-cards player2-cards]))
        end-state (last game-steps)
        [player1-end player2-end] end-state]
    (cond (not (empty? player1-end)) :player1
          (not (empty? player2-end)) :player2
          :else :draw)))
