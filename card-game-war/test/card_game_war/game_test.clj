(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= [[[:spade 4] [:spade 2]] []] (play-round [:spade 4] [:spade 2])))
    (is (= [[] [[:spade 4] [:spade 2]]] (play-round [:spade 2] [:spade 4]))))
  (testing "queens are higer rank than jacks"
    (is (= [[[:spade :queen] [:spade :jack]] []] (play-round [:spade :queen] [:spade :jack]))))
  (testing "kings are higer rank than queens"
    (is (= [[[:spade :king] [:spade :queen]] []] (play-round [:spade :king] [:spade :queen]))))
  (testing "aces are higer rank than kings"
    (is (= [[[:spade :ace] [:spade :king]] []] (play-round [:spade :ace] [:spade :king]))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= [[[:club 2] [:spade 2]] []] (play-round [:club 2] [:spade 2]))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= [[[:diamond 2] [:club 2]] []] (play-round [:diamond 2] [:club 2]))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= [[[:heart 2] [:diamond 2]] []] (play-round [:heart 2] [:diamond 2])))))

(deftest test-game-state
  (testing "the winner of the round gets those cards at the bottom of their deck"
    (let [player1-start [[:spade :queen] [:heart 2]]
          player2-start [[:spade :jack] [:heart 3]]]

      (is (= [[[:heart 2] [:spade :queen] [:spade :jack]]
              [[:heart 3]]]
             (next-step [player1-start player2-start])))))
  (testing "when a player is out of cards, the next game state is nil"
    (let [player1-start [[:heart 2]]
          player2-start []]
      (is (= nil (next-step [player1-start player2-start])))
      (is (= nil (next-step [player2-start player1-start]))))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
      (let [win-start [[:heart 10]]
            lose-start [[:heart 9]]]
        (is (= :player1 (play-game win-start lose-start)))
        (is (= :player2 (play-game lose-start win-start))))))
