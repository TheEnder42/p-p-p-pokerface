(ns p-p-p-pokerface)

(def face-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
  (if(Character/isDigit fst)
    (Integer/valueOf (str fst))
    (face-ranks fst))))

(defn suit [card]
  (let [[fst snd] card]
   (str snd)))

(defn pair? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 2)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 3)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 4)
    true
    false))

(defn flush? [hand]
  (if (contains? (set (vals (frequencies (map suit hand)))) (count hand))
    true
    false))

(defn full-house? [hand]
  (if (and (pair? hand) (three-of-a-kind? hand))
    true
    false))

;(def no-pair ["2H" "3S" "6C" "5D" "4D"])
;(def one-pair ["2H" "4S" "AC" "8D" "4D"])
;(def two-pair ["2H" "4S" "AC" "2D" "4D"])
;(def three-of-a-kind ["2H" "2C" "2S" "1D" "5D"])

(defn two-pairs? [hand]
  (let [rank-counts (frequencies (vals (frequencies (map rank hand))))]
     (= (get rank-counts 2) 2))) ;filter vals freqs?

;(def my-straight ["2H" "3S" "6C" "5D" "4D"])
;(def my-ace ["2H" "3S" "AC" "5D" "4D"])
;(def my-bad-ace ["2H" "4S" "AC" "8D" "4D"])
;(def my-vals [2 3 4 5 6])
;(def my-ace-vals [2 3 4 5 14])
;(def my-low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])

(defn straight? [hand]
  (let [sorted-hand (sort (set (map rank hand)))
        normal-hand (= sorted-hand (range (nth sorted-hand 0) (+ (nth sorted-hand 0) 5)))
        low-ace (= sorted-hand [2 3 4 5 14])]
   (if (or normal-hand low-ace)
      true
      false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    (high-card? hand) 0
    )
  )
