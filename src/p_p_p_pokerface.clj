(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (let [rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
        (rank-map fst)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn uniq-ranks-and-max-freq [hand uniq-ranks max-freq]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))]
    (and (= (count rank-freqs) uniq-ranks)
         (= (apply max rank-freqs) max-freq))))

(defn pair? [hand]
  (uniq-ranks-and-max-freq hand 4 2))

(defn three-of-a-kind? [hand]
  (uniq-ranks-and-max-freq hand 3 3))

(defn four-of-a-kind? [hand]
  (uniq-ranks-and-max-freq hand 2 4))

(defn in-seq? [seqn]
  (let [min-val (apply min seqn)]
    (= (sort seqn) (range min-val (+ min-val (count seqn))))))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-freqs (vals (frequencies suits))]
    (= (apply max suit-freqs) 5)))

(defn full-house? [hand]
  (uniq-ranks-and-max-freq hand 2 3))

(defn two-pairs? [hand]
  (uniq-ranks-and-max-freq hand 3 2))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or (in-seq? ranks) (in-seq? (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (pair? hand) 1
   (two-pairs? hand) 2
   (three-of-a-kind? hand) 3
   (straight-flush? hand) 8
   (straight? hand) 4
   (flush? hand) 5
   (full-house? hand) 6
   (four-of-a-kind? hand) 7
   :else 0))
