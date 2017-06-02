(ns p-p-p-pokerface)

(def high-sevenx ["2H" "3S" "4C" "5C" "7D"])

(def pair-handx ["2H" "2S" "4C" "5C" "7D"])

(def pair-handsx #{["2H" "2S" "4C" "5C" "7D"]
                  ["2S" "4S" "4C" "9D" "KS"]})
(def full-house-handx ["2H" "5D" "2D" "2C" "5S"])
(def straight-handx ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-handx ["2H" "3S" "4C" "5D" "AD"])

(def replacements-high {\T 10, \J 11, \Q 12, \K 13, \A 14,})
(def replacements-low {\T 10, \J 11, \Q 12, \K 13, \A 1,})

(defn rank [card]
  (let [[card-rank _] card]
    (if (Character/isDigit card-rank)
    (Integer/valueOf (str card-rank))
    (replacements-high card-rank))))

(defn rank-low [card]
  (let [[card-rank _] card]
    (if (Character/isDigit card-rank)
    (Integer/valueOf (str card-rank))
    (replacements-low card-rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (and
      (== 3 (apply max ranks))
      (== 2 (apply min ranks)))))

(defn two-pairs? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (=
      (seq [1 2 2])
      (sort ranks))))

(defn straight? [hand]
  (let [ranks-high (map rank hand)
        ranks-low (map rank-low hand)
        is-straight? (fn [ranks]
                       (=
                          (range
                            (apply min ranks)
                            (+ 1 (apply max ranks)))
                          (sort ranks)))]
    (or
      (is-straight? ranks-high)
      (is-straight? ranks-low))))

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matches (filter
                   (fn [checker]
                    ((first checker) hand))
                   checkers)]
    (apply max (map second matches))))


