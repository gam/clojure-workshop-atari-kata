(ns atari.core
  (:use [midje.sweet]))

(unfinished )

(def column :x)
(def row :y)

(defn same [lookup stone other-stone]
 (= (lookup stone)
    (lookup other-stone)))

(defn adjacent [lookup stone other-stone]
 (= 1 (clojure.contrib.math/abs (- (lookup stone) (lookup other-stone)))))

(tabular
 (fact "Stones are adjacent if their rows are offset by one"
  (adjacent row ?stone ?other-stone) => ?expected)
 ?stone    ?other-stone ?expected
 {row 1}    {row 2}     true
 {row 3}    {row 4}     true
 {row 17}   {row 18}    true
 {row 18}   {row 0}     false
 {row 0}    {row 2}     false)

(tabular
 (fact "Stones are adjacent if their columns are offset by one"
  (adjacent column ?stone ?other-stone) => ?expected)
 ?stone        ?other-stone ?expected
 {column 1}    {column 2}     true
 {column 3}    {column 4}     true
 {column 17}   {column 18}    true
 {column 18}   {column 0}     false
 {column 0}    {column 2}     false)

(defn adjacent? [stone placed-stones]
 (filter (fn [candidate]
           (or (and (same row stone candidate)
                    (adjacent column stone candidate))
               (and (same column stone candidate)
                    (adjacent row stone candidate))))
         placed-stones))

(fact "Adjacent stones are either adjacent on the same row or column"
 (adjacent? .stone.
            [.an-adjacent-stone. .a-non-adjacent-stone.]) =>
[.an-adjacent-stone.]
 (provided
   (same row .stone. .an-adjacent-stone.) => true
   (adjacent column .stone. .an-adjacent-stone.) => true
   (same row .stone. .a-non-adjacent-stone.) => false
   (same column .stone. .a-non-adjacent-stone.) => true
   (adjacent row .stone. .a-non-adjacent-stone.) => false))

(defn is-edge-column? [column]
 (or (= column 0)
     (= column 18)))

(tabular
 (fact "A column is an edge column if it is 0 or 18"
  (is-edge-column? ?column) => ?expected)
 ?column ?expected
 0    true
 18   true

 1    false
 2    false)

(defn is-edge-row? [row]
 (or (= row 0)
     (= row 18)))

;.;. Happiness comes when you believe that you have done something truly
;.;. meaningful. -- Yan
(tabular
 (fact "A row is an edge row if it is 0 or 18"
  (is-edge-row? ?row) => ?expected)
 ?row ?expected
 0    true
 18   true

 1    false
 2    false)

(defn is-corner? [stone]
 (and  (is-edge-column? (column stone))
       (is-edge-row? (row stone))))

(fact "A stone is a corner stone if it is on two edges"
 (is-corner? .corner-stone.) => true
 (provided
   (is-edge-column? (column .corner-stone.)) => true
   (is-edge-row? (row .corner-stone.)) => true))

(defn is-edge? [stone]
 (and (or (is-edge-column? (column stone))
          (is-edge-row? (row stone)))
      (not (is-corner? stone))))

(fact "A stone is an edge stone if it has exactly one edge"
 (is-edge? .edge-stone.) => true
 (provided
   (is-edge-column? (column .edge-stone.)) => true
   (is-edge-row? (row .edge-stone.)) => false)
 (is-edge? .corner-stone.) => false
 (provided
   (is-edge-column? (column .corner-stone.)) => true
   (is-edge-row? (row .corner-stone.)) => true))

(defn adjacent-stones [stone placed-stones]
 (filter #( adjacent? stone %) placed-stones))

(fact "Adjacent stones are placed stones that are next to a stone"
  (adjacent-stones .stone. [.an-adjacent-stone.
                            .a-non-adjacent-stone.]) => [.an-adjacent-stone.]
 (provided
   (adjacent? .stone. .an-adjacent-stone.) => true
   (adjacent? .stone. .a-non-adjacent-stone.) => false))

(defn available-liberties [stone]
 (cond
  (is-corner? stone) 2
  (is-edge? stone) 3
  :else 4))

(fact "Available-liberties is 2 at corner, 3 at edge and 4 otherwise"
  (against-background
    (is-corner? anything) => false
    (is-edge? anything) => false
    (is-corner? .corner-stone.) => true
    (is-edge? .edge-stone.) => true)
  
 (available-liberties .corner-stone.) => 2
 (available-liberties .edge-stone.)   => 3
 (available-liberties .other-stone.)  => 4)

(defn liberties [stone placed-stones]
 (- (available-liberties stone) (count (adjacent-stones stone placed-stones))))

(fact "Number of liberties are the difference between available
liberties and adjacent  stones"
 (liberties .stone. .enemy-stones.) => 2
 (provided
   (available-liberties .stone.) => 3
   (adjacent-stones .stone. .enemy-stones.) => [.a-stone.]))

(defn in-atari? [stone placed-stones]
 (= 1 (liberties stone placed-stones)))

(fact "A stone is in atari when it only has one liberty "
 (in-atari? .stone. .placed-stones.) => true
 (provided
   (liberties .stone. .placed-stones.) => 1))
