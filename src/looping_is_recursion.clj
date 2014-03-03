(ns looping-is-recursion)

(defn power [base exp]
  (let [looper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (looper 1 exp)))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   :else (if (= (first seq1) (first seq2))
           (recur (rest seq1) (rest seq2))
           false)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         xs a-seq]
    (cond
     (empty? xs) nil
     (pred (first xs)) i
     :else (recur (inc i) (rest xs)))))

(defn avg [a-seq]
  (loop [total 0
         n 0
         xs a-seq]
    (if (empty? xs)
      (if (zero? n)
        0
        (/ total n))
      (recur (+ total (first xs))
             (inc n)
             (rest xs)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         xs a-seq]
    (if (empty? xs)
      acc
      (recur (toggle acc (first xs))
             (rest xs)))))

(defn fast-fibo [n]
  (loop [i 1
         j 0
         m n]
    (if (zero? m)
      j
      (recur (+ i j) i (dec m)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         xs a-seq]
    (cond
     (empty? xs) acc
     (contains? (set acc) (first xs)) acc
     :else (recur (conj acc (first xs)) (rest xs)))))

