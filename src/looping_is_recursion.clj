(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1
         n exp]
    (if (zero? n)
      acc
      (recur (* acc base) (dec n)))))

(defn last-element [a-seq]
  (let [rst (rest a-seq)]
    (if (empty? rst)
      (first a-seq)
      (recur rst))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         rst a-seq]
    (cond
      (empty? rst) nil
      (pred (first rst)) index
      :else (recur (inc index) (rest rst)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         rst a-seq]
    (if (empty? rst)
      (if (zero? count)
        0
        (/ sum count))
      (recur (+ sum (first rst)) (inc count) (rest rst)))))

(defn parity [a-seq]
  (loop [parities {}
         rst a-seq]
    (let [fst (first rst)
          fst-val (parities fst)]
      (if (empty? rst)
        (set (keys (filter (comp odd? val) parities)))
        (recur (assoc parities fst (if (nil? fst-val)
                                     1
                                     (inc fst-val))) (rest rst))))))

(defn fast-fibo [n]
  (loop [fn-1 1
         fn-0 2
         current 3]
    (cond
      (= n 0) 0
      (= n 1) 1
      (= n 2) 1
      (= n current) fn-0
      :else (recur fn-0 (+ fn-1 fn-0) (inc current)))))

(defn cut-at-repetition [a-seq]
  (loop [visited #{}
         to-take 0
         rst a-seq]
    (let [fst (first rst)]
      (if (or (empty? rst)
              (contains? visited fst))
        (take to-take a-seq)
        (recur (conj visited fst) (inc to-take) (rest rst))))))
