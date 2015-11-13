(ns brainfuck.interpreter)

(defn- pad-zeroes
  [m addr]
  (if (<= (count m) addr)
      (recur (conj! m 0) addr)
      m))

(defn- write-memory
  [m addr value]
  (assoc! (pad-zeroes m addr) addr value))

(defn- read-memory
  [m addr]
  (or (get m addr) 0))

(defn- add-memory
  [m addr value]
  (write-memory m addr (+ value (read-memory m addr))))

(defn- seek
  [offset [memory pointer input]]
  [memory (+ pointer offset) input])

(defn- add
  [n [memory pointer input]]
  [(write-memory memory pointer (+ n (read-memory memory pointer))) pointer input])

(defn- read-char
  [[memory pointer input]]
  (if (empty? input)
      [memory pointer input]
      [(write-memory memory pointer (int (first input))) pointer (rest input)]))

(defn- write-char
  [[memory pointer input]]
  (do (print (char (read-memory memory pointer)))
      (flush)
      [memory pointer input]))

(defn- join-ops
  [ops]
  (fn [state]
      (loop [s state o ops]
        (if (empty? o)
            s
            (recur ((first o) s) (rest o))))))

(defn- loop-op
  [op]
  (fn [[mem pointer input]]
    (if (= 0 (read-memory mem pointer))
        [mem pointer input]
        (recur (op [mem pointer input])))))
