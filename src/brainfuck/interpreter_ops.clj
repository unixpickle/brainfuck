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

(defn- reverse-apply
  [a b]
  (b a))

(defn- thread-ops
  [instructions]
  (let [funcs (vec (map #(cons `partial %) instructions))]
    `(fn [state#] (reduce ~reverse-apply state# ~funcs))))

(defn- loop-ops
  [instructions]
  `((fn [[mem# pointer# input#]]
        (if (not (= 0 (read-memory mem# pointer#)))
            (recur (~(thread-ops instructions) [mem# pointer# input#]))
            [mem# pointer# input#]))))
