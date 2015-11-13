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

(defn- read-until-close-bracket
  ([s] (read-until-close-bracket s [] 1))
  ([s accumulator depth]
   (assert (not (empty? s)) "missing ]")
   (case (first s)
         \] (if (= depth 1)
                [(apply str accumulator) (apply str (rest s))]
                (recur (rest s) (conj accumulator \]) (dec depth)))
         \[ (recur (rest s) (conj accumulator \[) (inc depth))
         (recur (rest s) (conj accumulator (first s)) depth))))

(defn- read-repeated-character
  ([s c] (read-repeated-character s c 0))
  ([s c acc]
   (if (= (first s) c)
       (recur (rest s) c (inc acc))
       [acc s])))

(defn- compile-code
  ([code]
   (compile-code code []))
  ([code acc]
   (case (first code)
         nil (join-ops acc)
         \[ (let [[body remaining] (read-until-close-bracket (rest code))
                  loop-code (loop-op (compile-code body))]
              (recur remaining (conj acc loop-code)))
         \+ (let [[n r] (read-repeated-character code \+)]
              (recur r (conj acc (partial add n))))
         \- (let [[n r] (read-repeated-character code \-)]
              (recur r (conj acc (partial add (- n)))))
         \> (let [[n r] (read-repeated-character code \>)]
              (recur r (conj acc (partial seek n))))
         \< (let [[n r] (read-repeated-character code \<)]
              (recur r (conj acc (partial seek (- n)))))
         \. (recur (rest code) (conj acc (partial write-char)))
         \, (recur (rest code) (conj acc (partial read-char)))
         (recur (rest code) acc))))

(defn run-op
  [op input]
  (op [(transient []) 0 input]))
