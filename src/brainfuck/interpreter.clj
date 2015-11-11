(ns brainfuck.interpreter)

(defn- matching-bracket
  [tokens startIndex direction]
  (loop [i startIndex c 0]
    (let [chr (get tokens i)]
      (assert (not (= chr nil)))
      (cond (= chr \[) (if (= c -1) (inc i) (recur (direction i) (inc c)))
            (= chr \]) (if (= c 1) (inc i) (recur (direction i) (dec c)))
            :else (recur (direction i) c)))))

(defn- matching-close
  [tokens openIndex]
  (matching-bracket tokens openIndex inc))

(defn- matching-open
  [tokens closeIndex]
  (matching-bracket tokens closeIndex dec))

(defn- matching-cache
  [tokens]
  (loop [v (vec (repeat (count tokens) 0)) i 0]
    (if (= i (count tokens))
        v
        (case (get tokens i)
              \[ (recur (assoc v i (matching-close tokens i)) (inc i))
              \] (recur (assoc v i (matching-open tokens i)) (inc i))
              (recur v (inc i))))))

(defn- pad-zeroes
  [m addr]
  (if (<= (count m) addr)
      (recur (conj m 0) addr)
      m))

(defn- write-memory
  [m addr value]
  (assoc (pad-zeroes m addr) addr value))

(defn- read-memory
  [m addr]
  (or (get m addr) 0))

(defn- add-memory
  [m addr value]
  (write-memory m addr (+ value (read-memory m addr))))

(defn- machine-loop
  [tokens ip memory pointer input cache]
  (let [inst (get tokens ip)]
    (case inst
          nil {:memory memory :pointer pointer}
          \> (recur tokens (inc ip) memory (inc pointer) input cache)
          \< (recur tokens (inc ip) memory (dec pointer) input cache)
          \+ (recur tokens (inc ip) (add-memory memory pointer 1) pointer input cache)
          \- (recur tokens (inc ip) (add-memory memory pointer -1) pointer input cache)
          \. (do
               (print (char (read-memory memory pointer)))
               (flush)
               (recur tokens (inc ip) memory pointer input cache))
          \, (if (empty? input)
                 (recur tokens (inc ip) memory pointer input cache)
                 (let [c (int (first input)) ni (rest input)]
                   (recur tokens (inc ip) (write-memory memory pointer c) pointer ni cache)))
          \[ (if (= 0 (read-memory memory pointer))
                 (recur tokens (get cache ip) memory pointer input cache)
                 (recur tokens (inc ip) memory pointer input cache))
          \] (if (= 0 (read-memory memory pointer))
                 (recur tokens (inc ip) memory pointer input cache)
                 (recur tokens (get cache ip) memory pointer input cache))
          \# (do (println pointer memory)
                 (recur tokens (inc ip) memory pointer input cache))
          (recur tokens (inc ip) memory pointer input cache))))

(defn run-machine
  [code input]
  (machine-loop code 0 [] 0 input (matching-cache code)))
