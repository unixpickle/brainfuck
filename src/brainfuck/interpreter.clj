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

(defn- write-memory
  [memory pointer v]
  (concat (take pointer memory) (list v) (drop (inc pointer) memory)))

(defn- add-memory
  [m p s]
  (write-memory m p (+ s (nth m p))))

(defn- machine-loop
  [tokens ip memory pointer input]
  (let [inst (get tokens ip)]
    (cond (= inst nil) nil
          (= inst \>) (recur tokens (inc ip) memory (inc pointer) input)
          (= inst \<) (recur tokens (inc ip) memory (dec pointer) input)
          (= inst \+) (recur tokens (inc ip) (add-memory memory pointer 1) pointer input)
          (= inst \-) (recur tokens (inc ip) (add-memory memory pointer -1) pointer input)
          (= inst \.) (do
                        (print (char (nth memory pointer)))
                        (recur tokens (inc ip) memory pointer input))
          (= inst \,) (if (empty? input)
                          (recur tokens (inc ip) (write-memory memory pointer 0) pointer input)
                          (let [c (int (first input)) ni (rest input)]
                            (recur tokens (inc ip) (write-memory memory pointer c) pointer ni)))
          (= inst \[) (if (= 0 (nth memory pointer))
                          (recur tokens (matching-close tokens ip) memory pointer input)
                          (recur tokens (inc ip) memory pointer input))
          (= inst \]) (if (= 0 (nth memory pointer))
                          (recur tokens (inc ip) memory pointer input)
                          (recur tokens (matching-open tokens ip) memory pointer input))
          :else (recur tokens (inc ip) memory pointer input))))

(defn run-machine
  [code input]
  (machine-loop code 0 (repeat 0) 0 input))
