(ns brainfuck.core)
(require '[clojure.string :as str])

(defn- tokenize
  [x]
  (into [] (map (comp str identity) x)))

(defn- matching-bracket
  [tokens startIndex direction]
  (loop [i startIndex c 0]
    (let [chr (get tokens i)]
      (cond (= chr "[") (if (= c -1) (inc i) (recur (direction i) (inc c)))
            (= chr "]") (if (= c 1) (inc i) (recur (direction i) (dec c)))
            :else (recur (direction i) c)))))

(defn- matching-close
  [tokens openIndex]
  (matching-bracket tokens openIndex inc))

(defn- matching-open
  [tokens closeIndex]
  (matching-bracket tokens closeIndex dec))

(defn- pad-zeros
  [memory lastIndex]
  (let [deficit (- (inc lastIndex) (count memory))]
    (vec (concat memory (repeat deficit 0)))))

(defn- read-memory [memory pointer] (or (get memory pointer) 0))

(defn- write-memory
  [memory pointer v]
  (assoc (pad-zeros memory pointer) pointer v))

(defn- add-memory
  [memory pointer s]
  (write-memory memory pointer (+ s (read-memory memory pointer))))

(defn- run-machine
  [tokens' ip' memory' pointer' input']
  (loop [tokens tokens' ip ip' memory memory' pointer pointer' input input']
    (let [inst (get tokens ip)]
      (cond (= inst nil) nil
            (= inst ">") (recur tokens (inc ip) memory (inc pointer) input)
            (= inst "<") (recur tokens (inc ip) memory (dec pointer) input)
            (= inst "+") (recur tokens (inc ip) (add-memory memory pointer 1) pointer input)
            (= inst "-") (recur tokens (inc ip) (add-memory memory pointer -1) pointer input)
            (= inst ".") (do
                           (print (char (read-memory memory pointer)))
                           (recur tokens (inc ip) memory pointer input))
            (= inst ",") (if (empty? input)
                             (recur tokens (inc ip) (write-memory memory pointer 0) pointer input)
                             (let [c (read-memory memory pointer) ni (rest input)]
                               (recur tokens (inc ip) (write-memory memory pointer c) pointer ni)))
            (= inst "[") (if (= 0 (read-memory memory pointer))
                             (recur tokens (matching-close tokens ip) memory pointer input)
                             (recur tokens (inc ip) memory pointer input))
            (= inst "]") (if (= 0 (read-memory memory pointer))
                             (recur tokens (inc ip) memory pointer input)
                             (recur tokens (matching-open tokens ip) memory pointer input))
            :else (recur tokens (inc ip) memory pointer input)))))

(defn -main
  [program & input]
  (run-machine (tokenize (slurp program)) 0 [] 0 (apply str input)))
