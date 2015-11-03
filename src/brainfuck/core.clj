(ns brainfuck.core)
(require '[clojure.string :as str])

(defn- tokenize
  [x]
  (into [] (map (comp str identity) x)))

(defn- matching-close
  [x openI]
  (loop [i openI c 0]
    (let [chr (get x i)]
      (assert (not (= chr nil)))
      (println i c chr)
      (cond (= chr "[") (recur (inc i) (inc c))
            (= chr "]") (if (= c 1) (inc i) (recur (inc i) (dec c)))
            :else (recur (inc i) c)))))

(defn- matching-open
  [x closeI]
  (loop [i closeI c 0]
    (let [chr (get x i)]
      (assert (not (= chr nil)))
      (cond (= chr "]") (recur (dec i) (inc c))
            (= chr "[") (if (= c 1) (inc i) (recur (dec i) (dec c)))
            :else (recur (dec i) c)))))

(defn- read-memory [memory pointer] (or (get memory pointer) 0))

(defn- pad-zeros
  [memory lastIndex]
  (let [deficit (- (inc lastIndex) (count memory))]
    (vec (concat memory (repeat deficit 0)))))

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
