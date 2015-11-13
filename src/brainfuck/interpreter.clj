(ns brainfuck.interpreter)

(load "interpreter_ops")

(defn- read-until-close-bracket
  ([s]
   (read-until-close-bracket s [] 1))
  ([s accumulator depth]
   (assert (not (empty? s)) "missing ]")
   (case (first s)
         \] (if (= depth 1)
                [(apply str accumulator) (apply str (rest s))]
                (recur (rest s) (conj accumulator \]) (dec depth)))
         \[ (recur (rest s) (conj accumulator \[) (inc depth))
         (recur (rest s) (conj accumulator (first s)) depth))))

(defn- compile-code
  ([code]
   (compile-code code []))
  ([code acc]
   (case (first code)
         nil (join-ops acc)
         \[ (let [[body remaining] (read-until-close-bracket (rest code))
                  loop-code (loop-op (compile-code body))]
              (recur remaining (conj acc loop-code)))
         \+ (recur (rest code) (conj acc (partial add 1)))
         \- (recur (rest code) (conj acc (partial add -1)))
         \> (recur (rest code) (conj acc (partial seek 1)))
         \< (recur (rest code) (conj acc (partial seek -1)))
         \. (recur (rest code) (conj acc (partial write-char)))
         \, (recur (rest code) (conj acc (partial read-char)))
         (recur (rest code) acc))))

(defn run-op
  [op input]
  (op [(transient []) 0 input]))

(defn run-machine
  [code input]
  (let [c (compile-code code)
        [mem pointer input] (run-op c input)]
    {:memory (persistent! mem) :pointer pointer}))
