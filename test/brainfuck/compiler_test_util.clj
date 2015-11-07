(ns brainfuck.compiler-test)

(def initial-state
  (let [regs (apply concat (repeat reg-count '(0 256 1 2)))
        prefix (repeat (+ 4 scratch-size) 0)
        suffix '(0 2)]
    (concat prefix regs suffix)))

(defn state-set-scratch
  [state idx val]
  (seq (assoc (vec state) idx val)))

(defn state-set-reg
  [state idx val]
  (let [regIdx (+ 4 scratch-size (* 4 idx))]
    (seq (assoc (vec state) regIdx val (inc regIdx) (- 256 val)))))

(defn states-equal
  [s1 s2]
  (cond (< (count s1) (count s2)) (states-equal (concat s1 [0]) s2)
        (> (count s1) (count s2)) (states-equal s2 s1)
        :else (= (seq s1) (seq s2))))

(defn are-states-equal
  [s1 s2]
  (dorun (map #(is (states-equal %1 %2)) s1 s2)))
