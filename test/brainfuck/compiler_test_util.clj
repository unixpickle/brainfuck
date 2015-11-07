(ns brainfuck.compiler-test)

(def initial-state
  (let [regs (apply concat (repeat reg-count '(0 256 1 2)))
        prefix (repeat (+ 4 scratch-size) 0)
        suffix '(0 2)]
    (concat prefix regs suffix)))

(defn state-write-byte
  [state idx val]
  (if (< (count state) idx)
      (state-write-byte (concat (vec state) [0]) idx val)
      (assoc (vec state) idx val)))

(defn state-read-byte
  [state idx]
  (or (get state idx) 0))

(defn state-set-scratch
  [state idx val]
  (state-write-byte state idx val))

(defn state-set-reg
  [state idx val]
  (let [regIdx (+ 4 scratch-size (* 4 idx))]
    (assoc (vec state) regIdx val (inc regIdx) (- 256 val))))

(defn state-push-stack
  [state val]
  (loop [i (+ 5 scratch-size (* 4 reg-count))]
    (if (= 0 (state-read-byte state i))
        (-> state (state-write-byte i 1) (state-write-byte (dec i) val))
        (recur (+ i 4)))))

(defn states-equal
  [s1 s2]
  (cond (< (count s1) (count s2)) (states-equal (concat s1 [0]) s2)
        (> (count s1) (count s2)) (states-equal s2 s1)
        :else (= (seq s1) (seq s2))))

(defn are-states-equal
  [s1 s2]
  (dorun (map #(is (states-equal %1 %2)) s1 s2)))
