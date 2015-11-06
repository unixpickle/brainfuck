(ns brainfuck.compiler)

(defn- deep-str
  [& x]
  (apply str (map (partial apply str) x)))

(defn- seek
  "Generate a string of <'s or >'s."
  [start end]
  (let [x (- end start)]
    (deep-str (repeat x ">") (repeat (- x) "<"))))
