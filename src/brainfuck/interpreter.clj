(ns brainfuck.interpreter)

(load "interpreter_jit")
(load "interpreter_slow")

(defn run-machine
  [code input]
    (machine-loop code 0 (transient []) 0 input (matching-cache code)))

(defn run-machine-jit
  [code input]
  (let [c (compile-code code)
        [mem pointer input] (run-op c input)]
    {:memory (persistent! mem) :pointer pointer}))
