(ns brainfuck.core)
(require 'brainfuck.interpreter)

(defn -main
  [program & input]
  (brainfuck.interpreter/run-machine (slurp program) (apply str input)))
