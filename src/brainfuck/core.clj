(ns brainfuck.core
  (:require (brainfuck interpreter compiler)))

(def usage-str "Usage: lein run [<compile|interpret>] <file> [interpreter input]")

(defn interpret
  [& args]
  (if (> (count args) 2)
      (println usage-str)
      (let [contents (slurp (first args))
            input (apply str (rest args))]
        (brainfuck.interpreter/run-machine contents input))))

(defn compiler
  ([& args]
   (if (= 1 (count args))
       (println (brainfuck.compiler/compile-code (slurp (first args))))
       (println usage-str))))

(defn -main
  [& args]
  (cond (< (count args) 2) (println usage-str)
        (= (first args) "interpret") (apply interpret (rest args))
        (= (first args) "compile") (apply compiler (rest args))
        :else (println usage-str)))
