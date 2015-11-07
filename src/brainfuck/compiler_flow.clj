(ns brainfuck.compiler)

(defn- while-reg
  "Run some code while the given register is not 0."
  [reg & body]
  (deep-str (seek-mem-to-reg reg) "[" seek-reg-to-mem
            body (seek-mem-to-reg reg) "]"
            seek-reg-to-mem))

(defn while-bf
  "Run a body of code while block of \"condition\" code returns a non-zero value."
  [condition & body]
  (str condition (while-reg return-value-reg body condition)))

(defn if-bf
  "If a block of \"condition\" returns a non-zero value, run if-body, otherwise
   run else-body.
   The if-body and else-body may modify any registers they please.
   Neither the if-body nor the else-body should leave the stack modified.
   The return value register may be affected at any time during an if statement."
  [condition if-body else-body]
  (str (set-reg return-value-reg 1)
       (push-stack return-value-reg)
       condition
       (while-reg return-value-reg
                  if-body
                  (pop-stack return-value-reg)
                  (set-reg return-value-reg 0)
                  (push-stack return-value-reg))
       (pop-stack return-value-reg)
       (while-reg return-value-reg
                  else-body
                  (set-reg return-value-reg 0))))
