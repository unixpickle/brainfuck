(ns brainfuck.compiler)

(defn- while-reg
  "Run some code while the given register is not 0."
  [reg & code]
  (deep-str (seek-mem-to-reg reg) "[" seek-reg-to-mem
            code (seek-mem-to-reg reg) "]"
            seek-reg-to-mem))

(defn while-bf
  "Run a body of code while block of \"condition\" code sets register 0
   to a non-zero value."
  [condition & body]
  (str condition (while-reg 0 code condition)))

(defn if-bf
  "If a block of \"condition\" code sets register 0 to a non-zero value,
   run the if body; otherwise, run the else body.
   The if body and else body may modify any registers they please.
   Neither the if body nor the else body should modify the stack.
   This may change the value of register 0, but it will not affect others."
  [condition if-body else-body]
  (str (set-reg 0 1)
       (push-stack 0)
       condition
       (while-reg 0
                  if-body
                  (pop-stack 0)
                  (set-reg 0 0)
                  (push-stack 0))
       (pop-stack 0)
       (while-reg 0
                  else-body
                  (set-reg 0 0))))
