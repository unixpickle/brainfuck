(ns brainfuck.compiler)

(defn inc-reg
  "Increment the given register."
  [reg]
  (with-reg reg inc-current-reg))

(defn dec-reg
  "Decrement the given register."
  [reg]
  (with-reg reg dec-current-reg))

(defn add-regs
  "Sum two registers and save the result in a third register."
  [r1 r2 sum]
  (str (set-reg sum 0)
       (push-stack r1)
       (while-reg r1
                  (dec-reg r1)
                  (inc-reg sum))
       (pop-stack r1)
       (push-stack r2)
       (while-reg r2
                  (dec-reg r2)
                  (inc-reg sum))
       (pop-stack r2)))

(defn add-bf
  "Sum the return values of any number of code blocks and return the result.
   This has a transparent effect on all registers except the return register,
   meaning that the expressions may modify any registers they wish.
   The expressions should leave the stack as they found it."
  [& expressions]
  (if (<= (count expressions) 1)
      (str (first expressions))
      (str (first expressions)
           (push-stack return-value-reg)
           (add-bf (rest expressions))
           (push-stack preferred-reg-1)
           (peak-stack preferred-reg-1 1)
           (push-stack preferred-reg-2)
           (mov-reg preferred-reg-2 return-value-reg)
           (add-regs preferred-reg-1 preferred-reg-2 return-value-reg)
           (pop-stack preferred-reg-2)
           (pop-stack preferred-reg-1)
           (pop-stack))))
