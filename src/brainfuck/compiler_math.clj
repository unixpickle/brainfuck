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
   This preserves the state of the preferred registers.
   If the expressions modify the preferred registers, the changes will be lost."
  [& expressions]
  (if (<= (count expressions) 1)
      (str (first expressions))
      (str (push-stack preferred-reg-1)
           (push-stack preferred-reg-2)
           (first expressions)
           (push-stack return-value-reg)
           (add-bf (rest expressions))
           (pop-stack preferred-reg-1)
           (mov-reg preferred-reg-2 return-value-reg)
           (add-regs preferred-reg-1 preferred-reg-2 return-value-reg)
           (pop-stack preferred-reg-2)
           (pop-stack preferred-reg-1))))
