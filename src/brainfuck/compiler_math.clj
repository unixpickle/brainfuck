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
  "Sum two registers and save the result in a third register.
   This will not modify any registers besides the sum register."
  [r1 r2 sum]
  (str (mov-reg sum r1)
       (push-stack r2)
       (while-reg r2
                  (dec-reg r2)
                  (inc-reg sum))
       (pop-stack r2)))

(defn add-bf
  "Sum the return values of any number of code blocks and return the result.
   This may modify the scratch registers, and so may the expressions."
  [& expressions]
  (if (<= (count expressions) 1)
      (str (first expressions))
      (str (first expressions)
           (push-stack return-value-reg)
           (apply add-bf (rest expressions))
           (pop-stack scratch-reg-1)
           (mov-reg scratch-reg-2 return-value-reg)
           (add-regs scratch-reg-1 scratch-reg-2 return-value-reg))))

(defn div-regs
  "Compute a/b and (a mod b).
   If b is 0, this will return 0.
   None of the registers may be the return value register, since this uses
   said register internally."
  [a b quotient modulus]
  (if-bf (return-reg b)
         (str (push-stack a)
              (push-stack b)
              (set-reg quotient 0)
              (set-reg modulus 0)
              (while-reg a
                         (while-reg b
                                    (dec-reg b)
                                    (dec-reg a)
                                    (inc-reg modulus)
                                    (if-not-bf (return-reg a)
                                               (if-bf (return-reg b)
                                                      (set-reg b 0)
                                                      (str (set-reg modulus 0)
                                                           (inc-reg quotient)))))
                         (if-bf (return-reg a)
                                (str (peak-stack b)
                                     (inc-reg quotient)
                                     (set-reg modulus 0))))
              (pop-stack b)
              (pop-stack a))
         (str (set-reg quotient 0)
              (set-reg modulus 0))))
