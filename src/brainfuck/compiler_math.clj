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
  (with-reg sum
            reset-current-reg
            (seek-between-regs sum r1)
            (reg-to-scratch (dec scratch-size) (- scratch-size 2))
            "[-" (seek-between-regs r1 sum) ">-<+" (seek-between-regs sum r1) "]"
            (scratch-to-reg (dec scratch-size) false)
            (seek-between-regs r1 r2)
            (reg-to-scratch (dec scratch-size) (- scratch-size 2))
            "[-" (seek-between-regs r2 sum) inc-current-reg (seek-between-regs sum r2) "]"
            (scratch-to-reg (dec scratch-size) false)))

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

(defn equal-regs
  "Set a register to a non-zero value if two registers are equal, or a zero
   value otherwise.
   The result register may be one of the operand registers, and the operand registers
   may be the same (although that seems pointless)."
  [r1 r2 result]
  (str (seek-mem-to-reg r1)
       (reg-to-scratch (- scratch-size 5) (- scratch-size 4))
       (seek-between-regs r1 r2)
       (reg-to-scratch (- scratch-size 4) (- scratch-size 3))
       "<<[<<<<]<<"
       "<+<<+<<"
       ; Current scratch layout is |a|b|1|0|1|.
       ; We are pointed at a, and the third byte is our return value.
       "[->>[-]<[->+>]>-[+>>-]+<<<<]"
       ">[[-]>-<]>>>->"
       (seek scratch-size (+ scratch-size 4 (* 4 result)))
       reset-current-reg
       (scratch-to-reg (- scratch-size 3))
       seek-reg-to-mem))

(defn equal-bf
  "Return a non-zero value if all blocks return the same value.
   This will mess with the scratch registers (possibly between block calls)."
  [& blocks]
  (cond (< (count blocks) 2) (return-num 1)
        (= (count blocks) 2) (str (first blocks)
                                  (push-stack return-value-reg)
                                  (second blocks)
                                  (pop-stack scratch-reg-1)
                                  (equal-regs scratch-reg-1 return-value-reg
                                              return-value-reg))
        :else (str (if-bf (apply equal-bf (rest blocks))
                          (str (apply equal-bf (take 2 blocks))
                               (mov-reg scratch-reg-1 return-value-reg))
                          (set-reg scratch-reg-1 0))
                   (return-reg scratch-reg-1))))

(defn less-than-bf
  "Return a non-zero value if the first block is less than the second block.
   This assumes that both values are unsigned 8-bit integers.
   This will mess with the scratch registers."
  [a b]
  (str b
       (mov-reg scratch-reg-2 return-value-reg)
       (if-bf (return-reg scratch-reg-2)
              (str (push-stack scratch-reg-2)
                   a
                   (mov-reg scratch-reg-1 return-value-reg)
                   (pop-stack scratch-reg-2)
                   (while-reg scratch-reg-1
                              (dec-reg scratch-reg-1)
                              (dec-reg scratch-reg-2)
                              (if-not-bf (return-reg scratch-reg-2)
                                         (set-reg scratch-reg-1 0)))))
       (return-reg scratch-reg-2)))

(defn greater-than-bf
  "Return a non-zero value if the first block is greater than the second block.
   This assumes that both values are unsigned 8-bit integers.
   This will mess with the scratch registers."
  [a b]
  (less-than-bf b a))

(defn logical-or-bf
  "Perform a logical OR on one or more expressions.
   This will return a non-zero value if any of the input values were non-zero.
   This will mess with the scratch registers (possibly between block calls)."
  [v1 & vs]
  (if (zero? (count vs))
      v1
      (str (set-reg scratch-reg-1 0)
           (if-bf v1
                  (set-reg scratch-reg-1 1)
                  (str (apply logical-or-bf vs)
                       (mov-reg scratch-reg-1 return-value-reg)))
           (return-reg scratch-reg-1))))
