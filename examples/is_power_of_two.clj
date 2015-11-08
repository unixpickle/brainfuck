(set-reg 1 -0x30)
(set-reg 6 0)
(read-char 0)
(while-bf (return-reg 0)
          (mov-reg 2 6)
          (memseek-up 2)
          (memseek-up 2)
          ; Shift the input buffer to the right.
          (while-reg 2
                     (dec-reg 2)
                     (memseek-down-bf (return-num 2))
                     (memread 3)
                     (memseek-up-bf (return-num 2))
                     (memwrite 3)
                     (memseek-down-bf (return-num 2)))
          memseek-zero
          (memseek-up-bf (return-num 2))
          (inc-reg 6)
          (add-regs 0 1 2)
          (memwrite 2)
          (read-char 0))

(print-string "Processing...\n")

memseek-zero
(memseek-up-bf (return-num 1))

(set-reg 0 1)
(set-reg 3 10)
(memwrite-bf (return-num 1))

(while-bf (logical-or-bf (less-than-bf (return-reg 0) (return-reg 6))
                         (equal-bf (return-reg 0) (return-reg 6)))
          (mov-reg 1 0)
          (set-reg 4 0)

          ; Multiply the number by two, leaving the last carry bit in register 4.
          (while-reg 1
                     (dec-reg 1)
                     (add-bf (memread) (memread) (return-reg 4))
                     (mov-reg 2 return-value-reg)
                     (div-regs 2 3 4 5)
                     (memwrite 5)
                     (memseek-up-bf (return-num 2)))

          ; If we carried, add it to the cell and increment the count
          (if-bf (return-reg 4)
                 (str (memwrite 4)
                      (inc-reg 0))
                 (memseek-down-bf (return-num 2)))

          ; If the square is the right length, check if we've got it.
          (if-bf (equal-bf (return-reg 0) (return-reg 6))
                 (str (mov-reg 1 0)
                      (set-reg 2 1)
                      (while-reg 1
                                 (dec-reg 1)
                                 (memread 4)
                                 (memseek-up-bf (return-num 1))
                                 (if-not-bf (equal-bf (return-reg 4) (memread))
                                            (set-reg 2 0))
                                 (memseek-down-bf (return-num 3)))
                      (memseek-up-bf (return-num 1))
                      (if-bf (return-reg 2) (set-reg 6 0)))
                 (str memseek-zero
                      (memseek-up-bf (return-num 1)))))

(if-bf (return-reg 6)
       (print-string "Not square\n")
       (print-string "Square\n"))
