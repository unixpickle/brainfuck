; Print out fibonacci numbers
; Memory layout is: (longer_len, longer0, shorter0, longer1, shorter1, ...)

; Set the first three bytes of memory to 1.
(set-reg 0 3)
(while-reg 0
           (dec-reg 0)
           (memwrite-bf (return-num 1))
           (memseek-up-bf (return-num 1)))

(while-bf (return-num 1)
          memseek-zero

          ; Register 1 is longer_len.
          (memread 0)
          
          (memseek-up-bf (return-num 1))

          ; Register 1 is the carry flag.
          (set-reg 1 0)
          (while-reg 0
                     (dec-reg 0)

                     ; Reg 3 is the digit from the longer string
                     (memread 3)
                     (memseek-up-bf (return-num 1))
                     ; Reg 2 is the digit from the shorter string
                     (memread 2)

                     ; Set reg 4 to 1+2+3
                     (add-regs 2 3 5)
                     (add-regs 5 1 4)

                     (set-reg 2 10)

                     ; Set reg 1 to (2+3)/10 and reg 5 to (2+3)%10
                     (div-regs 4 2 1 5)

                     (memwrite 3)
                     (memseek-down-bf (return-num 1))
                     (memwrite 5)
                     (memseek-up-bf (return-num 2)))

          (if-bf (return-reg 1)
                 (str (memwrite 1)
                      memseek-zero
                      (memwrite-bf (add-bf (memread) (return-num 1))))
                 (memseek-down-bf (return-num 1)))

          memseek-zero
          (memread 0)
          (memseek-up 0)
          (memseek-up 0)
          (memseek-down-bf (return-num 1))
          (while-reg 0
                     (dec-reg 0)
                     (print-bf (add-bf (return-num 0x30) (memread)))
                     (memseek-down-bf (return-num 2)))
          (print-string "\n"))
