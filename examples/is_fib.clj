; Tell if a decimal input is a fibonacci number
; Memory layout is: (input_len, longer_len, input0, longer0, shorter0, ...)

; Read input from the console.
(read-char 0)
(while-reg 0
           (memread 1)
           (inc-reg 1)
           (memwrite 1)
           (memseek-up-bf (return-num 2))

           ; Move the existing buffer up by 3 bytes.
           (dec-reg 1)
           (memseek-up 1)
           (memseek-up 1)
           (memseek-up 1)
           (while-reg 1
                      (dec-reg 1)
                      (set-reg 3 3)
                      (memseek-down 3)
                      (memread 2)
                      (memseek-up 3)
                      (memwrite 2)
                      (memseek-down 3))

           (memwrite-bf (add-bf (return-reg 0) (return-num -0x30)))
           (read-char 0)
           memseek-zero)

(print-string "Processing...\n")

; Set longer_len, longer0, and shorter0 to 1.
(set-reg 1 1)
(memseek-up 1)
(memwrite 1)
(memseek-up 1)
(memseek-up 1)
(memwrite 1)
(memseek-up 1)
(memwrite 1)

; Set reg 0 to input_len and reg 1 to longer_len
memseek-zero
(memread 0)
(memseek-up 1)
(memread 1)

; If the input was empty, tamper with evidence.
(if-not-bf (return-reg 0)
           (str memseek-zero
                (memwrite-bf (return-num 1))
                (inc-reg 1)))

(while-bf (logical-or-bf (equal-bf (return-reg 0) (return-reg 1))
                         (greater-than-bf (return-reg 0) (return-reg 1)))

          ; Check if the current Fibonacci number is equal to the input.
          (if-bf (equal-bf (return-reg 0) (return-reg 1))
                 (str (set-reg 2 1)
                      memseek-zero
                      (memseek-up-bf (return-num 2))
                      (while-reg 0
                                 (dec-reg 0)
                                 (memread 3)
                                 (memseek-up-bf (return-num 1))
                                 (memread 4)
                                 (memseek-up-bf (return-num 2))
                                 (if-not-bf (equal-bf (return-reg 3) (return-reg 4))
                                            (set-reg 2 0)))
                      (if-bf (return-reg 2)
                             (str memseek-zero
                                  (memwrite-bf (return-num 0))))))

          memseek-zero
          (memseek-up-bf (return-num 1))

          ; Register 0 is longer_len.
          (memread 0)

          (memseek-up-bf (return-num 2))

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
                     (memseek-up-bf (return-num 3)))

          ; If the last add carried, increase the length of the string.
          (if-bf (return-reg 1)
                 (str (memwrite 1)
                      memseek-zero
                      (memseek-up-bf (return-num 1))
                      (memwrite-bf (add-bf (memread) (return-num 1)))))

          memseek-zero
          (memread 0)
          (memseek-up-bf (return-num 1))
          (memread 1))

memseek-zero
(if-bf (equal-bf (memread) (return-num 0))
       (print-string "Is Fibonacci.\n")
       (print-string "Not Fibonacci.\n"))
