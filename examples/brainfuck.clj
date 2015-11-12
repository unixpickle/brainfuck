; Memory is laid out as follows:
; <code_byte, cur_code, input_byte, cur_input, tape_byte, cur_tape>
; Code bytes are of the following format:
; - 1
; + 2
; < 3
; > 4
; [ 5
; ] 6
; . 7
; , 8
; Terminate 0

; Set the cur_code, cur_input, and cur_tape bytes.
(memseek-up-num 1)
(memwrite-num 1)
(memseek-up-num 2)
(memwrite-num 1)
(memseek-up-num 2)
(memwrite-num 1)
memseek-zero

; Read the code input.
(read-char 0)
(while-reg 0
           (set-reg 1 0)
           (case-reg 0
                     (int \-) (set-reg 1 1)
                     (int \+) (set-reg 1 2)
                     (int \<) (set-reg 1 3)
                     (int \>) (set-reg 1 4)
                     (int \[) (set-reg 1 5)
                     (int \]) (set-reg 1 6)
                     (int \.) (set-reg 1 7)
                     (int \,) (set-reg 1 8))
           (case-reg 0
                     (int \!) (set-reg 0 0)
                     (str (read-char 0)
                          (if-bf (return-reg 1)
                                 (str (memwrite 1)
                                      (memseek-up-num 6))))))

; Read the standard input.
memseek-zero
(memseek-up-num 2)
(read-char 0)
(while-reg 0
           (memwrite 0)
           (memseek-up-num 6)
           (read-char 0))

(do (defn bf-seek-marker
       [idx]
       (str memseek-zero
            (memseek-up-num idx)
            (while-bf (equal-bf (return-num 0) (memread))
                      (memseek-up-num 6))
            (memseek-down-num 1)))
    (def bf-seek-to-code (bf-seek-marker 1))
    (def bf-seek-to-input (bf-seek-marker 3))
    (def bf-seek-to-tape (bf-seek-marker 5))
    (def bf-seek-to-loop-end
      (str bf-seek-to-code
           (set-reg 1 1)
           (memseek-up-num 1)
           (memwrite-num 0)
           (memseek-down-num 1)
           (while-reg 1
                      (memseek-up-num 6)
                      (memread 2)
                      (case-reg 2
                                5 (inc-reg 1)
                                6 (dec-reg 1)
                                0 (set-reg 1 0)))
           (memseek-up-num 1)
           (memwrite-num 1)))
    (def bf-seek-to-loop-start
      (str bf-seek-to-code
           (set-reg 1 1)
           (memseek-up-num 1)
           (memwrite-num 0)
           (memseek-down-num 1)
           (while-reg 1
                      (memseek-down-num 6)
                      (memread 2)
                      (case-reg 2
                                6 (inc-reg 1)
                                5 (dec-reg 1)
                                0 (set-reg 1 0)))
           (memseek-up-num 1)
           (memwrite-num 1)))
    "")

; Execute the code!
memseek-zero
(memread 0)

(while-reg 0
           bf-seek-to-tape
           (case-reg 0
                     1 (str (memread 1)
                            (dec-reg 1)
                            (memwrite 1))
                     2 (str (memread 1)
                            (inc-reg 1)
                            (memwrite 1))
                     3 (str (memseek-up-num 1)
                            (memwrite-num 0)
                            ; NOTE: I overshoot so that "<" at the first cell works.
                            (memseek-down-num 11)
                            (memseek-up-num 5)
                            (memwrite-num 1))
                     4 (str (memseek-up-num 1)
                            (memwrite-num 0)
                            (memseek-up-num 6)
                            (memwrite-num 1))
                     5 (if-not-bf (memread)
                                  bf-seek-to-loop-end)
                     6 (if-bf (memread)
                              bf-seek-to-loop-start)
                     7 (print-bf (memread))
                     8 (str bf-seek-to-input
                            (memread 1)
                            (memseek-up-num 1)
                            (memwrite-num 0)
                            (memseek-up-num 6)
                            (memwrite-num 1)
                            bf-seek-to-tape
                            (memwrite 1)))
           bf-seek-to-code
           (memseek-up-num 1)
           (memwrite-num 0)
           (memseek-up-num 6)
           (memwrite-num 1)
           (memseek-down-num 1)
           (memread 0))
