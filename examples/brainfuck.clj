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
(memseek-up-bf (return-num 1))
(memwrite-bf (return-num 1))
(memseek-up-bf (return-num 2))
(memwrite-bf (return-num 1))
(memseek-up-bf (return-num 2))
(memwrite-bf (return-num 1))
memseek-zero

; Read the code input.
(read-char 0)
(while-reg 0
           (set-reg 1 0)
           (case-reg 0
                     (int \+) (set-reg 1 2)
                     (int \<) (set-reg 1 3)
                     (int \>) (set-reg 1 4)
                     (int \[) (set-reg 1 5)
                     (int \]) (set-reg 1 6)
                     (int \.) (set-reg 1 7)
                     (int \,) (set-reg 1 8))
           (if-bf (equal-bf (return-reg 0) (return-num (int \!)))
                  (set-reg 0 0)
                  (str (read-char 0)
                       (if-bf (return-reg 1)
                              (str (memwrite 1)
                                   (memseek-up-bf (return-num 6)))))))

; Read the standard input.
memseek-zero
(read-char 0)
(while-reg 0
           (memwrite 0)
           (memseek-up-bf (return-num 6)))

(do (defn bf-seek-marker
       [idx]
       (str memseek-zero
            (memseek-up-bf (return-num idx))
            (while-bf (memread)
                      (memseek-up-bf 6))
            (memseek-down-bf (return-num 1))))
    (def bf-seek-to-code (bf-seek-marker 1))
    (def bf-seek-to-input (bf-seek-marker 3))
    (def bf-seek-to-tape (bf-seek-marker 5))
    "")

; Execute the code!
memseek-zero
(memread 0)

(while-reg 0
           (case-reg 0
                     1 (str bf-seek-to-tape
                            (memread 1)
                            (dec-reg 1)
                            (memwrite 1))
                     2 (str bf-seek-to-tape
                            (memread 1)
                            (inc-reg 1)
                            (memwrite 1))
                     3 (str bf-seek-to-tape
                            (memseek-up-bf (return-num 1))
                            (memwrite-bf (return-num 0))
                            ; NOTE: I overshoot so that "<" at the first cell works.
                            (memseek-down-bf (return-num 11))
                            (memseek-up-bf (return-num 5))
                            (memwrite-bf (return-num 1)))
                     4 (str bf-seek-to-tape
                            (memseek-up-bf (return-num 1))
                            (memwrite-bf (return-num 0))
                            (memseek-up-bf (return-num 6))
                            (memwrite-bf (return-num 1)))
                     ; TODO: implement [ and ] here.
                     5 (print-bf (str bf-seek-to-tape
                                      (memread)))
                     6 (str bf-seek-to-input
                            (memread 1)
                            bf-seek-to-tape
                            (memwrite 1)))
           bf-seek-to-code
           (memseek-up-bf (return-num 1))
           (memwrite-bf (return-num 0))
           (memseek-up-bf (return-num 6))
           (memwrite-bf (return-num 1))
           (memseek-down-bf (return-num 1))
           (memread 0))
