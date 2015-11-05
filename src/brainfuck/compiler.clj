(ns brainfuck.compiler)

; This library generates code which operates on a fancy state.
; The state begins with several scratch bytes, followed by an array of registers
; and then by an infinite array of memory cells.
;
; Each register is of the form (v=0, comp=256, flag=1, 2).
; This form makes it possible to increment/decrement registers portably.
; Registers provide wrapping, 8-bit value stores.
;
; Each memory cell is of the form (stack-val, end-flag, heap-val, deref-flag).
; As you can see, a memory cell can contain both heap and stack data.
;
; The full memory layout is as follows:
; <scratch_bytes...,0,0,0,0, regs...,0,2,0,0,memory...>

(def scratch-size 5)
(def reg-count 10)

(defn- deep-str
  [& x]
  (apply str (map (partial apply str) x)))

(def initialize-state
  "Initialize the state of the machine.
   This code should prefix all generated programs."
  (deep-str (repeat (+ 4 (dec scratch-size)) ">")
            (repeat 16 "+") "[->" (repeat 16 "+") "<]>"
            "[->+" (repeat (dec reg-count) ">>>>+")
            (repeat (dec reg-count) "<<<<") "<]"
            ">>" (repeat reg-count "+>++>>>") "<++<"))

(def ^:private reg-inc
  "Increment the current register."
  (str "+>-[>-]"            ; inc v, dec comp, set flag to 0 if comp isn't 0
       ">--[++>--]++<"      ; seek to flag
       "[-<<[->+<]>>]+<<")) ; if flag!=0, set v to 0 and comp to 256

(def ^:private reg-dec
  "Decrement the current register."
  (str "[>>-]"              ; set flag to 0 if v=0
       ">--[++>>--]++<"     ; seek to the flag
       "[-<[-<+>]>]+<"      ; if flag=1, set v to 256 and comp to 0
       "+<-"))              ; inc comp, dec v

(defn- seek
  "Generate a string of <'s or >'s."
  [start end]
  (let [x (- end start)]
    (deep-str (repeat x ">") (repeat (- x) "<"))))

(defn- backup-reg
  "Backup the current register to a scratch byte.
   Since copying a value requires two destinations, this uses two scratch bytes.
   The first scratch byte is the backup, the second is left at zero."
  [s0 s1]
  (let [go-scratch-end "<<[<<<<]<<"
        go-s1 (str go-scratch-end (seek scratch-size s1))
        go-reg (str (seek s1 (+ scratch-size 6)) "[>>>>]<<")]
    (str ">>-<<"
         "[-" go-scratch-end
         (seek scratch-size s0) "+"
         (seek s0 s1) "+"
         go-reg "]"
         go-s1 "[-" go-reg "+" go-s1 "]"
         go-reg ">>+<<")))
