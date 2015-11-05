(ns brainfuck.compiler)

; This library generates code which operates on a fancy state.
; The state begins with several scratch bytes, followed by an array of registers
; and then by an infinite array of memory cells.
;
; Each register is of the form (v, 256-v, 1, 2).
; This form makes it possible to increment/decrement registers portably.
; Registers provide wrapping, 8-bit value stores.
;
; Each memory cell is of the form (stack-val, end-flag, heap-val, deref-flag).
; As you can see, a memory cell can contain both heap and stack data.
; The first memory cell is not used for storage but rather for seeking.
; The first cell is special in that it's end-flag is 2, not 1 or 0.

(def scratch-size 5)
(def reg-count 10)

(defn- deep-str
  [& x]
  (apply str (map (partial apply str) x)))

(def initialize-state
  "Initialize the state of the machine.
   This code should prefix all generated programs."
  (deep-str (repeat (dec scratch-size) ">")
            (repeat 16 "+") "[->" (repeat 16 "+") "<]>"
            "[->+" (repeat (dec reg-count) ">>>>+")
            (repeat (dec reg-count) "<<<<") "<]"
            ">" (repeat reg-count ">>>>") "++<"))

(def- reg-inc
  "Increment the current register."
  (str "+>-[>-]"            ; inc v, dec 256-v, set flag to 0 if 256-v isn't 0
       ">--[++>--]<"        ; seek to the flag
       "[-<<[->+<]>>]+<<")) ; if the flag is set, set v to 0 and 256-v to 256

(def- reg-dec
  "Decrement the current register."
  (str "[>>-]"              ; set flag to 0 if v=0
       ">--[++>>--]++<"     ; seek to the flag
       "[-<[-<+>]>]+<"      ; if flag=1, set v to 256 and 256-v to 0
       "+<-"))              ; inc 256-v, dec v

