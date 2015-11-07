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

(load "compiler_util" "compiler_regs" "compiler_stack" "compiler_mem" "compiler_flow"
      "compiler_math")

(def initialize-state
  "Initialize the state of the machine.
   This code should prefix all generated programs."
  (deep-str (repeat (+ 4 (dec scratch-size)) ">")
            (repeat 16 "+") "[->" (repeat 16 "+") "<]>"
            "[->+" (repeat (dec reg-count) ">>>>+")
            (repeat (dec reg-count) "<<<<") "<]"
            ">>" (repeat reg-count "+>++>>>") "<++<"))
