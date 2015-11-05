(ns brainfuck.compiler)

; This library generates code which operates on well-defined machine states.
; The machine state starts with seven special bytes, which are followed by an array of memory units.
;
; The memory begins with <regS0, regS1, regS2, regAH, regAL, regB, 2>.
; The regAH and regAL byte together make up the A register, used for memory references.
; The regB byte holds the value of the B register.
; The regS* bytes are scratch registers.
; The 2 acts as a special marker for finding the first memory cell.
;
; Each memory unit is of the form <value, deref-flag> where value is the unit's byte value
; and deref-flag is 0 unless a pointer dereference is taking place.
; The first memory unit is special because it's deref-flag is 2 and its value is unused.

(def initialization-code
  "Initialize the state of the machine.
   This code should prefix all generated programs."
  ">>>>>>++<")

(def- goto-pre-cell
  "Move the cursor to two bytes before the first memory unit's value."
  ">--[++<--]++<")

(defn- goto-reg
  "Go to an indexed register, where 0 maps to regS0, 1 to regS1, etc."
  [x]
  (apply str goto-pre-cell (repeat "<" (- 5 x))))

(defn- set-reg
  "Generate code that sets a register to a pre-defined value."
  [reg val]
  (let [numGen (apply str (repeat val "+"))
        getBack (apply str (repeat ">" (- 5 x)))]
    (str (goto-reg reg) "[-]" numGen getBack)))

(def goto-unit
  "Move to the value field of the memory unit referenced by the A register.
   This code will not modify the A or B registers."
  (let [markS1 (str "[->>>>>>>[>>]+<" (goto-reg 1) "]")
        half (repeat 128 "+")]
    (str (goto-reg 0) "[-]>[-]>[-]"          ; Zero out S0, S1, and S2
         ">[-<<+<+>>>]"                      ; Move AH into S0 and S1
         "<<[->>+<<]"                        ; Move S1 into AH
         ">>>[-<<+<+>>>]"                    ; Move AL into S1 and S2
         "<<[->>+<<]"                        ; Move S2 into AL
         "<" markS1                          ; Mark the first (deref S1) deref-flags
         "<[->" half markS1 half markS1 "<]" ; Mark the next 256*(deref S0) deref-flags
         ">>>>>>>>[>>]+"                     ; Set the last deref-flag
         "<<--[+<<--]++"                     ; Go back to pre-cell and reset prior deref-flags
         ">>-[+>>-]<")))                     ; Go to the memory unit and reset its deref-flag

(def write-unit
  "Copy the value from the B register into the memory unit referenced by the A register.
   This code will not modify the A or B registers."
  (str goto-deref ">+<[-]"                    ; Setup the target unit
       (goto-reg 1) "[-]>[-]"                 ; Zero out S1 and S2
       ">>>[-<<<+<+>>>>]"                     ; Move B into S1 and S2
       "<<<<[->>>>+<<<<]"                     ; Move S1 back into B
       ">[->>>>>>-[+>>-]+<+" (goto-reg 2) "]" ; Move S2 into the target unit
       ">>>>>>-[+>>-]<"))                     ; Unset the deref-flag of the target unit

(def read-unit
  "Copy the value from the memory unit referenced by the A register into the B register.
   This code will not modify the A register or the referenced memory cell."
   (str (set-reg 2 0)                           ; Zero out S2.
        goto-deref ">+<"                        ; Set the deref-flag of the target unit to 1.
        "[-" (goto-reg 5) "+<<<+>>>>-[+>>-]+<]" ; Move target unit into B and S2
        (goto-reg 2) "[->>>>-[+>>-]+<+"
        (goto-reg 2) "]"                        ; Move S2 back into the target unit
        ">>>>>>-[+>>-]<"))                      ; Unset the deref-flag of the target unit
