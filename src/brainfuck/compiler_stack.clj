(ns brainfuck.compiler)

(defn push-stack
  "Create a new entry on the stack and copy a register's value to it."
  [reg]
  (str (seek-mem-to-reg reg)
       (reg-to-scratch (dec scratch-size) (- scratch-size 2))
       "[-" seek-reg-to-mem ">[>>>>]<+" (seek-mem-to-reg reg) "]"
       (scratch-to-reg (dec scratch-size) false)
       seek-reg-to-mem ">[>>>>]+<"))

(def ^:private seek-last-stack-entry
  "Seek from the beginning of memory to the last entry of the stack.
   If the stack has no entries, this goes to the first usable memory cell."
  ">>>>>>>>>[>>>>]<<<<<")

(defn pop-stack
  "Remove the last entry from the stack and, optionally, copy its value into a register."
  ([] (str ">--[++<<<<--]++<" seek-last-stack-entry ">[-]<[-]"))
  ([reg]
   (str (with-reg reg reset-current-reg)
        seek-last-stack-entry
        ">[-]<"
        "[-" (seek-mem-to-reg reg) ">-<+" seek-reg-to-mem ">[>>>>]<]")))

(def ^:private pop-no-change
  "Emulate a pop without actually removing the value from the stack.
   This assumes that the tape is currently at the first memory cell.
   This code will leave the tape at the first memory cell."
  (str seek-last-stack-entry ">[-]--[++<<<<--]++<"))

(def ^:private push-no-change
  "Emulate a push without actually setting a value on the stack.
   This assumes that the tape is currently at the first memory cell.
   This code will leave the tape at the first memory cell."
  ">>>>>[>>>>]-[++<<<<--]++<")

(defn peak-stack
  "Look at an element near the end of the stack without modifying it.
   The offset you give specifies how far back on the stack to look.
   An offset of 0 represents at the last stack element.
   If the offset is greater than or equal to the number of elements on the stack,
   this gives the first stack value or 0.
   If no offset is specified, a 0 offset is used."
  ([reg] (peak-stack reg 0))
  ([reg offset]
   (deep-str (with-reg reg reset-current-reg)
             (repeat offset pop-no-change)
             seek-last-stack-entry
             "[-" (seek-mem-to-reg reg) ">-<+" seek-reg-to-mem seek-last-stack-entry "]"
             (seek-mem-to-reg reg)
             (reg-to-scratch (dec scratch-size) (- scratch-size 2))
             "[-" seek-reg-to-mem seek-last-stack-entry "+" (seek-mem-to-reg reg) "]"
             (scratch-to-reg (dec scratch-size) false)
             seek-reg-to-mem
             (repeat offset push-no-change))))
