(ns brainfuck.compiler)

(defn push-stack
  "Create a new entry on the stack and copy a register's value to it."
  [reg]
  (str (seek-mem-to-reg reg)
       "[->+<" seek-reg-to-mem ">[>>>>]<+" (seek-mem-to-reg reg) "]"
       seek-reg-to-mem ">[>>>>]+<"))

(def ^:private seek-last-stack-entry
  "Seek from the beginning of memory to the last entry of the stack.
   If the stack has no entries, this goes to the first usable memory cell."
  ">>>>>>>>>[>>>>]<<<<<")

(defn pop-stack
  "Remove the last entry from the stack and copy its value into a register."
  [reg]
  (str (with-reg reg reg-reset)
       seek-last-stack-entry
       ">[-]<"
       "[-" (seek-mem-to-reg reg) ">-<+" seek-reg-to-mem ">[>>>>]<]"))
