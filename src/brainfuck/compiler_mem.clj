(ns brainfuck.compiler)

(defn memseek-up
  "Move the memory read/write head down by a dynamic number of bytes."
  [reg]
  (str (seek-mem-to-reg reg)
       (reg-to-scratch (dec scratch-size) (- scratch-size 2))
       "[-" seek-reg-to-mem ">>>>>>>[>>>>]+<<<" (seek-mem-to-reg reg) "]"
       (scratch-to-reg (dec scratch-size) false)
       seek-reg-to-mem))

(defn memseek-down
  "Move the memory read/write head down by a dynamic number of bytes.
   The seek will not go past the beginning of memory."
  [reg]
  (str (seek-mem-to-reg reg)
       (reg-to-scratch (dec scratch-size) (- scratch-size 2))
       "[-" seek-reg-to-mem ">>>>>>>>>>>[>>>>]<<<<[-]<<<" (seek-mem-to-reg reg) "]"
       (scratch-to-reg (dec scratch-size) false)
       seek-reg-to-mem))

(def memseek-zero
  "Move the memory read/write head to the beginning of memory."
  ">--[++<<<<--]++>>>>>>[->>>>]<<<")

(def ^:private seek-mem-to-head
  "Seek from any place in memory to the current memory cell."
  ">--[++<<<<--]++>>>>>>[>>>>]<<<")

(def ^:private seek-reg-to-head
  "Seek from a register to the current memory cell."
  (str seek-reg-to-mem ">>>>>>>[>>>>]<<<"))

(defn memwrite
  "Write a register to the current memory cell."
  [reg]
  (str seek-mem-to-head ">>>[-]<<<"
       (seek-mem-to-reg reg)
       (reg-to-scratch (dec scratch-size) (- scratch-size 2))
       "[-" seek-reg-to-head ">>>+<<<" (seek-mem-to-reg reg) "]"
       (scratch-to-reg (dec scratch-size) false)
       seek-reg-to-mem))
