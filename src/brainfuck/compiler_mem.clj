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
       (reg-to-scratch (dec scratch-size) (- scratch-size 1))
       "[-" seek-reg-to-mem ">>>>>>>>>>>[>>>>]<<<<[-]<<<" (seek-mem-to-reg reg) "]"
       (scratch-to-reg (dec scratch-size) false)
       seek-reg-to-mem))

(def memseek-zero
  "Move the memory read/write head to the beginning of memory."
  ">--[++<<<<--]++>>>>>>[->>>>]<<<")
