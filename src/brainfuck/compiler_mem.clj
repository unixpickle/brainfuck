(ns brainfuck.compiler)

(defn memseek-up
  "Move the memory read/write head up by a dynamic number of bytes."
  [reg]
  (str (seek-mem-to-reg reg)
       (reg-to-scratch (dec scratch-size) (- scratch-size 2))
       "[-" seek-reg-to-mem ">>>>>>>[>>>>]+<<<" (seek-mem-to-reg reg) "]"
       (scratch-to-reg (dec scratch-size) false)
       seek-reg-to-mem))

(defn memseek-up-num
  "Move the memory read/write head up by a pre-programmed number of bytes.
   The supplied number of bytes (n) should be relatively small, since the
   size of the output code is O(n)."
  [n]
  (deep-str ">--[++<<<<--]++"
            ">>>>>>[>>>>]"
            (repeat n "+>>>>")
            ">"))

(defn memseek-up-bf
  "Like memseek-up, but uses the return value of some code."
  [code]
  (str code (memseek-up return-value-reg)))

(defn memseek-down
  "Move the memory read/write head down by a dynamic number of bytes.
   The seek will not go past the beginning of memory."
  [reg]
  (str (seek-mem-to-reg reg)
       (reg-to-scratch (dec scratch-size) (- scratch-size 2))
       "[-" seek-reg-to-mem ">>>>>>>>>>>[>>>>]<<<<[-]<<<" (seek-mem-to-reg reg) "]"
       (scratch-to-reg (dec scratch-size) false)
       seek-reg-to-mem))

(defn memseek-down-num
  "Move the memory read/write head down by a pre-programmed number of bytes.
   Just like for memseek-up-num, the size of the generated code is O(n)
   where n is the number of bytes to seek."
  [n]
  (deep-str ">--[++<<<<--]++>>>>>>"
            (repeat n ">>>>")
            "[>>>>]"
            (repeat n "<<<<[-]")
            ">"))

(defn memseek-down-bf
  "Like memseek-down, but uses the return value of some code."
  [code]
  (str code (memseek-down return-value-reg)))

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
  (str seek-mem-to-head ">>[-]<<"
       (seek-mem-to-reg reg)
       (reg-to-scratch (dec scratch-size) (- scratch-size 2))
       "[-" seek-reg-to-head ">>+<<" (seek-mem-to-reg reg) "]"
       (scratch-to-reg (dec scratch-size) false)
       seek-reg-to-mem))

(defn- add-square-mem
  "Add the square of an integer to the current memory cell."
  [n]
  (deep-str ">" (repeat n "+")
            "[-<" (repeat n "+") ">]<"))

(defn- short-add-code-mem
  "Add an integer to the current memory cell."
  [n]
  (if (zero? n)
      ""
      (let [naive-code (deep-str (repeat n "+"))
            sqrt (int (Math/sqrt n))
            code (str (add-square-mem sqrt)
                      (short-add-code-mem (- n (* sqrt sqrt))))
            trimmed-code (remove-seek-redundancies code)]
        (first (sort-by count [naive-code trimmed-code])))))

(defn memwrite-num
  "Write a pre-programmed value into the current memory cell."
  [n]
  (str seek-mem-to-head
       ">>[-]"
       (short-add-code-mem n)
       "<<"))

(defn memwrite-bf
  "Run some code and write it's return value to memory."
  [code]
  (str code (memwrite return-value-reg)))

(defn- memtransfer
  "Read the current memory cell into a register while simultaneously zeroing
   the memory cell."
  [reg]
  (str (with-reg reg reset-current-reg)
       seek-mem-to-head ">>[-<<" (seek-mem-to-reg reg) ">-<+" seek-reg-to-head ">>]<<"))

(defn memread
  "Read the current memory cell into a register.
   If no register is specified, it uses the return value register"
  ([] (memread return-value-reg))
  ([reg] (str (memtransfer reg) (memwrite reg))))
