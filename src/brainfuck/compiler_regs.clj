(ns brainfuck.compiler
  (:require [clojure.string]))

(def ^:private inc-current-reg
  "Increment the current register."
  (str "+>-[>-]"            ; inc v, dec comp, set flag to 0 if comp isn't 0
       ">--[++>--]++<"      ; seek to flag
       "[-<<[->+<]>>]+<<")) ; if flag!=0, set v to 0 and comp to 256

(def ^:private dec-current-reg
  "Decrement the current register."
  (str "[>>-]"              ; set flag to 0 if v=0
       ">--[++>>--]++<"     ; seek to the flag
       "[-<[-<+>]>]+<"      ; if flag=1, set v to 256 and comp to 0
       "+<-"))              ; inc comp, dec v

(def ^:private reset-current-reg
  "Zero out the current register."
  "[->+<]")

(defn- reg-to-scratch
  "Copy the current register to a scratch byte.
   Since copying a value requires two destinations, this uses two scratch bytes.
   The first scratch byte is the backup, the second is left at zero.
   Both scratch bytes must start at zero before the call.
   This will leave the tape pointed at the source register."
  [s0 s1]
  (let [go-scratch-end "<<[<<<<]<<"
        go-s1 (str go-scratch-end (seek scratch-size s1))
        go-reg-flag (str (seek s1 (+ scratch-size 6)) "[>>>>]")
        go-reg (str go-reg-flag "<<")]
    (str ">>-<<"
         "[-" go-scratch-end
         (seek scratch-size s0) "+"
         (seek s0 s1) "+"
         go-reg "]"
         go-s1 "[-" go-reg "+" go-s1 "]"
         go-reg-flag "+<<")))

(defn- scratch-to-reg
  "Copy a scratch byte into the current register.
   The current register must be set to zero before this call.
   This will leave the tape pointed at the target register.
   If the second (optional) argument is false, this will not affect the
   complement of the register."
  ([s] (scratch-to-reg s true))
  ([s do-sub]
   (let [go-scratch-end "<<[<<<<]<<"
         go-s (str go-scratch-end (seek scratch-size s))
         go-reg-flag (str (seek s (+ scratch-size 6)) "[>>>>]")
         go-reg (str go-reg-flag "<<")
         add1 (if do-sub "+>-<" "+")]
    (str ">>-<<" go-s "[-" go-reg add1 go-s "]" go-reg-flag "+<<"))))

(defn- seek-mem-to-reg
  "Move from the memory cell portion of the tape to a given register."
  [r]
  (deep-str ">--[++<<<<--]++<" (repeat (- reg-count r) "<<<<")))

(def ^:private seek-reg-to-mem
  "Move from the current register to the first memory cell."
  ">>[>>>>]<<")

(defn- with-reg
  "Seek from memory to a register, run some code, and then seek back
   to the first cell of memory.
   Your code must leave the tape pointed to any arbitrary register."
  [r & code]
  (str (seek-mem-to-reg r) (deep-str code) seek-reg-to-mem))

(defn- seek-between-regs
  "Seek from one register to another"
  [start end]
  (seek (* 4 start) (* 4 end)))

(defn mov-reg
  "Copy the value of the second register to the first register."
  [destination source]
  (if (< destination source)
      (with-reg source
                ">>->--<<<"
                (seek-between-regs source destination)
                reset-current-reg
                ">>>-"
                "[>>>>]<<<"
                "[->>+>-[+<<<<-]+<<-<+>>>[>>>>]<<<]"
                ">>[-<<+>>]+>+[+<<<<-]++>")
      (with-reg destination
                reset-current-reg
                ">>>--<<<"
                (seek-between-regs destination source)
                ">>->-<<<"
                "[->>+>[>>>>]<<-<+<-[+<<<<-]+<<<]"
                ">>[-<<+>>]+>+[>>>>]++<<<")))

(defn- add-product-reg
  "Adds the product of two integers the current register.
   This assumes that the register will not overflow."
  [big small]
  (deep-str ">>" (repeat (- big 1) "+")
            "[-<<" (repeat small "+") ">" (repeat small "-") ">]+<<"))

(defn- short-add-code
  "Add an integer to the current register.
   This assumes that the register will not overflow."
  [n]
  (if (zero? n)
      (deep-str ">" (repeat n "-") "<" (repeat n "+"))
      (let [naive-code (deep-str ">" (repeat n "-") "<" (repeat n "+"))
            big-factor (int (Math/sqrt (* 2 n)))
            small-factor (int (/ n big-factor))
            code (str (add-product-reg big-factor small-factor)
                      (short-add-code (- n (* big-factor small-factor))))
            trimmed-code (clojure.string/replace code #"(<>|<<>>)" "")]
        (first (sort-by count [naive-code trimmed-code])))))

(defn set-reg
  "Set the value of a register to a hard-coded number."
  [reg val]
  (with-reg reg reset-current-reg (short-add-code (mod val 256))))
