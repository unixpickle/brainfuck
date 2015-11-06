(ns brainfuck.compiler)

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

(def ^:private reg-reset
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
   This will leave the tape pointed at the target register."
  [s]
  (let [go-scratch-end "<<[<<<<]<<"
        go-s (str go-scratch-end (seek scratch-size s))
        go-reg-flag (str (seek s (+ scratch-size 6)) "[>>>>]")
        go-reg (str go-reg-flag "<<")]
    (str ">>-<<" go-s "[-" go-reg "+>-<" go-s "]" go-reg-flag "+<<")))

(defn- seek-mem-to-reg
  "Move from the memory cell portion of the tap to a given register."
  [r]
  (deep-str ">--[++<--]++<" (repeat (- reg-count r) "<<<<")))

(def ^:private seek-reg-to-mem
  "Move from the current register to the first memory cell."
  ">>>[>>>>]<<<")

(defn- with-reg
  "Seek from memory to a register, run some code, and then seek back to memory.
   Your code must leave the tape seeked to some arbitrary register."
  [r & code]
  (str (seek-mem-to-reg r) (deep-str code) seek-reg-to-mem))

(defn- seek-between-regs
  "Seek from one register to another"
  [start end]
  (seek (* 4 start) (* 4 end)))

(defn mov-reg
  "Copy the value of the first register to the second register."
  [source destination]
  (with-reg source
            (reg-to-scratch (dec scratch-size) (- scratch-size 2))
            (seek-between-regs source destination)
            reg-reset
            (scratch-to-reg (dec scratch-size))))

(defn set-reg
  "Set the value of a register to a hard-coded number."
  [reg val]
  ; TODO: shrink the code that this generates.
  (with-reg reg reg-reset (repeat val "+")))
