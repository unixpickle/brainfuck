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

(defn- reg-to-scratch
  "Copy the current register to a scratch byte.
   Since copying a value requires two destinations, this uses two scratch bytes.
   The first scratch byte is the backup, the second is left at zero.
   Both scratch bytes must start at zero before the call."
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
