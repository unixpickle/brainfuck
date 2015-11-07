(ns brainfuck.compiler)

(defn print-reg
  "Print the character stored in the given register."
  [reg]
  (with-reg reg "."))

(defn print-bf
  "Print the return value of the given code."
  [code]
  (str code (print-reg return-value-reg)))

(defn print-string
  "Print a pre-programmed string without affecting any registers."
  [s]
  (deep-str (push-stack return-value-reg)
            (map (comp print-bf return-num int) s)
            (pop-stack return-value-reg)))

(defn read-char
  "Input a character from the console.
   If no destination register is given, this sets the return value register.
   If EOF was reached, the register will be set to 0."
  ([] (read-char return-value-reg))
  ([reg]
   (let [go-scratch "<<[<<<<]<<<"
         go-reg-flag ">>>>>>>[>>>>]"
         go-reg (str go-reg-flag "<<")]
     (with-reg reg
               reset-current-reg
               ">>-<<"
               go-scratch
               ",+[-[-" go-reg ">-<+" go-scratch "]]"
               go-reg-flag "+<<"))))
