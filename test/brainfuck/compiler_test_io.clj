(ns brainfuck.compiler-test)

(deftest read-char-test
  (testing "read-char"
    (let [program (str initialize-state
                       (set-reg 1 13)
                       (read-char)
                       (read-char 0)
                       (read-char 1)
                       (read-char 2)
                       (read-char 3))
          expected (-> initial-state
                       (state-set-reg return-value-reg 0x64)
                       (state-set-reg 0 0x69)
                       (state-set-reg 1 0x78)
                       (state-set-reg 2 0x21))
          actual (:memory (run-machine program "dix!"))]
      (is (states-equal actual expected)))))
