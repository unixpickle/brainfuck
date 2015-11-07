(ns brainfuck.compiler-test)

(deftest add-regs-test
  (testing "add-regs"
    (let [program (str initialize-state
                       (set-reg 1 17)
                       (set-reg 2 49)
                       (add-regs 1 2 0))
          expected (-> initial-state
                       (state-set-reg 1 17)
                       (state-set-reg 2 49)
                       (state-set-reg 0 66))
          actual (:memory (run-machine program ""))]
      (is (states-equal actual expected)))))
