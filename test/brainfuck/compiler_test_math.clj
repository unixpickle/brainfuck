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

(deftest add-bf-test
  (testing "add-bf"
    (let [program (str initialize-state
                       (add-bf (return-num 17) (return-num 49) (return-num 69))
                       (mov-reg 0 return-value-reg)
                       (add-bf (return-num 250) (return-num 10))
                       (mov-reg 1 return-value-reg)
                       (add-bf (return-num 8))
                       (set-reg scratch-reg-1 0)
                       (set-reg scratch-reg-2 0))
          expected (-> initial-state
                       (state-set-reg 0 135)
                       (state-set-reg 1 4)
                       (state-set-reg return-value-reg 8))
          actual (:memory (run-machine program ""))]
      (is (states-equal actual expected)))))
