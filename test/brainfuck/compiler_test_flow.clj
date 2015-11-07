(ns brainfuck.compiler-test)

(deftest while-bf-test
  (testing "while-bf"
    (let [program (str initialize-state
                       (set-reg return-value-reg 17)
                       (while-bf (dec-reg return-value-reg)
                                 (inc-reg 1)
                                 (inc-reg 1)
                                 (inc-reg 1)
                                 (inc-reg 1))
                       (set-reg return-value-reg 3)
                       (while-bf ""
                                 (inc-reg 2)
                                 (inc-reg 2)
                                 (dec-reg return-value-reg)))
          expected (-> initial-state
                       (state-set-reg 1 64)
                       (state-set-reg 2 6))
          actual (:memory (run-machine program ""))]
      (is (states-equal actual expected)))))
