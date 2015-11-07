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

(deftest if-bf-test
  (testing "if-bf"
    (let [program (str initialize-state
                       (set-reg 1 3)
                       (if-bf (return-reg 1) (set-reg 2 1))
                       (if-bf (return-reg 3) (set-reg 4 13))
                       (if-bf (return-reg 3)
                              (set-reg 1 1)
                              (set-reg 1 2))
                       (dec-reg 1)
                       (if-bf (return-reg 1)
                              (set-reg 1 7)
                              (set-reg 1 9)))
          expected (-> initial-state
                       (state-set-reg 1 7)
                       (state-set-reg 2 1))
          actual (:memory (run-machine program ""))]
      (is (states-equal actual expected)))))
