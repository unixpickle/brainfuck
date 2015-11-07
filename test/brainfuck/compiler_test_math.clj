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

(deftest div-regs-test
  (testing "div-regs"
    (let [numerators [50 38 32 1 0 0 255 255]
          denominators [7 2 17 10 5 0 16 0]
          quotients (map #(if (zero? %2) 0 (int (/ %1 %2))) numerators denominators)
          moduli (map #(if (zero? %2) 0 (mod %1 %2)) numerators denominators)
          programs (map #(str initialize-state
                              (set-reg 0 %1)
                              (set-reg 1 %2)
                              (div-regs 0 1 2 3)
                              (return-num 0))
                        numerators denominators)
          expected (map #(-> initial-state
                             (state-set-reg 0 %1)
                             (state-set-reg 1 %2)
                             (state-set-reg 2 %3)
                             (state-set-reg 3 %4))
                        numerators denominators quotients moduli)
          actual (map :memory (map #(run-machine % "") programs))]
      (are-states-equal actual expected))))
