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

(deftest equal-bf-test
  (testing "equal-bf"
    (let [nums [[10] [10 10] [15 10] [10 15] [10 10 20] [10 5 5] [129 129 129] [255 255 255]
                [1 1 1 1 1 2] [1 1 1 1] [0 0] [0] [] [0 0 0 0] [0 0 1 0]]
          equal-push (map #(str (apply equal-bf (map return-num %))
                                (push-stack return-value-reg))
                          nums)
          program (deep-str initialize-state
                            equal-push
                            (set-reg scratch-reg-1 0)
                            (set-reg scratch-reg-2 0)
                            (set-reg return-value-reg 0))
          true-vals (map #(if (zero? (count %)) 1 (if (apply = %) 1 0)) nums)
          expected (reduce #(state-push-stack %1 %2) initial-state true-vals)
          actual (:memory (run-machine program ""))]
      (is (states-equal actual expected)))))

(deftest less-than-bf-test
  (testing "less-than-bf"
    (let [lhs [10 0 5 16 20 255 255 255 254 0]
          rhs [5 5 5 200 0 0 255 254 255 1]
          check-push (map #(str (less-than-bf (return-num %1) (return-num %2))
                                (push-stack return-value-reg))
                          lhs rhs)
          program (deep-str initialize-state
                            check-push
                            (set-reg scratch-reg-1 0)
                            (set-reg scratch-reg-2 0)
                            (set-reg return-value-reg 0))
          true-vals (map #(if (< %1 %2) (- %2 %1) 0) lhs rhs)
          expected (reduce #(state-push-stack %1 %2) initial-state true-vals)
          actual (:memory (run-machine program ""))]
      (is (states-equal actual expected)))))
