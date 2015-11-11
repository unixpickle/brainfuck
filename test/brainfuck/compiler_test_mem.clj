(ns brainfuck.compiler-test)

(deftest memseek-up-test
  (testing "memseek-up"
    (let [amounts (concat (range 0 10) (range 10 -1 -1))
          programs (reduce-results #(str %1 (set-reg 2 %2) (memseek-up 2))
                                   initialize-state
                                   amounts)
          expected (reduce-results #(-> %1
                                        (state-set-reg 2 %2)
                                        (state-memseek-up %2))
                                   initial-state
                                   amounts)
          results (map #(run-machine % "") programs)
          actual (map :memory results)
          pointers (map :pointer results)
          exp-pointer (+ 4 scratch-size (* 4 reg-count))]
      (is (every? #(= % exp-pointer) pointers))
      (are-states-equal actual expected))))

(deftest memseek-up-num-test
  (testing "memseek-up-num"
    (let [actual (compounded-machines initialize-state
                                      (memseek-up-num 1)
                                      (memseek-up-num 2)
                                      memseek-zero
                                      (memseek-up-num 0)
                                      (memseek-up-num 5))
          expected (compounded-states initial-state
                                      (state-memseek-up 1)
                                      (state-memseek-up 2)
                                      (state-memseek-zero)
                                      (state-memseek-up 0)
                                      (state-memseek-up 5))]
      (are-states-equal actual expected))))

(deftest memseek-down-test
  (testing "memseek-down"
    (let [init (str initialize-state (set-reg 1 5) (memseek-up 1))
          programs (map #(str init (set-reg 2 %) (memseek-down 2))
                        (range 10))
          expected (map #(-> initial-state
                             (state-set-reg 1 5)
                             (state-set-reg 2 %)
                             (state-memseek-abs (- 5 %)))
                        (range 10))
          results (map #(run-machine % "") programs)
          actual (map :memory results)
          pointers (map :pointer results)
          exp-pointer (+ 4 scratch-size (* 4 reg-count))]
      (is (every? #(= % exp-pointer) pointers))
      (are-states-equal actual expected))))

(deftest memseek-down-num-test
  (testing "memseek-down-num-test"
    (let [actual (compounded-machines initialize-state
                                      (memseek-up-num 1)
                                      (memseek-down-num 2)
                                      (memseek-down-num 1)
                                      (memseek-down-num 10)
                                      (memseek-up-num 10)
                                      (memseek-down-num 0)
                                      (memseek-down-num 1)
                                      (memseek-down-num 4)
                                      (memseek-down-num 5))
          expected (compounded-states initial-state
                                      (state-memseek-up 1)
                                      (state-memseek-zero)
                                      (state-memseek-zero)
                                      (state-memseek-zero)
                                      (state-memseek-up 10)
                                      (state-memseek-up 0)
                                      (state-memseek-abs 9)
                                      (state-memseek-abs 5)
                                      (state-memseek-zero))]
      (are-states-equal actual expected))))

(deftest memseek-zero-test
  (testing "memseek-zero"
    (let [program (str initialize-state (set-reg 1 5) (memseek-up 1) memseek-zero)
          expected (state-set-reg initial-state 1 5)
          result (run-machine program "")
          actual (:memory result)
          pointer (:pointer result)
          exp-pointer (+ 4 scratch-size (* 4 (+ 6 reg-count)))]
      (is (= pointer exp-pointer))
      (is (states-equal actual expected)))))

(deftest memwrite-test
  (testing "memwrite"
    (let [program (str initialize-state
                       (set-reg 1 3)
                       (memseek-up 1)
                       (memwrite 1)
                       memseek-zero
                       (set-reg 1 17)
                       (memwrite 1)
                       (memseek-up 1)
                       (set-reg 2 0)
                       (memwrite 2)
                       (set-reg 2 2)
                       (memseek-down 2)
                       (memwrite 2)
                       (set-reg 2 3)
                       (memwrite 2))
          expected (-> initial-state
                       (state-memseek-up 3)
                       (state-memwrite 3)
                       (state-memseek-abs 0)
                       (state-memwrite 17)
                       (state-memseek-up 15)
                       (state-memwrite 3)
                       (state-set-reg 2 3)
                       (state-set-reg 1 17))
          result (run-machine program "")
          actual (:memory result)
          pointer (:pointer result)
          exp-pointer (+ 4 scratch-size (* 4 reg-count))]
      (is (= pointer exp-pointer))
      (is (states-equal actual expected)))))

(deftest memread-test
  (testing "memread"
    (let [program (str initialize-state
                       (set-reg 1 17)
                       (memseek-up 1)
                       (memwrite 1)
                       (memread 0)
                       (memread 0)
                       (memread 2)
                       (memseek-up 0)
                       (memwrite 0)
                       (memseek-down 0))
          expected (-> initial-state
                       (state-memseek-up 17)
                       (state-memwrite 17)
                       (state-set-reg 0 17)
                       (state-set-reg 1 17)
                       (state-set-reg 2 17)
                       (state-memseek-up 17)
                       (state-memwrite 17)
                       (state-memseek-abs 17))
          result (run-machine program "")
          actual (:memory result)
          pointer (:pointer result)
          exp-pointer (+ 4 scratch-size (* 4 reg-count))]
      (is (= pointer exp-pointer))
      (is (states-equal actual expected)))))
