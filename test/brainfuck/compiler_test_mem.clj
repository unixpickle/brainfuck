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
                       (set-reg 2 1)
                       (memseek-down 2)
                       (memwrite 2))
          expected (-> initial-state
                       (state-memseek-up 3)
                       (state-memwrite 3)
                       (state-memseek-abs 0)
                       (state-memwrite 17)
                       (state-memseek-up 15)
                       (state-memwrite 1)
                       (state-set-reg 2 1)
                       (state-set-reg 1 17))
          result (run-machine program "")
          actual (:memory result)
          pointer (:pointer result)
          exp-pointer (+ 4 scratch-size (* 4 reg-count))]
      (is (= pointer exp-pointer))
      (states-equal actual expected))))
