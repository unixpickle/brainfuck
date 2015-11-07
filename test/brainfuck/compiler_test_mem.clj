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
