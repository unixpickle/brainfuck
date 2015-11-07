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
          actual (map :memory (map #(run-machine % "") programs))]
      (are-states-equal actual expected))))
