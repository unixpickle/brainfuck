(ns brainfuck.compiler-test)

(deftest push-stack-test
  (testing "push-stack"
    (let [program (deep-str initialize-state
                            (set-reg 0 100)
                            (set-reg 1 17)
                            (set-reg 2 20)
                            (map push-stack [0 1 2 1 0]))
          reg-state (-> initial-state
                        (state-set-reg 0 100)
                        (state-set-reg 1 17)
                        (state-set-reg 2 20))
          expected (reduce state-push-stack reg-state [100 17 20 17 100])
          result (run-machine program "")
          actual (:memory result)
          pointer (:pointer result)]
        (is (= pointer (+ 4 scratch-size (* 4 (+ 5 reg-count)))))
        (is (states-equal actual expected)))))
