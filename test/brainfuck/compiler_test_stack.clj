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

(deftest pop-stack-test
  (testing "pop-stack"
    (let [program (deep-str initialize-state
                            (set-reg 0 100)
                            (set-reg 1 17)
                            (push-stack 0)
                            (push-stack 1)
                            (pop-stack 2)
                            (pop-stack 3)
                            (set-reg 4 20)
                            (pop-stack 4))
          expected (-> initial-state
                       (state-set-reg 0 100)
                       (state-set-reg 1 17)
                       (state-set-reg 2 17)
                       (state-set-reg 3 100))
          result (run-machine program "")
          actual (:memory result)
          pointer (:pointer result)]
      (is (= pointer (+ 4 scratch-size (* 4 (inc reg-count)))))
      (is (states-equal actual expected)))))
