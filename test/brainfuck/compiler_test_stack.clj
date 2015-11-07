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
                            (pop-stack 4)
                            (push-stack 0)
                            (push-stack 0)
                            (pop-stack)
                            (pop-stack)
                            (push-stack 0)
                            (push-stack 0)
                            (pop-stack))
          expected (-> initial-state
                       (state-set-reg 0 100)
                       (state-set-reg 1 17)
                       (state-set-reg 2 17)
                       (state-set-reg 3 100)
                       (state-push-stack 100))
          result (run-machine program "")
          actual (:memory result)
          pointer (:pointer result)]
      (is (= pointer (+ 4 scratch-size (* 4 (+ 2 reg-count)))))
      (is (states-equal actual expected)))))

(deftest peak-stack-test
  (testing "peak-stack"
    (let [program (deep-str initialize-state
                            (set-reg 0 17)
                            (push-stack 0)
                            (set-reg 0 49)
                            (push-stack 0)
                            (peak-stack 0 1)
                            (peak-stack 1)
                            (push-stack 1))
          expected (-> initial-state
                       (state-set-reg 0 17)
                       (state-set-reg 1 49)
                       (state-push-stack 17)
                       (state-push-stack 49)
                       (state-push-stack 49))
          actual (:memory (run-machine program ""))]
      (is (states-equal actual expected)))))
