(ns brainfuck.compiler-test
  (:require [clojure.test :refer :all]
            [brainfuck.compiler :refer :all]
            [brainfuck.interpreter :refer :all]))

(deftest initialize-state-test
  (testing "initialize-state"
    (let [{:keys [memory pointer]} (run-machine initialize-state "")
          regs (apply concat (repeat 10 '(0 256 1 2)))
          null-preamble (repeat (+ 4 scratch-size) 0)
          suffix (concat (repeat 0 5) '(0 2))
          expected-memory (concat null-preamble regs suffix)]
      (is pointer (+ scratch-size (* (inc reg-count) 4)))
      (is (= expected-memory (take (count expected-memory) memory))))))
