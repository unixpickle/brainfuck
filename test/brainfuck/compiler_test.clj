(ns brainfuck.compiler-test
  (:require [clojure.test :refer :all]
            [brainfuck.compiler :refer :all]
            [brainfuck.interpreter :refer :all]))

(def reg-inc @#'brainfuck.compiler/reg-inc)

(deftest initialize-state-test
  (testing "initialize-state"
    (let [{:keys [memory pointer]} (run-machine initialize-state "")
          regs (apply concat (repeat reg-count '(0 256 1 2)))
          null-preamble (repeat (+ 4 scratch-size) 0)
          suffix '(0 2)
          expected-memory (concat null-preamble regs suffix)]
      (is pointer (+ scratch-size (* (inc reg-count) 4)))
      (is (= expected-memory (take (count expected-memory) memory))))))

(deftest reg-inc-test
  (testing "reg-inc basic"
    (let [programs (map #(str initialize-state "<<<<" %)
                        (list reg-inc
                              (str reg-inc reg-inc)
                              (apply str (repeat 256 reg-inc))
                              (apply str (repeat 259 reg-inc))))
          prefix (concat (repeat (+ 4 scratch-size) 0))
          regs (apply concat (repeat (dec reg-count) '(0 256 1 2)))
          suffix '(1 2 0 2)
          expected (map #(concat prefix regs (list % (- 256 %)) suffix)
                        '(1 2 0 3))
          mem-len (+ scratch-size (* (inc reg-count) 4))
          actual (map #(take mem-len (:memory (run-machine % ""))) programs)]
      (println (nth programs 0))
      (dorun (map #(is (= %1 %2)) actual expected)))))
