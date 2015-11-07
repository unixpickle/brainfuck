(ns brainfuck.compiler-test
  (:require [clojure.test :refer :all]
            [brainfuck.compiler :refer :all]
            [brainfuck.interpreter :refer :all]))

(def deep-str #'brainfuck.compiler/deep-str)
(def inc-current-reg @#'brainfuck.compiler/inc-current-reg)
(def dec-current-reg @#'brainfuck.compiler/dec-current-reg)
(def reg-to-scratch #'brainfuck.compiler/reg-to-scratch)

(deftest initialize-state-test
  (testing "initialize-state"
    (let [{:keys [memory pointer]} (run-machine initialize-state "")]
      (is pointer (+ scratch-size (* (inc reg-count) 4)))
      (states-equal memory initial-state))))

(deftest inc-dec-current-reg-test
  (testing "inc-current-reg/dec-current-reg"
    (let [programs (map #(str initialize-state "<<<<" %)
                        (list inc-current-reg
                              (str inc-current-reg inc-current-reg)
                              (apply str (repeat 256 inc-current-reg))
                              (apply str (repeat 259 inc-current-reg))
                              dec-current-reg
                              (str dec-current-reg dec-current-reg)
                              (apply str (repeat 256 dec-current-reg))
                              (apply str (repeat 259 dec-current-reg))))
          actual (map #(:memory (run-machine % "")) programs)
          expected (map #(state-set-reg initial-state (dec reg-count) %)
                        [1 2 0 3 255 254 0 253])]
      (are-states-equal actual expected))))

(deftest reg-to-scratch-test
  (testing "reg-to-scratch"
    (let [s0 [0 1 2 2] s1 [1 2 0 1]
          init (deep-str initialize-state "<<<<" (repeat 100 inc-current-reg))
          programs (map #(str init (reg-to-scratch %1 %2)) s0 s1)
          before (state-set-reg initial-state (dec reg-count) 100)
          expected (map #(state-set-scratch before % 100) s0)
          actual (map #(:memory (run-machine % "")) programs)]
      (are-states-equal actual expected))))

(deftest mov-reg-test
  (testing "mov-reg"
    (let [lastReg (dec reg-count)
          sources [0 2 1 lastReg]
          destinations [lastReg 1 2 0]
          start-vals [1 2 3 4]
          programs (map #(deep-str initialize-state
                                   (repeat (- reg-count %2) "<<<<")
                                   (repeat 17 inc-current-reg)
                                   (repeat (- reg-count %2) ">>>>")
                                   (repeat (- reg-count %1) "<<<<")
                                   (repeat %3 inc-current-reg)
                                   (repeat (- reg-count %1) ">>>>")
                                   (mov-reg %1 %2))
                        destinations sources start-vals)
          results (map #(run-machine % "") programs)
          actual (map :memory results)
          pointers (map :pointer results)
          expected-pointer (+ 4 scratch-size (* 4 reg-count))
          expected (map #(-> initial-state (state-set-reg %1 17) (state-set-reg %2 17))
                        sources destinations)]
      (is (= pointers (repeat (count pointers) expected-pointer)))
      (are-states-equal actual expected))))

(deftest set-reg-test
  (testing "set-reg"
    (let [vals (range 256)
          regs (repeatedly (count vals) (partial rand-int reg-count))
          start-vals (repeatedly (count vals) (partial rand-int 256))
          programs (map #(deep-str initialize-state
                                   (repeat (- reg-count %1) "<<<<")
                                   (repeat %3 inc-current-reg)
                                   (repeat (- reg-count %1) ">>>>")
                                   (set-reg %1 %2))
                        regs vals start-vals)
          results (map #(run-machine % "") programs)
          actual (map :memory results)
          pointers (map :pointer results)
          expected-pointer (+ 4 scratch-size (* 4 reg-count))
          expected (map #(state-set-reg initial-state %1 %2) regs vals)]
      (is (= pointers (repeat (count pointers) expected-pointer)))
      (are-states-equal actual expected))))
