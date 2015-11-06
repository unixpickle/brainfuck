(ns brainfuck.compiler-test
  (:require [clojure.test :refer :all]
            [brainfuck.compiler :refer [initialize-state reg-count scratch-size]]
            [brainfuck.interpreter :refer :all]))

(def reg-inc @#'brainfuck.compiler/reg-inc)
(def reg-dec @#'brainfuck.compiler/reg-dec)
(def reg-to-scratch #'brainfuck.compiler/reg-to-scratch)

(deftest initialize-state-test
  (testing "initialize-state"
    (let [{:keys [memory pointer]} (run-machine initialize-state "")
          regs (apply concat (repeat reg-count '(0 256 1 2)))
          null-preamble (repeat (+ 4 scratch-size) 0)
          suffix '(0 2)
          expected-memory (concat null-preamble regs suffix)]
      (is pointer (+ scratch-size (* (inc reg-count) 4)))
      (is (= expected-memory (take (count expected-memory) memory))))))

(deftest reg-inc-dec-test
  (testing "reg-inc/reg-dec"
    (let [programs (map #(str initialize-state "<<<<" %)
                        (list reg-inc
                              (str reg-inc reg-inc)
                              (apply str (repeat 256 reg-inc))
                              (apply str (repeat 259 reg-inc))
                              reg-dec
                              (str reg-dec reg-dec)
                              (apply str (repeat 256 reg-dec))
                              (apply str (repeat 259 reg-dec))))
          prefix (repeat (+ 4 scratch-size) 0)
          regs (apply concat (repeat (dec reg-count) '(0 256 1 2)))
          suffix '(1 2 0 2)
          expected (map #(concat prefix regs (list % (- 256 %)) suffix)
                        '(1 2 0 3 255 254 0 253))
          mem-len (+ 4 scratch-size (* (inc reg-count) 4))
          actual (map #(take mem-len (:memory (run-machine % ""))) programs)]
      (dorun (map #(is (= %1 %2)) actual expected)))))

(deftest reg-to-scratch-test
  (testing "reg-to-scratch"
    (let [s0 [0 1 2 2] s1 [1 2 0 1]
          init (str initialize-state "<<<<" (apply str (repeat 100 reg-inc)))
          programs (map #(str init (reg-to-scratch %1 %2)) s0 s1)
          scratchRegs (map #(assoc [0 0 0] % 100) s0)
          prefixes (map #(concat %1 (repeat (inc scratch-size) 0)) scratchRegs)
          normalRegs (apply concat (repeat (dec reg-count) '(0 256 1 2)))
          body (concat normalRegs [100 156 1 2 0 2])
          mem-len (+ 4 scratch-size (* (inc reg-count) 4))
          expected (map #(concat % body) prefixes)
          actual (map #(take mem-len (:memory (run-machine % ""))) programs)]
      (dorun (map #(is (= %1 %2)) actual expected)))))
