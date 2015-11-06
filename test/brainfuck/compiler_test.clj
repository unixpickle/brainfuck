(ns brainfuck.compiler-test
  (:require [clojure.test :refer :all]
            [brainfuck.compiler :refer :all]
            [brainfuck.interpreter :refer :all]))

(def deep-str #'brainfuck.compiler/deep-str)
(def reg-inc @#'brainfuck.compiler/reg-inc)
(def reg-dec @#'brainfuck.compiler/reg-dec)
(def reg-to-scratch #'brainfuck.compiler/reg-to-scratch)

(def initial-state
  (let [regs (apply concat (repeat reg-count '(0 256 1 2)))
        prefix (repeat (+ 4 scratch-size) 0)
        suffix '(0 2)]
    (concat prefix regs suffix)))

(defn state-set-scratch
  [state idx val]
  (seq (assoc (vec state) idx val)))

(defn state-set-reg
  [state idx val]
  (let [regIdx (+ 4 scratch-size (* 4 idx))]
    (seq (assoc (vec state) regIdx val (inc regIdx) (- 256 val)))))

(defn states-equal
  [s1 s2]
  (cond (< (count s1) (count s2)) (states-equal (concat s1 [0]) s2)
        (> (count s1) (count s2)) (states-equal s2 s1)
        :else (= (seq s1) (seq s2))))

(defn are-states-equal
  [s1 s2]
  (dorun (map #(is (states-equal %1 %2)) s1 s2)))

(deftest initialize-state-test
  (testing "initialize-state"
    (let [{:keys [memory pointer]} (run-machine initialize-state "")]
      (is pointer (+ scratch-size (* (inc reg-count) 4)))
      (states-equal memory initial-state))))

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
          actual (map #(:memory (run-machine % "")) programs)
          expected (map #(state-set-reg initial-state (dec reg-count) %)
                        [1 2 0 3 255 254 0 253])]
      (are-states-equal actual expected))))

(deftest reg-to-scratch-test
  (testing "reg-to-scratch"
    (let [s0 [0 1 2 2] s1 [1 2 0 1]
          init (deep-str initialize-state "<<<<" (repeat 100 reg-inc))
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
                                   (repeat 17 reg-inc)
                                   (repeat (- reg-count %2) ">>>>")
                                   (repeat (- reg-count %1) "<<<<")
                                   (repeat %3 reg-inc)
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
    (let [regs (range reg-count)
          vals (take (count regs) (take-nth 13 (range 256)))
          start-vals (repeatedly (count vals) rand)
          programs (map #(deep-str initialize-state
                                   (repeat (- reg-count %1) "<<<<")
                                   (repeat %3 reg-inc)
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
