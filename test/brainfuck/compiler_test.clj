(ns brainfuck.compiler-test
  (:require [clojure.test :refer :all]
            [brainfuck.compiler :refer [initialize-state reg-count scratch-size]]
            [brainfuck.interpreter :refer :all]))

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
          init (str initialize-state "<<<<" (apply str (repeat 100 reg-inc)))
          programs (map #(str init (reg-to-scratch %1 %2)) s0 s1)
          before (state-set-reg initial-state (dec reg-count) 100)
          expected (map #(state-set-scratch before % 100) s0)
          actual (map #(:memory (run-machine % "")) programs)]
      (are-states-equal actual expected))))
