(ns brainfuck.compiler-test
  (:require [clojure.test :refer :all]
            [brainfuck.compiler :refer :all]
            [brainfuck.interpreter :refer :all]))

(def deep-str #'brainfuck.compiler/deep-str)
(def initialize-state @#'brainfuck.compiler/initialize-state)

(load "compiler_test_util" "compiler_test_regs" "compiler_test_stack"
      "compiler_test_mem" "compiler_test_flow" "compiler_test_math")
