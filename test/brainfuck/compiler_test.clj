(ns brainfuck.compiler-test
  (:require [clojure.test :refer :all]
            [brainfuck.compiler :refer :all]
            [brainfuck.interpreter :refer :all]))

(load "compiler_test_util" "compiler_test_regs" "compiler_test_stack"
      "compiler_test_mem")
