module Main where

import Html exposing (Html, text)
import Signal
import Task

import Benchmark
import Benchmarks


-- COMPILER BENCHMARK

simplifyBenchmark : Benchmark.Benchmark
simplifyBenchmark =
  Benchmark.suite "Simplify"
    [ Benchmarks.benchmark ]


-- DISPLAY THINGS

main =
  results.signal


results : Signal.Mailbox Html
results =
  Signal.mailbox (text "Working...")


port benchResults : Task.Task x ()
port benchResults =
  Benchmark.run simplifyBenchmark
    `Task.andThen` (Benchmark.view >> Signal.send results.address)
