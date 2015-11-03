module Benchmarks (benchmark) where

import Array
import Benchmark
import Fixtures
import Simplify

many = Array.fromList Fixtures.many
seventythousand = Array.fromList Fixtures.seventythousand

benchmark : Benchmark.Benchmark
benchmark =
  Benchmark.suite "run"
    [ Benchmark.test "many with 1" (\_ -> Simplify.run 1 many)
    , Benchmark.test "many with 5" (\_ -> Simplify.run 5 many)
    , Benchmark.test "many with 20" (\_ -> Simplify.run 20 many)
    , Benchmark.test "thousands with 1" (\_ -> Simplify.run 1 seventythousand)
    , Benchmark.test "thousands with 5" (\_ -> Simplify.run 5 seventythousand)
    , Benchmark.test "thousands with 20" (\_ -> Simplify.run 20 seventythousand)
    ]

