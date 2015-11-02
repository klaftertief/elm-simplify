module Benchmarks (benchmark) where

import Benchmark
import Fixtures
import Simplify

benchmark : Benchmark.Benchmark
benchmark =
  Benchmark.suite "run"
    [ Benchmark.test "many with 1" (\_ -> Simplify.run 1 Fixtures.many)
    , Benchmark.test "many with 5" (\_ -> Simplify.run 5 Fixtures.many)
    , Benchmark.test "many with 20" (\_ -> Simplify.run 20 Fixtures.many)
    , Benchmark.test "thousands with 1" (\_ -> Simplify.run 1 Fixtures.seventythousand)
    , Benchmark.test "thousands with 5" (\_ -> Simplify.run 5 Fixtures.seventythousand)
    , Benchmark.test "thousands with 20" (\_ -> Simplify.run 20 Fixtures.seventythousand)
    ]

