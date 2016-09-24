package com.github.jancajthaml

import org.scalameter.api.{Measurer, Bench, Gen, exec}

object ReferencePerformance extends Bench.OfflineReport {

  val times = Gen.range("times")(0, 100000, 20000)
  
  val id: String = "507f1f77bcf86cd799439011"
  val modulus: Int = 300

  performance of "BigInt" in {
    using(times) config (
      exec.benchRuns -> 20,
      exec.independentSamples -> 1,
      exec.outliers.covMultiplier -> 1.5,
      exec.outliers.suspectPercent -> 40
    ) in { sz => { (0 to sz).foreach { x => { ((BigInt.apply(id, 16) % modulus).toString) } } } }
  }

}

object RegressionPerformance extends Bench.OfflineReport {

  val times = Gen.range("times")(0, 100000, 20000)

  val id: String = "507f1f77bcf86cd799439011"
  val modulus: Int = 300

  performance of "com.github.jancajthaml" in {
    measure method "shardHash" in {
      using(times) config (
        exec.benchRuns -> 20,
        exec.independentSamples -> 1,
        exec.outliers.covMultiplier -> 1.5,
        exec.outliers.suspectPercent -> 40
      ) in { sz => { (0 to sz).foreach { x => { shardHash(id, modulus) } } } }
    }
  }

}