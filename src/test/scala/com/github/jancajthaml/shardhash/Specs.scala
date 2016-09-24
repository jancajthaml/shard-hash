package com.github.jancajthaml

import org.scalatest.{FlatSpec, Matchers}

class ShardHashSpecs extends FlatSpec with Matchers {

  "shardHash" should "have same result as BigInt(x) % modulus" in {
    val id: String = "507f1f77bcf86cd799439011"
    val modulus: Int = 300

    val hash: String = shardHash(id, modulus)

    hash should === ((BigInt.apply(id, 16) % modulus).toString)
  }

}