package com.github.jancajthaml

import org.scalatest.{FlatSpec, Matchers}
import reactivemongo.bson.BSONObjectID

class ShardHashSpecs extends FlatSpec with Matchers {
  
  private def referenceHasher(x: String, m: Int): String =
    (BigInt.apply(x, 16) % m).toString

  "shardHash" should "have same result as BigInt(x) % modulus" in {
    val id: String = "507f1f77bcf86cd799439011"
    val modulus: Int = 300
    val hash: String = shardHash(id, modulus)

    hash should === (referenceHasher(id, modulus))
  }

  it should "be consistent in different ids (10000 times)" in {
    val modulus: Int = 300

    (0 to 10000).foreach { t => {
      val id: String = BSONObjectID.generate().stringify
      shardHash(id, modulus) should === (referenceHasher(id, modulus))
    } }
  }

  it should "be consistent in different modulus (1 to 10000)" in {
    val id: String = BSONObjectID.generate().stringify

    (1 to 10000).foreach { modulus => {
      shardHash(id, modulus) should === (referenceHasher(id, modulus))
    } }
  }

}