package com.github.jancajthaml

object shardHash extends ((String, Int) => String) {

  def apply(x: String, mod: Int): String = {
    var hash: Int = 0
    val limit: Int = ((Int.MaxValue >>> 4) * .8).asInstanceOf[Int]

    //@info remove mutation and introduce recursion
    x.getBytes.foreach { b => {
      hash = (hash << 4) + (if (b <= 57) (b + 16 & 0x1F) else ((b + 31 & 0x1F) + 10))

      if (hash > limit) {
        hash %= mod 
      }
    } }

    (if (hash >= mod) (hash % mod) else hash).toString
  }

}