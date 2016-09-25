package com.github.jancajthaml

object shardHash extends ((String, Int) => String) {

  def apply(x: String, mod: Int): String = {
    var hash: Int = 0
    //@info we assume that reasonable threshold is 40:60 instead of median
    val limit: Int = ((Int.MaxValue >>> 5) * .4 + (Int.MaxValue >>> 4) * .6).asInstanceOf[Int]
    //@info remove mutation and introduce recursion
    x.getBytes("US-ASCII").foreach { b => {
      hash = ((hash << 4) + (if (b < 58) (b + 16 & 0x1f) else ((b + 31 & 0x1f) + 10)))
      if (hash > limit) hash %= mod
    } }

    (if (hash >= mod) (hash % mod) else hash).toString
  }

}