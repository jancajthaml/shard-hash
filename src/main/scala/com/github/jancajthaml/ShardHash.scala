package com.github.jancajthaml

object shardHash extends ((String, Int) => String) {

  def apply(x: String, modulo: Int): String = {
    var hash: Int = 0
    val limit: Int = ((Int.MaxValue / 16) * 0.8).asInstanceOf[Int]

    x.getBytes.foreach { b => {
      //@info this should be part or private object x in scala here
      val digit: Int = com.github.jancajthaml.Latin.A(b.toChar)

      hash *= 16

      if ((digit & 0x1F) == 9) {
        val codePoint = b.toInt + ((digit & 0x3E0) >> 5) & 0x1F
        hash = if (codePoint < 16) hash + codePoint else hash - 1
      } else if ((digit & 0xC00) == 0x00000C00) {
        val codePoint = b.toInt + ((digit & 0x3E0) >> 5) & 0x1F
        hash = if (codePoint < 26) hash + codePoint + 10 else hash - 1
      }

      if (hash >= limit) {
        hash %= modulo 
      }
    } }

    (if (hash >= modulo) (hash % modulo) else hash).toString
  }

}