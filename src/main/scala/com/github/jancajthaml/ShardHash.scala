package com.github.jancajthaml

object shardHash extends ((String, Int) => String) {

  def apply(x: String, modulo: Int): String = {
    val radix: Int = 16
    var result: Int = 0

    x.getBytes.foreach { b => {
      val codePoint = b.toInt
      val digit: Int = com.github.jancajthaml.Latin.A(b.toChar)

      result *= radix

      if ((digit & 0x1F) == 9) {
        val value = codePoint + ((digit & 0x3E0) >> 5) & 0x1F
        if (value < radix) {
          result += value
        } else {
          result -= 1
        }
      } else if ((digit & 0xC00) == 0x00000C00) {
        val value = (codePoint + ((digit & 0x3E0) >> 5) & 0x1F) + 10
        if (value < radix) {
        result += value
        } else {
          result -= 1
        }
      }

      if (result >= modulo) {
        result %= modulo 
      }

    } }

    result.toString
  }

}