package com.github.jancajthaml

object shardHash extends ((String, Int) => String) {

  def apply(x: String, modulo: Int): String = {
    //@info radix will always be 16 for hexadecimal, use as magic number
    //save allocation
    val radix: Int = 16
    var result: Int = 0

    x.getBytes.foreach { b => {
      val codePoint = b.toInt
      //@info this should be part or private object x in scala here
      val digit: Int = com.github.jancajthaml.Latin.A(b.toChar)

      //@info we can work in a lower radix kelner and then multiply the result
      //in the end will save "mutliply times number of character" steps
      result *= radix

      if ((digit & 0x1F) == 9) {
        //@info duplicate first part 
        val value = codePoint + ((digit & 0x3E0) >> 5) & 0x1F
        //@info change to mathematic operation instead of condition
        if (value < radix) {
          result += value
        } else {
          result -= 1
        }
      } else if ((digit & 0xC00) == 0x00000C00) {
        //@info duplicate first part 
        val value = (codePoint + ((digit & 0x3E0) >> 5) & 0x1F) + 10
        //@info change to mathematic operation instead of condition
        if (value < radix) {
          result += value
        } else {
          result -= 1
        }
      }

      if (result >= modulo) {
        //@info this is really costly operation, should implement smarter
        result %= modulo 
      }

    } }

    result.toString
  }

}