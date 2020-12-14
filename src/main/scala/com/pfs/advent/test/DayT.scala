package com.pfs.advent.test

object DayT {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    Console.out.println("2020 T...")
    val ls = toLines(input)
    ls.foreach(Console.out.println(_))
    
  }
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """
      |
      |""".stripMargin
  
  val input =
    """
      |
      |""".stripMargin

}
