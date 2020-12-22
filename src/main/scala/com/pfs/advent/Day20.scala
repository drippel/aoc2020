package com.pfs.advent

object Day20 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    Console.out.println("2020 20...")
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
