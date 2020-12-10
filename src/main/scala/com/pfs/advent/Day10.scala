package com.pfs.advent

object Day10 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    Console.out.println("2020 10...")
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
