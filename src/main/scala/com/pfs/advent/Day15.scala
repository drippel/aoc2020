package com.pfs.advent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day15 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    Console.out.println("2020 15...")
    val ls = test1.split(',').toList
    ls.foreach(Console.out.println(_))
    
    part1( ls.map( _.toInt ) )
    
  }
  
  def part1( is : List[Int] ) = {
    
    val limit = 30000000
    var spoken = ListBuffer[Int]()
    spoken ++= is
    
    def innerPart1( turn : Int ) : Int = {
      
      if( spoken.length >= limit ) {
        spoken.last
      }
      else {
        // get the last number spoken
        // was this number spoken before?
        val last = spoken.last
        val idx = spoken.lastIndexOf(last,spoken.length - 2) 
        if( idx == -1 ) {
          spoken += 0
        }
        else {
          // we have seen this before
          val age = spoken.length - ( idx + 1 )
          spoken += age
        }
        
        innerPart1(turn + 1)
      }
    }
    
    val last = innerPart1( 4 )
  
    println("done")
    println( last )
  }
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty )

  val test1 = """0,3,6"""
  val test = """1,3,2"""
  
  val input = """14,3,1,0,9,5"""

}
