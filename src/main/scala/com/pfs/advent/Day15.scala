package com.pfs.advent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day15 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    Console.out.println("2020 15...")
    val ls = input.split(',').toList
    ls.foreach(Console.out.println(_))
    
    // part1( ls.map( _.toInt ) )
    part2( ls.map( _.toInt ) )
    
  }
  
  def part1( is : List[Int] ) = {
    
    val limit = 2020
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
  
  def part2( is : List[Int] ) = {

    val limit = 30000000
    // val limit = 2020
    var spokenTurn = mutable.HashMap[Int,Int]()
    val zs = is.zipWithIndex.reverse.tail
    zs.foreach( t => { spokenTurn += ( t._1 -> ( t._2 + 1 ) )})
    println(spokenTurn)

    def innerPart2( turn : Int, lastSpoken : Int ) : Int = {

      if( turn > limit ) {
        lastSpoken
      }
      else {
        
        val next = if( !spokenTurn.contains(lastSpoken) ){
          // next spoken is 0
          spokenTurn += ( lastSpoken -> (turn - 1) ) 
          0
        }
        else {
          
          // calc the age
          val prev = spokenTurn(lastSpoken)
          val age = turn - prev - 1
          spokenTurn(lastSpoken) = turn - 1
          age 
        }

        innerPart2( turn + 1, next )
      }
    }

    val last = innerPart2( is.size + 1, is.last )

    println(last)
    println("done")
  }

  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty )

  val test1 = """0,3,6"""
  val test = """3,1,2"""
  
  val input = """14,3,1,0,9,5"""

}
