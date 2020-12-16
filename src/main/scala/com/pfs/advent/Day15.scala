package com.pfs.advent

import java.time.Instant
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day15 {

  def main( args : Array[String] ) : Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    Console.out.println( "2020 15..." )
    val ls = input.split( ',' ).toList
    ls.foreach( Console.out.println( _ ) )

    // part1( ls.map( _.toInt ) )
    println( Instant.now().toString )
    part2( ls.map( _.toInt ) )
    println( Instant.now().toString )
    stolen()
    println( Instant.now().toString )
    stolen2()
    println( Instant.now().toString )

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
        val idx = spoken.lastIndexOf( last, spoken.length - 2 )
        if( idx == -1 ) {
          spoken += 0
        }
        else {
          // we have seen this before
          val age = spoken.length - ( idx + 1 )
          spoken += age
        }

        innerPart1( turn + 1 )
      }
    }

    val last = innerPart1( 4 )

    println( "done" )
    println( last )
  }

  def stolen( ) : Unit = {

    def playGame( numbers : Map[Int, Int], turn : Int, number : Int ) : Int = {
      if( turn == 30000000 )
        number
      else {
        playGame( numbers + ( number -> turn ), turn + 1, turn - numbers.getOrElse( number, turn ) )
      }
    }

    val start = Map( 1 -> 1, 0 -> 2, 18 -> 3, 10 -> 4, 19 -> 5 )
    println( playGame( start, 6, 6 ) )
  }

  def stolen2( ) : Unit = {

    val memory = mutable.HashMap[Int, Int]( 1 -> 1, 0 -> 2, 18 -> 3, 10 -> 4, 19 -> 5 )

    def playGame( turn : Int, number : Int ) : Int = {
      if( turn == 30000000 )
        number
      else {
        val nn = ( turn - memory.getOrElse( number, turn ) )
        memory( number ) = turn
        playGame( turn + 1, nn )
      }
    }

    println( playGame( 6, 6 ) )
  }

  def part2( is : List[Int] ) = {

    val limit = 30000000
    // val limit = 2020
    var spokenTurn = mutable.HashMap[Int, Int]()
    val zs = is.zipWithIndex.reverse.tail
    zs.foreach( t => {spokenTurn += ( t._1 -> ( t._2 + 1 ) ) } )
    println( spokenTurn )

    def innerPart2( turn : Int, lastSpoken : Int ) : Int = {

      if( turn > limit ) {
        lastSpoken
      }
      else {

        val next = if( !spokenTurn.contains( lastSpoken ) ) {
          // next spoken is 0
          spokenTurn += ( lastSpoken -> ( turn - 1 ) )
          0
        }
        else {
          // calc the age
          val prev = spokenTurn( lastSpoken )
          val age = turn - prev - 1
          spokenTurn( lastSpoken ) = turn - 1
          age
        }

        innerPart2( turn + 1, next )
      }
    }

    val last = innerPart2( is.size + 1, is.last )

    println( last )
    println( "done" )
  }

  def toLines( src : String ) = src.split( "\n" ).toList.map( _.trim ).filter( s => !s.isEmpty )

  val test1 = """0,3,6"""
  val test = """3,1,2"""

  val input = """14,3,1,0,9,5"""

}
