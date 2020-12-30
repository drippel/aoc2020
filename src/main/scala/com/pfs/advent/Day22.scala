package com.pfs.advent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day22 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    Console.out.println("2020 22...")
    val ls = toLines(input)

    val ps = parse(ls)
    ps.foreach(println(_))

    val w = game( 1, 1, ps(0), ps(1))
    println(w)
    println( calcScore(w) )

  }

  def calcScore( winner : Player ) : Long = {

    // calc the score
    val size = winner.deck.size

    val cardVals = (1 to size).toList.reverse

    val zs = winner.deck.zip(cardVals)
    val ps = zs.map( t => t._1 * t._2 )
    val sum = ps.foldLeft(0L)( _ + _ )

    sum
  }

  val roundCache = mutable.HashSet[(mutable.Queue[Int],mutable.Queue[Int])]()

  def game( gid : Int, rid : Int,  p1 : Player, p2 : Player ) : Player = {
    
    println( s"Game: ${gid} Round: ${rid}")
    
    if( p1.deck.size == 0 ) {
        p2
    }
    else if( p2.deck.size == 0 ) {
        p1
    }
    else if( roundCache.contains( (p1.deck, p2.deck ) ) ) {
      println("***********************dupe check**************************************")
      p1
    }
    else {
      
      // add this state to the round check
      val cs =( p1.deck, p2.deck )
      roundCache += cs 

      val p1c = p1.deck.dequeue()
      val p2c = p2.deck.dequeue()
      
      // sub game check
      if( p1c <= p1.deck.size && p2c <= p2.deck.size ) {
        
        println("sub game")
        // println( s"${p1c} ${p2c}")
        // println( s"${p1.deck}")
        // println( s"${p2.deck}")
        
        // make a new p1 and p2 for the sub game
        val p1s = Player( p1.name, p1.deck.slice(0, p1c))
        val p2s = Player( p2.name, p2.deck.slice(0, p2c))
        val wp = game( gid + 1, 0, p1s, p2s )
        if( wp.name.equalsIgnoreCase(p1.name) ) {
          p1.deck += p1c
          p1.deck += p2c
        }
        else {
          p2.deck += p2c
          p2.deck += p1c
        }
      }
      else {

        if( p1c > p2c ) {
          p1.deck += p1c
          p1.deck += p2c
        }
        else {
          p2.deck += p2c
          p2.deck += p1c
        }
      }
      
      game( gid, rid + 1, p1, p2 )
    }

  }

  def parse( lines : List[String] ) : List[Player] = {

    def innerParse( ls : List[String], accum : List[Player] ) : List[Player] = {
      if( ls.isEmpty ) {
        accum
      }
      else {

        val line = ls.head
        val nextAccum = if( line.contains("Player")) {

          // start a new player
          val p = Player( line, mutable.Queue[Int]() )
          accum :+ p

        }
        else {

          // the current line belongs to the last player
          val p = accum.last
          p.deck += line.trim.toInt

          accum
        }

        innerParse(ls.tail, nextAccum )
      }
    }

    innerParse( lines, List() )

  }

  case class Player( name : String, deck : mutable.Queue[Int] )
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """Player 1:
      9
      2
      6
      3
      1
      
      Player 2:
      5
      8
      4
      7
      10"""
  
  val input =
    """Player 1:
      6
      25
      8
      24
      30
      46
      42
      32
      27
      48
      5
      2
      14
      28
      37
      17
      9
      22
      40
      33
      3
      50
      47
      19
      41
      
      Player 2:
      1
      18
      31
      39
      16
      10
      35
      29
      26
      44
      21
      7
      45
      4
      20
      38
      15
      11
      34
      36
      49
      13
      23
      43
      12"""

}
