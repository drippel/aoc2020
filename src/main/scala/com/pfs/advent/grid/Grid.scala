package com.pfs.advent.grid

import scala.collection.immutable.ListSet

class Grid( val rows : Int, val cols : Int, val init : Char = '.' ) {
  
  val cells = {
    val tc = Array.ofDim[Char](rows, cols )
    for( r <- 0 until rows )
      for( c <- 0 until cols )
        tc(r)(c) = init

    tc
  }

  def apply( r : Int, c : Int ) = cells(r)(c)

  def apply( r : Int, c : Int, ch : Char ) = cells(r)(c) = ch

  def apply( pos : Pos ) = cells(pos.row)(pos.col)

  def apply( pos : Pos, ch : Char ) = cells(pos.row)(pos.col) = ch

}

abstract class Dir( val row : Int, val col : Int )
case class East() extends Dir( 0, 1 )
case class West() extends Dir( 0, -1 )
case class North() extends Dir( -1, 0 )
case class South() extends Dir( 1, 0 )

case class NorthEast() extends Dir( -1, 1 )
case class SouthEast() extends Dir( 1, 1 )
case class NorthWest() extends Dir( -1, -1 )
case class SouthWest() extends Dir( 1, -1 )

val Dirs = Set( East(), West(), North(), South() )
val AllDirs = Set( East(), West(), North(), South(), NorthEast(), SouthEast(), SouthWest(), NorthWest() )

case class Pos( val row : Int, col : Int )

case class Path( steps : ListSet[Pos] )

object Grid {

  def main(args: Array[String]): Unit =
    Console.out.println("grid...")

  def print( g : Grid ) =
    for( r <- 0 until g.rows )
      for( c <- 0 until g.cols )
        Console.out.print(g(r,c))
    Console.out.print('\n')

  def parse( raw : String ) : Grid = 
    val lines = raw.split("\n").toList.map(_.trim).toList
    val rx = lines.size
    val cx = lines(0).size
    val g = new Grid(rx, cx)
    for (r <- 0 until rx) {
      val l = lines(r)
      for (c <- 0 until l.size) {
        g(r, c, lines(r)(c))
      }
    }

    g
  end parse

}
