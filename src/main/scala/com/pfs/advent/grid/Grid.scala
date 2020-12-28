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

  override def toString: String = {
    var s = ""
    for( r <- 0 until this.rows )
      for( c <- 0 until this.cols )
        s = s + this(r,c)
      s = s + "\n"
    s
  } 

  def apply( r : Int, c : Int ) = cells(r)(c)

  def apply( r : Int, c : Int, ch : Char ) = cells(r)(c) = ch

  def apply( pos : Grid.Pos ) = cells(pos.row)(pos.col)

  def apply( pos : Grid.Pos, ch : Char ) = cells(pos.row)(pos.col) = ch
  
  def charAt( r : Int, c : Int ) : Option[Char] = {
    if( r < 0 || r >= rows || c < 0 || c >= cols ) {
      None
    }
    else {
      Some( cells(r)(c))
    }
  }
  
  def adjacentChars( r : Int, c : Int, all : Boolean = true ) : List[Char] = {
    
    val ds = if( all ) {   Grid.AllDirs } else { Grid.Dirs }
    
    val coords = ds.map( d => Grid.add( d, (r,c))).toList
    
    coords.map( p => charAt(p._1, p._2)).flatten
    
  }
  
  def getAllInDir( rs : Int, cs : Int, dir : Grid.Dir ) : List[Char] = {
    
    def innerGetAllInDir( r : Int, c : Int, accum : List[Char] ) : List[Char] = {
      
      if( r < 0 || r >= rows || c < 0 || c >= cols ) { accum }
      else {
        val n = accum :+ this(r,c)
        innerGetAllInDir( r + dir.row, c + dir.col, n )
      }
    }
    
    innerGetAllInDir( rs + dir.row, cs + dir.col, List() )
  }

  def firstInDir( rs : Int, cs : Int, dir : Grid.Dir, ec : Char = '.' ) : Option[Char] = {

    def innerFirstInDir( r : Int, c : Int, found : Option[Char] ) : Option[Char] = {

      if( r < 0 || r >= rows || c < 0 || c >= cols || found.isDefined ) { 
        found 
      }
      else {
        val n = this(r,c)
        val nf = if( n != ec ) { Some(n) }
        else { None }
        innerFirstInDir( r + dir.row, c + dir.col, nf )
      }
    }
    
    innerFirstInDir( rs + dir.row, cs + dir.col, None )

  }
  
  def row( i : Int ) = {
    val cs = for{
      c <- 0 until cols
    } yield this(i,c)
    
    cs.toList
  }
  
  def toRows() : List[List[Char]] = {
    val rs = for{
      r <- 0 until rows
    } yield row(r)
    
    rs.toList
  }
  
  def toCols() : List[List[Char]] = {
    val rs = for{
      r <- 0 until cols
    } yield col(r)

    rs.toList
  }

  def col( i : Int ) = {
    val rs = for{
      r <- 0 until rows
    } yield this(r,i)

    rs.toList
  }

  override def hashCode( ) : Int = {
    val rs = toRows().map( _.mkString ).mkString
    rs.hashCode
  }

  override def equals( obj : Any ) : Boolean = {
    
    if( obj.isInstanceOf[Grid]) {
      
      val rs = toRows().map( _.mkString ).mkString 
      val other = obj.asInstanceOf[Grid]
      val theirs = other.toRows().map( _.mkString ).mkString
      rs.equals(theirs)
    }
    else {
      false
    }
  } 
  
}


object Grid {

  abstract class Dir( val row : Int, val col : Int )
  case class East() extends Dir( 0, 1 )
  case class West() extends Dir( 0, -1 )
  case class North() extends Dir( -1, 0 )
  case class South() extends Dir( 1, 0 )

  case class NorthEast() extends Dir( -1, 1 )
  case class SouthEast() extends Dir( 1, 1 )
  case class NorthWest() extends Dir( -1, -1 )
  case class SouthWest() extends Dir( 1, -1 )

  val Dirs = Set[Dir]( East(), West(), North(), South() )
  val AllDirs = Set[Dir]( East(), West(), North(), South(), NorthEast(), SouthEast(), SouthWest(), NorthWest() )

  case class Pos( val row : Int, col : Int )

  case class Path( steps : ListSet[Pos] )
  
  def add( dir : Dir, pos : (Int,Int), scale : Int = 1 ) : (Int,Int) = {
    ( pos._1 + ( dir.row * scale ), pos._2 + ( dir.col * scale ) ) 
  }

  def main(args: Array[String]): Unit =
    Console.out.println("grid...")

  def print( g : Grid ) =
    for( r <- 0 until g.rows )
      for( c <- 0 until g.cols )
        Console.out.print(g(r,c))
      Console.out.print("\n")
    Console.out.print('\n')

  def parse( raw : String ) : Grid = 
    val lines = raw.split("\n").toList.map(_.trim).filter( !_.isEmpty ).toList
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
