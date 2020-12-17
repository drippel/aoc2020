package com.pfs.advent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day17 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    Console.out.println("2020 17...")
    val ls = toLines(input)
    ls.foreach(Console.out.println(_))
    
    part1(ls)
    
  }
  
  // 3 ^ 4 - 0,0,0,0
  def genCoords( dims : Int ) : List[List[Int]] = {
    
    def innerGen( dim : Int, accum : List[List[Int]]  ) : List[List[Int]] = {
      
      if( dim <= 0 ) {
        accum
      }
      else {
        
        val next = ListBuffer[List[Int]]() 
        for( l <- accum ) {
          next += l :+ 0
          next += l :+ -1
          next += l :+ 1 
        }
        innerGen( dim - 1, next.toList )
      }
      
    }
    
    val res = innerGen( dims - 1, List( List(0), List(1), List(-1)))
    res.filterNot( l => l.forall( _ == 0 ) )
    
  }
  
  val ACTIVE = '#'
  val INACTIVE = '.'
  
  def part1( ls : List[String] ) : Int = {
    
    val limit = 6
    var noOfCubes = 0
    
    val start = parse(ls)
    println(start)

    val noOfDims = start.head._1.size
    var state = start
    println("t = 0")
    printCube(state)
    val genNieghbors = genCoords(noOfDims)
    println(genNieghbors)

    for( cycle <- 0 until limit ) {

      // find bounding dims of current state
      val (min,max) = findBoundingBox(start)
      println(s"cycle = ${cycle}")
      println(s"bs = ${min} ${max}")
      val offset = cycle + 1

      // this is our accum for the dimloop
      val nextState = mutable.HashMap[List[Int],Char]()
      
      def dimLoop( currentCoords : List[Int] ) : Unit = {
        
        if( currentCoords.size == noOfDims ) {
          
          println(currentCoords)
          
          // find all 3d neighbors
          val ns = for{ 
            n <- genNieghbors 
            z = currentCoords.zip(n)
            zs = z.map( t => t._1 + t._2 )
          } yield zs 
          
          // println(ns)
          
          val nss = ns.map( t => state.getOrElse( t, INACTIVE ) )
          // println(nss)
          val activeCount = nss.count( _ == ACTIVE )

          val currentState = state.getOrElse( currentCoords, '.' )

          if( currentState == ACTIVE ) {
            if( activeCount == 2 || activeCount == 3 ) {
              nextState( currentCoords ) = ACTIVE
            }
            else {
              // INACTIVE - don't add to map
            }
          }

          if( currentState == INACTIVE ) {
            if( activeCount == 3 ) {
              nextState( currentCoords ) = ACTIVE
            }
            else {
              // INACTIVE
            }
          }
        }
        else {
          
          // what is the size of the current input
          val currDim = currentCoords.size
          val nextDim = currDim 
          // the starting value for this dim is
          val dimStart = min(nextDim) - offset
          val dimEnd = max(nextDim) + offset
          // println( s"${nextDim} ${dimStart} ${dimEnd}")
          for( d <- dimStart to dimEnd ) {
            val nextCoords = currentCoords :+ d
            dimLoop( nextCoords )
          }
        }
      }
      
      dimLoop(List())
      
      state = nextState.toMap
      // printCube(state)
    }
    
    // printCube(state)
    println(state)
    val factive = state.size
    println( factive )
    
    noOfCubes
  }
  
  def printCube( cube : Map[List[Int],Char] ) = {
    val bounds = findBoundingBox(cube)
    println("not yet...")
  }
  
  def parse( ls : List[String] ) : Map[List[Int],Char] = {
    
    val res = mutable.HashMap[List[Int],Char]()
    
    for( y <- 0 until ls.size ) {
      for( x <- 0 until ls(y).size ) {
        res += ( List(x,y,0,0) -> ls(y)(x) )
      }
    }
    
    res.toMap
    
  }
  
  def findBoundingBox( cube : Map[List[Int],Char] ) : (List[Int],List[Int]) = {
    
    val dims = cube.head._1.size
    
    val mins = mutable.ListBuffer[Int]()
    val maxs = mutable.ListBuffer[Int]()
    
    for( idx <- 0 until dims ) {

      val is = cube.keys.map( t => t(idx) ).toList.sorted

      val (minI, maxI) = (is.head, is.last)
      val (bMinI, bMaxI) = (minI - 1, maxI + 1)
      mins += bMinI
      maxs += bMaxI
      
    }
    
    ( mins.toList, maxs.toList )

  }
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """.#.
       ..#
       ###"""
  
  val input =
    """.##..#.#
       #...##.#
       ##...#.#
       .##.##..
       ...#.#.#
       .##.#..#
       ...#..##
       ###..##."""

}
