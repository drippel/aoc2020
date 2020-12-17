package com.pfs.advent

import scala.collection.mutable

object Day17 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    Console.out.println("2020 17...")
    val ls = toLines(input)
    ls.foreach(Console.out.println(_))
    
    part1(ls)
    
  }
  
  // one layer              N              NE             E             SE            S             SW             W              NW
  val allNeighbors = List(  ( -1, 0, 0 ),  ( -1, 1, 0 ),  ( 0, 1, 0 ),  ( 1, 1, 0 ),  (1, 0, 0),  ( 1, -1, 0 ),  ( 0, -1, 0 ),  ( -1, -1, 0 ),
                            ( -1, 0, -1 ), ( -1, 1, -1 ), ( 0, 1, -1 ), ( 1, 1, -1 ), (1, 0, -1), ( 1, -1, -1 ), ( 0, -1, -1 ), ( -1, -1, -1 ), ( 0, 0, -1 ),
                            ( -1, 0,  1 ), ( -1, 1,  1 ), ( 0, 1,  1 ), ( 1, 1,  1 ), (1, 0,  1), ( 1, -1,  1 ), ( 0, -1,  1 ), ( -1, -1,  1 ), ( 0, 0,  1) )
  
  val ACTIVE = '#'
  val INACTIVE = '.'
  
  def part1( ls : List[String] ) : Int = {
    
    val limit = 6
    var noOfCubes = 0
    
    val start = parse(ls)
    println(start)
    
    
    var state = start
    println("t = 0")
    printCube(state)

    for( cycle <- 0 until limit ) {

      // find bounding dims of current state
      val (min,max) = findBoundingBox(start)
      println(s"cycle = ${cycle}")
      println(s"bs = ${min} ${max}")
      val offset = cycle + 1

      val nextState = mutable.HashMap[(Int,Int,Int),Char]()
      
      // 3d loop through that?
      for( x <- ( min._1 - offset ) to ( max._1 + offset ) ) {
        
        for( y <- ( min._2 - offset ) to ( max._2 + offset ) ) {

          // this is not right
          for( z <- ( min._3 - offset ) to ( max._3 + offset ) ) {
            
            // println( s"${x},${y},${z}")
            // create blank new state

            // find all 3d neighbors
            val ns = allNeighbors.map( t => (t._1 + x, t._2 + y, t._3 + z ) )
            
            val nss = ns.map( t => state.getOrElse( t, INACTIVE ) )
            val activeCount = nss.count( _ == ACTIVE )
            
            val currentState = state.getOrElse( (x,y,z), '.' )

            if( currentState == ACTIVE ) {
              if( activeCount == 2 || activeCount == 3 ) {
                nextState( (x,y,z) ) = ACTIVE
              }
              else {
                // INACTIVE - don't add to map
              }
            }

            if( currentState == INACTIVE ) {
              if( activeCount == 3 ) {
                nextState( (x,y,z) ) = ACTIVE
              }
              else {
                // INACTIVE
              }
            }
          }
        }
      }
      
      state = nextState.toMap
      printCube(state)
    }
    
    printCube(state)
    val factive = state.size
    println( factive )
    
    noOfCubes
  }
  
  def printCube( cube : Map[(Int,Int,Int),Char] ) = {
    val bounds = findBoundingBox(cube)
    
    for( z <- bounds._1._3 to bounds._2._3 ) {
      println(s"layer = ${z}")
      for( y <- bounds._1._2 to bounds._2._2 ) {
        for( x <- bounds._1._1 to bounds._2._1 ) {
          val c = cube.getOrElse( (x,y,z), '.')
          print(c)
        }
        print("\n")
      }
      print("\n")
    }
  }
  
  def parse( ls : List[String] ) : Map[(Int,Int,Int),Char] = {
    
    val res = mutable.HashMap[(Int,Int,Int),Char]()
    
    for( y <- 0 until ls.size ) {
      for( x <- 0 until ls(y).size ) {
        res += ( (x,y,0) -> ls(y)(x) )
      }
    }
    
    res.toMap
    
  }
  
  def findBoundingBox( cube : Map[(Int,Int,Int),Char] ) = {
    
    val xs = cube.keys.map( t => t._1 ).toList.sorted
    val ys = cube.keys.map( t => t._2 ).toList.sorted
    val zs = cube.keys.map( t => t._3 ).toList.sorted
    
    val (minX,maxX) = (xs.head,xs.last)
    val (minY,maxY) = (ys.head,ys.last)
    val (minZ,maxZ) = (zs.head,zs.last)
    
    println( s"${minX},${maxX}  ${minY},${maxY}, ${minZ},${maxZ}")
    
    val (bMinX,bMaxX) = (minX - 1, maxX + 1)
    val (bMinY,bMaxY) = (minY - 1, maxY + 1)
    val (bMinZ,bMaxZ) = (minZ - 1, maxZ + 1)
    
    ((bMinX,bMinY,bMinZ),(bMaxX,bMaxY,bMaxZ))

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
