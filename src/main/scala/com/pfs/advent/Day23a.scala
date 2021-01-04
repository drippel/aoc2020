package com.pfs.advent

import com.pfs.advent.utils.AOC

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day23a {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    // day20 - lots of grid functions
    Console.out.println("2020 23a...")
    
    val testData  = "3,8,9,1,2,5,4,6,7"
    val inputData = "1,6,7,2,4,8,3,5,9"
    
    // solve( makeArray(testData))
    val np = makeNexts(inputData)
    // np.foreach( println(_))
    // solve(np)
    println(np(9))
    println(np(10))
    println(np(1000000))
    println(np.size)
    
    AOC.timeIt( () => {
      solve( np )
    })
    
  }

  def solve( cups : Array[Int] ) = {

    def calcNext( c : Int, pus : Set[Int], max : Int, found : Option[Int] ) : Int = {

      if( found.isDefined ) {
        found.get
      }
      else {

        val (nextC, nextFound) = if( !pus.contains(c) && c > 0 ) {
          (-1,Some(c))
        }
        else {
          val nc = if( c <= 1 ) { max }
          else { c - 1 }
          (nc,None)
        }
        calcNext( nextC, pus, max, nextFound )
      }

    }

    var currentCup = 1
    for( m <- 0 until 10000000 ) {
      // println( s"Move: ${m}")
      // println( s"Current: ${currentCup}")

      // pick up 
      val c1 = cups(currentCup)
      val c2 = cups(c1)
      val c3 = cups(c2)

      // println( s"Pickup: ${c1} ${c2} ${c3}")

      // 
      val dest = calcNext( (currentCup - 1), Set(c1,c2,c3), 1000000, None )
      // println( s"Destination: ${ dest }")
      cups(currentCup) = cups(c3)
      cups(c3) = cups(dest)
      cups(dest) = c1

      // println( s"Next: ${ cups(currentCup) }")
      // println(" ")
      currentCup = cups(currentCup)
    }

    val a1 = cups(1)
    println(a1)
    val a2 = cups(a1)
    println(a2)
    println( a1.toLong * a2.toLong )

  }
  
  def solveMap( cups : mutable.HashMap[Int,Int] ) = {

    def calcNext( c : Int, pus : Set[Int], max : Int, found : Option[Int] ) : Int = {
      
      if( found.isDefined ) {
        found.get
      }
      else {
        
        val (nextC, nextFound) = if( !pus.contains(c) && c > 0 ) {
          (-1,Some(c))
        }
        else {
          val nc = if( c <= 1 ) { max }
          else { c - 1 }
          (nc,None)
        }
        calcNext( nextC, pus, max, nextFound )
      }
      
    }
    
    var currentCup = 1
    for( m <- 0 until 10000000 ) {
      // println( s"Move: ${m}")
      // println( s"Current: ${currentCup}")
      
      // pick up 
      val c1 = cups(currentCup)
      val c2 = cups(c1)
      val c3 = cups(c2)
      
      // println( s"Pickup: ${c1} ${c2} ${c3}")
      
      // 
      val dest = calcNext( (currentCup - 1), Set(c1,c2,c3), 1000000, None )
      // println( s"Destination: ${ dest }")
      cups(currentCup) = cups(c3)
      cups(c3) = cups(dest)
      cups(dest) = c1

      // println( s"Next: ${ cups(currentCup) }")
      // println(" ")
      currentCup = cups(currentCup)
    }
    
    val a1 = cups(1)
    println(a1)
    val a2 = cups(a1)
    println(a2)
    println( a1.toLong * a2.toLong )
    
  }
  
  def calcDest( i : Int, ups : Array[Int], max : Int ) = {
    
    def innerCalc( ii : Int ) : Int = {
      
      if( !ups.contains(ii) && ii > 0 ) {
        ii
      }
      else {
        val nextII = if( ii - 1 <= 0 ) {
          max
        }
        else {
          ii - 1
        }
        
        innerCalc( nextII )
        
      }
    }
    
    innerCalc(i - 1)
    
  }
  
  def nextThree( pos : Int, src : Array[Int] ) = {
    val ret = Array.ofDim[Int](3)
    
    var i = wrap(pos,src.size)
    ret(0) = src(i)
    
    i = wrap(i,src.size)
    ret(1) = src(i)

    i = wrap(i,src.size)
    ret(2) = src(i)
    
    ret
  }
  
  def wrap( idx : Int, max : Int ) : Int = {
    val n = idx + 1 
    if( n >= max ) {
      0
    }
    else {
      n
    }
  } 
  
  def makeArray( src : String ) : Array[Int] = {
    val ps = src.split(",").map( _.toInt )
    ps
  }
  
  def makeNextMap( src : String ) = {
    
    val nmap = mutable.HashMap[Int,Int]()
    
    val ps = src.split(",").map( _.toInt )
    
    for( i <- 0 until ps.size - 1 ) {
      nmap( ps(i) ) = ps( i + 1)
    }
    
    // now add the numbers from 10 to 1 000 000
    for( i <- 10 until 1000000 ) {
      nmap( i ) = i + 1
    }

    nmap( ps( ps.size - 1 ) ) = 10
    
    nmap(1000000) = ps(0)
    
    
    nmap
  }

  def makeNexts( src : String ) = {

    val nmap = Array.ofDim[Int](1000001)

    val ps = src.split(",").map( _.toInt )

    for( i <- 0 until ps.size - 1 ) {
      nmap( ps(i) ) = ps( i + 1)
    }

    // now add the numbers from 10 to 1 000 000
    for( i <- 10 until 1000000 ) {
      nmap( i ) = i + 1
    }

    nmap( ps( ps.size - 1 ) ) = 10

    nmap(1000000) = ps(0)


    nmap
  }

}
