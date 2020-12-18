package com.pfs.advent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day18 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    Console.out.println("2020 18...")
    val ls = toLines(input).map(clean(_))
    ls.foreach(Console.out.println(_))
    
    val ts = ls.map( tokenize(_))
    val rs = ts.map( reduce(_))
    val is = rs.map( evaluate(_))
    
    val sum = is.foldLeft(0L)( _ + _)
    println(sum)
    
    
  }
  
  def reduce( is : List[String] ) : List[String] = { 
    
    def innerReduce( current : List[String] ) : List[String] = {
      
      val open = current.lastIndexOf("(")
      if( open != -1 ) {
        
        val close = current.indexOf(")", open )
        // println( s"${open} ${close}")
        val sub = current.slice( open + 1, close )
        val x = evaluate(sub).toString
        // println(x)
        val next = ListBuffer[String]()
        next ++= current.slice(0, open )
        next += x 
        next ++= current.slice( close + 1, current.size )
        // println(next)
        innerReduce( next.toList )
      }
      else {
        current
      }
      
    }
    
    innerReduce( is )
    
  }
  
  def tokenize( line : String ) : List[String] = line.split(" ").toList.map( _.trim )
  
  def evaluate( items : List[String] ) : Long = {
    
    def innerAdds( is : List[String] ) : List[String] = {
      
      val idx = is.indexOf("+")
      if( idx > 0 ) {
        
        // get the number before and the number after
        val idx = is.indexOf("+")
        val a = is(idx - 1)
        val b = is(idx + 1)
        
        val sum = a.toLong + b.toLong
        
        // h 
        val h = is.slice(0,idx - 1)
        val t = is.slice(idx + 2, is.size)
        val next = ListBuffer[String]()
        next ++= h
        next += sum.toString
        next ++= t
        innerAdds(next.toList)

      }
      else {
        is
      }
      
    }

    def innerMult( is : List[String] ) : List[String] = {
      
      val idx = is.indexOf("*")
      if( idx > 0 ) {

        // get the number before and the number after
        val idx = is.indexOf("*")
        val a = is(idx - 1)
        val b = is(idx + 1)

        val sum = a.toLong * b.toLong

        // h 
        val h = is.slice(0,idx - 1)
        val t = is.slice(idx + 2, is.size)
        val next = ListBuffer[String]()
        next ++= h
        next += sum.toString
        next ++= t
        innerMult(next.toList)

      }
      else {
        is
      }

    }
    
    // reduce the additions
    val as = innerAdds(items)
    
    
    // reduce the multiplications
    val is = innerMult(as)
    
    is.head.toLong
    
  } 
  
  def clean( line : String ) = {
    val a = line.replace("(", "( ")
    val b = a.replace(")", " )")
    b
  }
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val simple = "1 + 2 * 3 + 4 * 5 + 6"

  val simple2 = "2 * 3 + (4 * 5)"
  
  val simple3 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

  val test =
    """1 + (2 * 3) + (4 * (5 + 6))
      2 * 3 + (4 * 5)
      5 + (8 * 3 + 9 + 3 * 4 * 3)
      5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
      ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"""
  
  
  val input =
    """(4 + (2 * 7) * 4) + (6 * 9 + 8 * 4 + 7 * 3) * 3 * 5
      5 * 9 + (5 * 9) * (6 + 2) + 3 + 7
      8 + 5 * 9 * 9 + 9 + 5
      (7 + 9 * 8 + 4 * 6) + 6 * ((9 + 9 + 5 * 7 + 4) * 8 * 2 + 5 * 6 + 2) + 2 * 6
      (6 * (5 + 8 * 7 * 8 + 4) * (7 + 7 * 3 * 5)) + 5 * (8 + (8 + 3 + 5 + 5) + (3 + 2 + 7 * 2 * 9) + 6 * 5 + (2 * 6)) * ((4 * 3) + 3) + 9 * (3 + 6 * 2 + 3 * 8)
      2 * 9 + (6 * 2 + 6 + (9 + 6 + 8 + 5 + 5) + (8 + 7 * 4 + 3) + 8) * (4 + 4 * 2 + 6)
      5 + 4 + 5 + (7 + (9 + 7 + 4 + 4 + 4 + 5) + 2 * 4) + 4 * 9
      9 + (7 * 8 + (6 + 3 + 4)) + (5 * 9)
      2 * 4 + 8 + (5 + 8) * 9 * (9 + (5 + 3) + 3)
      8 * 6 * 5 + 8 + (8 * (2 + 8 * 7 * 5 + 3 + 5)) * 2
      6 * ((3 + 4 * 3 * 7 * 8) * 8 + (9 + 5 + 2 * 3) * 3 * 2) + 7 + 8 * 8 + 4
      9 + 3 + 7 * 8
      (9 + (4 + 9 + 3 + 5 * 5 + 5) * 5 + 9 * 6) * 3 * 2 * 6 * 5
      7 * 9 * 6
      (5 + 2 * 8) * 8 + 3 + 8
      9 * 5 * (5 + (5 * 4 + 8 + 6 * 5) * (9 * 7 + 5)) * 3 * 9
      (6 * (8 * 3 * 7 + 2) * (3 + 9 + 5 + 5) + 3) + 7 * 3
      7 + 9 + (4 + 9 * (4 * 6 + 3 * 8 * 5) + 8 + 9 + 3) + 6 * (6 + 8 + 7 * 9 * 6 + 6) + 7
      2 * (5 * 5 + 6 + (5 + 5 * 5 + 6)) * 4 + 5 + 2
      8 + 2 * (8 * 7 * 4 + 8) * 9
      9 + 5 * (4 * 8 + 9 * 6 + (3 * 3) + 2) + 5 + (4 * 5 + 2 * 2)
      (5 * 7 + 2) + 3 + 9
      7 * 9 + 8
      3 * 7 * (7 + 5 + 8 * 5 * 8)
      (7 + (3 * 6 + 6 * 9 + 2) * 8 * 6 + 6 + 2) * 6
      4 * (7 * 5 * (6 + 8) * 7 * (5 * 3 * 2 * 9 + 2 * 4) * 6) * 2 + 2 * 5
      ((3 * 3 * 9 + 8) + 2 + 5 + 8) + 6 * (6 + 9 * 2) + 7 * 9 * ((9 + 4 + 4) * 4 + 4 + 9)
      7 * (4 * 2 + 3 + 7 + 5) * (8 + 2 + (5 * 9) * 2 + 3 * 5)
      7 + (8 + 3 + 8 * 4 * 2 * 2) + 7 * 2 + ((3 + 5 * 3 + 5) * 8 + 3) + 8
      7 + 2 + 9 * 9 + 5
      ((6 + 4 + 2) * 2 + 9) + 3 * 6 * (6 + 4 + 9 + 2 + 6) * 9
      6 + 6 * (9 * 9 + 6 + 6) + 2 * 6
      2 + 5 * ((4 * 4) * 6) + (3 * 3 + 2) * 9 + 5
      9 * 4 + 8 * 5 * ((6 * 5 * 4) + (3 + 8 * 7 + 6 * 7) * (6 * 6 + 7) * 8 + 4 + 7)
      3 * 8 + (4 + (7 * 7 + 9 * 8 * 6 + 7) * (8 * 4 + 4 * 6 * 4 + 4) + 2 * 9)
      3 + 6 * 2 * 3 * (4 + 7 + (3 * 6 + 7 * 8) * 7 + 9 * 8)
      2 + 8 + (5 * 4 * 2 * 5 + 5) * 4 + 3 * 4
      6 + 4 + (5 + 5)
      6 * 3 + 8 + (8 + 2 + 9 * 3) * ((6 * 8 + 6) + 6 + (7 * 4 * 3) * 4 + 9 + 3)
      ((9 + 7 + 9 + 2 * 7 * 7) + 8 * 6) + 2 + 7
      6 * (6 + 9 * 2 + 4 * 7 + 8) + 5 * 9
      ((2 + 5 + 6 * 3) + 7 + (7 * 6 * 8 * 3 + 8 * 3)) + (8 * 6 * 8 * 9 * 5)
      6 * 7 * 5 + 5 * (2 * 9) * 4
      (8 * 7) * (2 * (7 + 3) + 3) + 8
      (7 * 9 + 7 + 7 + 7 + 7) + (7 * 5 * 8 + 4 * 5) + 6
      5 * (6 * 7 * 8 * 5) * 6
      5 * (2 + (6 * 7) * 8 * 3 + 6 * 2) + 2 * 4 + 8 + 3
      3 * 5 + 6 + 6
      ((9 * 9) * 6) + (4 + 9 * 6 + 4 * 9 + 7)
      (3 + 9 + 2) * (3 * 7 * 7 * 7 * (8 * 8 * 4 + 3 + 8)) + 2
      6 * 6 + 8
      ((9 + 5 + 8) + 9) + 7 + ((9 * 5) * 3 * (3 * 4 * 4 + 3) * 7) + ((5 + 6 + 2) * (7 * 2 * 9 * 8) * 5)
      6 + (8 * 7 * 6 + 5) + 9 * 7 * 7 * 7
      2 + (4 * 9 * 3 * 9 + 8) + 2 * 7 * 3 + 4
      7 + 3 + (5 + 7 * (4 + 4 * 8) + 2 + 8 * 6) * 2
      (2 * 5 * (4 * 7) * 9 * (3 + 7)) + 3 * 4 * (2 * 3 * 8 * 4) * ((6 + 2 * 2) + (6 * 5 * 7 * 6) + 9 * (8 + 4))
      ((9 + 3 + 5) + 8) * 2
      2 + 4 + 7 + 5 * (6 * 7 + 8 * (4 * 3 + 6 * 4 + 7) * (7 + 3 + 7 + 3 * 2) + 8)
      5 + (9 * 8 + 5)
      (2 + (9 + 7 * 8) + (6 + 7) + 6) * 5 + 3 * 2
      7 + (9 + (8 * 7 * 9)) * 8 + 3
      9 * (9 + 4 * 2 * 3 + (6 + 7 * 8 + 9)) + 6 * ((7 * 4 + 7 * 4) + 2 * 6 * 5 + 6 * 6)
      ((3 * 3 + 4 * 9) + 7 + 7 + 3) * 8
      9 + 8 * (2 + 6 + 8 * 6 + 3 + (8 * 6 * 6 * 4 * 6))
      3 * (9 + 2 * (9 + 2 * 8) + 3 + 8 * 5) + (9 + 8 + (6 * 9 + 2 + 8 * 5 + 7) * 7) * 2
      ((2 + 9) + 3) * 6 * 2 * 2 * 4 + 7
      2 * 3 + 4 + (7 + (7 * 5 * 5 + 5) * 9 + (3 + 3 * 5) + 3 * 8)
      4 * (8 + 2 + (3 * 7 + 4 + 2 * 6 + 2) * 8 + 7)
      4 + (5 * 9 * (8 + 3 * 9 + 9 + 5) * (3 * 8 + 9 * 8)) * 8 + 9
      4 * 5 * (2 + 2 + 7 + (3 * 7 * 4 * 2) + 3) * 6
      ((8 * 8 * 4 + 4 + 5) + 6) + 9 * (3 * 3) * 4
      9 * (8 + (6 + 2 * 7 + 3 + 8 * 8) * 3 * (9 + 3) * (3 + 7 * 3) + 5)
      9 * 3
      6 * (7 + 8 * 8 * 4) * 6 * 9 + (4 * 7) * 4
      4 * (3 * (4 * 9 * 6) + 5 + 5) * 4 * 6 * 6 * ((9 + 3 * 6 + 5 + 8) + 4)
      5 * 2 * (4 * 3 + (8 * 4 + 2 + 2) * 6 * (8 * 8 * 2 + 7 + 9) + 3) * 5 + (4 + 6)
      3 + (5 + 6 * 5 * 9) * 9 + 7
      ((3 + 6 + 8) * 7 * 3 + 4 + 4 * 8) * 2 * 2
      2 + ((9 + 9 * 4 + 2 + 3) * 4 + 9 + 5 + (7 + 3 * 9)) + 8 + 7 * 3
      5 * 5 * (5 * 6 * 6 + (3 + 4 * 8 + 7) * 2) * 6 + 4 + 3
      4 + 3 * (5 + (6 * 5 + 6 * 2) * 4) + 9 + 7 * (5 * 5 + 9 * 5)
      2 + (6 * 7) + 7 + 3 * 7
      5 * 7 * (6 + (9 * 5)) * 4 + 2
      ((7 * 7 * 3) + 2 + 2 + (9 + 8 + 3)) * (3 * 8 + 6) + ((6 + 2 * 6) * (2 * 4 + 3 + 8 + 2)) * 8
      ((8 + 3 + 8 + 5 + 8 * 3) * 2 + 4 * (6 + 4)) * 8 * 2 * 5 + 9
      7 + 6 * 3 + 3 + (5 * 7 + 3 + 3 + 8) * 6
      8 + 7 + ((5 * 9 * 3 + 3 + 4 + 6) + (6 * 6 + 2 + 6) + 8)
      5 + 5
      6 + (6 * 5 * 7 + 8)
      (8 + (9 + 2 + 8 + 2 + 2) * 4 + 7 * 6) * 9 * 6 * (6 * 6 * 5 + 5 * 4) + 7 + 9
      3 + 8 + (3 + (6 * 9 + 9 * 2 * 3 * 4) * 3 + 9 * (4 * 2) + (3 * 2 * 9 + 2))
      (5 * 8 + 7 * 8 + (2 + 3 * 8) * (8 + 4 * 7)) * 3
      8 + 2 * (2 + (6 * 6 + 9 * 3 + 6) + (9 * 2 * 4) + 6 * 4 * (6 * 3 + 2 + 6 + 3 + 5)) + 8 + 4 * 9
      8 + 8 + (4 * 9 + 9)
      5 + 5 * (5 + 8 + (3 * 9 * 9 + 8 + 8 * 2) * 5) * (8 + 7 + 7 + 8 + (2 + 7 * 4 * 8))
      6 * 3 + 6 + ((9 * 5 * 4 + 6 + 5 + 5) * 6) * 2 + (6 * 5 + 5 * 7 * 8 * (3 + 7 * 7 * 9))
      4 + (3 + 4 + 5 * (4 * 2 + 7 * 9 + 6 + 6)) + (7 + 7)
      2 * (6 + 2 + 6 * 3 + 4) + (6 + 5 * 4 * 8 + 3 + 7) + 8 + 3 * 5
      ((5 + 3 + 7 * 2 + 9 * 5) + 7 * 7 * (4 + 7 + 9) + 8 + 6) + 3
      ((9 * 9 + 2 * 7 * 5 * 6) + 8 * 7 * 4 * 2 + 6) * 5 * 9
      ((3 * 3 * 2 + 5 + 6) * 7 + (3 * 6 * 2) * 3 + 3) + 9 * 7 + 5
      (7 + (4 + 7 + 6 * 8 + 9 * 9) + 7 * 2 * 2) * 7
      (9 * 3 * 9) + ((2 * 6 * 6 + 9 * 7) * 3) + 3 + 8 + 8
      2 + (3 * 5) * (2 + 8 + 8 * 3 * 6 + 6) * 6 * 9
      (4 * 7 * 7 + 3 + 4) * 3 * 9 + 6 * (2 + 5 * 4)
      (8 * 4 + 3 * 8) * 4 * 5
      (7 + 5 + (4 + 2) + 4) + (2 * 5 * 9 * 2 * 6)
      9 * 4 + 3 + 8 * (2 * 4 + 7 * 2 * 2) * (2 + 6 + 5 + 9 + 5)
      8 + 3 * (6 * 8) + (2 * 4 + 7 * 8)
      (9 + 6 + 3) * 5 + (9 * 8) * 5 + 6 * 5
      ((6 * 2 + 6 * 2) + 3 + 7 + 5 * 8) * 2 + 2
      2 + (9 * 3 * 9) * (4 + (6 + 4 + 7) + 5) + 9
      (3 + 4) + 5 + 7 + 7
      9 + 8 + 8 + 7 + 5 + ((2 + 7 * 9 * 7 * 6 * 9) * 8 * 8 + 7 * 3 + 9)
      (8 * 7 * (8 * 7)) * 9 + (4 * (4 + 9) * (7 * 9 + 9 + 6 * 5 + 5) + 7)
      (6 + 7 * 9 + 5 + 5) * 5 * (2 + 6 + (3 * 6) + (7 + 3 + 5 * 5) + 9)
      2 + ((7 + 8 * 2 + 8) * 4 + 2) * 9 * 9 + 8
      8 * 8 * 7 * 2 + 2
      7 * 2 + 9 + ((5 * 2 + 6 * 9 + 8 + 7) + 6 * 2 + 8 + 4 * (3 * 2 * 3 * 2))
      5 * (7 * 5 + (7 * 9) * 4 * 6 * (8 + 7 + 3 + 9 + 9)) * 4
      8 * ((3 + 7 + 5) + 7) + 5 + (6 + 4 + 6 + 4 * 4) + ((2 * 6) * 3) + 9
      3 * 3
      4 * (5 + 5 + 6 + 2 + 7) + 7 * 6
      (7 + 4 + 5 * 9 + 5 + 8) * 8 * (7 * 9 * 9) + 4
      3 + (9 * (7 * 7 + 3 * 4 + 8 + 5) * 7 * (2 * 4) + 9 + 5) * (4 + 7)
      6 * (5 * (6 * 2) + 6 + (4 * 8 + 9) + 3 * (2 * 4 + 4)) * 6 * 5 + 6 + 3
      (9 * 3 * 9) * 8 * 5 * (8 + 3 + 2 + 9)
      (8 + 8 * (8 * 9) + 2 + 7) + 8 * (5 * 7 + 6 + 2 * 4 * 2) + 6 * (3 * 8 + 6) * 2
      2 * (8 * 5 + 7) * 8 + 7 * 9
      4 * 4 * (5 + (5 * 4 + 2 * 9 + 5 + 7)) * 8 * ((4 * 9 + 4) + 6 * (7 + 2 + 4 * 8 * 9))
      4 * 6 + (4 * 8 * 3 * (9 + 8) * 7 * 9)
      (6 + 9 + 3 * 2 + 8 * 8) * 6
      (8 + (3 + 3 + 2 * 3) + (6 + 7 * 6) + 7 * 4 + (9 * 5 * 4 * 6 + 2)) * 2 + 5
      4 * 6 + 2 * 5 * 2 + (5 + 3 + 2 + (5 * 3 * 3 + 7 * 6 + 2) + 8)
      9 + 2 + (7 * 7 + 7 + 9)
      5 + 8 + (8 + (5 + 9 + 6 * 2 * 4 * 4)) * 9
      (2 + 4 + 8 + 7 * 8 * 5) + 3 + 3 + (9 + 9 + (9 * 2 + 6 + 4 + 4) + 6 * 2) + 2
      7 + (6 + 5 * 2 * 9 + 7 + (9 + 7)) * (3 + 6 + 2 * 5 * 3)
      8 + (9 + (4 * 3 * 7 * 7) * 7) + 9 * 6
      6 * 9 * ((8 + 5) + 9) * 7 * 8
      (2 * 9 + 2) + (6 * 3 * 3 + 6 * 7 + 9)
      7 * 5 * (4 * 4 + 7 * 4 + 8 + 3)
      3 * (6 + (5 + 3) + 5) * 3
      (7 * (9 * 5 * 9 + 2 + 4) + 3 * 8 + (9 + 7) + (2 * 9 + 5)) * (6 + 3 * 9 + 3 + (7 + 4 * 9 + 5) + 9) * (8 * 2 + (2 * 9 + 7 + 5 * 4) * 4)
      4 + 7 + 5 + 6 * 8
      5 + 4 + 3
      9 * (6 * 2 + 7 + 5) * 3 * 2 * 2
      7 * 4 + ((6 * 7 * 6 * 8 + 6 * 9) * 7 * 3) + 8
      9 * 5 + 4 + 8 + 8 * 8
      (3 + 6) * 2 * 7 * (2 * 7 + 2 + 6 + 3)
      5 * 2 + 8 * 7 + (4 + 8 + (3 * 6 + 7 * 2 + 4 + 7))
      6 + (8 + 6 * (9 * 7 + 2 + 2 + 3 + 4) * (4 * 7 + 2 + 4 * 3) * 3)
      (8 + 5 * 2) * (9 * 4) + 5
      3 * (4 * 5 * (4 + 3 + 4) + 5 * (3 + 6 + 3 * 7 * 7)) * 3
      2 + 7 + (8 + 3) + 8
      8 * (9 * 8 * 3) + 4
      (2 * 6) * 3
      ((9 * 6 + 3 + 3 * 9 + 7) + 2 * (2 + 9 * 8 + 2 * 4) + 9 * 8 + 6) + (5 * 9 * (2 + 7) + 7 * 9) + 7
      4 + 7 * ((3 * 7 + 2) * 9 * 7 * 2 + 4 + 3) * 4 * (3 + 8 * 9) + 6
      9 * (9 + (9 * 6 + 2 * 7 + 6) + 7 * 2 + 2)
      (9 * 2 + 4 + 8 + 6 * 8) * (4 + 8 + 7) * 5 + (4 * 2 + 2) * ((4 * 5) * 8) * 2
      (5 * 9 + 9 + 2) * 3 + ((7 * 5 + 9 * 3) + 2 + 3 + 6 + 7) + 4 * 6 * 6
      9 * 8 + 3 * 4 * (9 + 5 + 7 + 5) * (6 * (7 + 4 * 6 * 7) + 6 * (6 * 5 * 3 + 9 + 4) + (2 * 3 + 3 + 6))
      7 * 6 * 2 + (8 * 9 + (6 * 7 * 8 + 6 + 9 * 8) * 3 + 6)
      5 + 7 * 8 + 2 + ((2 + 4) * 7)
      2 * 5 + 6 * 6 * (3 * 6)
      6 * 6 * 6 * (6 * (4 * 4 + 5) * 4 + 3 + 9) + 4
      ((3 * 4 * 7) * 5 * 9 + 7 * 6 + 5) + 2 * 2 * 6 + 4 * 8
      7 * 2 * (3 + (6 * 9 * 9) * 4) * (5 * 8 * 7 * 6 + 2 * 9) * 7
      9 + (6 + 6 + (8 + 8 * 3 + 6 + 6))
      5 * 2 + 7 + 3 * (9 + 9 * 9 * 9 * (4 * 4)) * 2
      4 + 6 + 3 + (2 + 2 * 7 * 2) + 5 + 7
      7 * 9 * 9 * 2 * 4 + 7
      6 + (6 + 5 + 5) + 7 + 4
      9 + (9 * (7 * 3 + 9 + 7 + 8 * 8) * 6 * 5) + 2 * 5
      8 + 6 * 8
      8 * (9 * 6 + 4 + 8 + 8)
      ((8 + 3) + 8 * 2) + (3 * 6 * 7 + 7 + 4 + 2) * 9 * 5
      6 * (4 * 5 + 6 * 7 * 4) + (2 * 8 + 4 + 7 + 7 + 6) + 9 + 5
      9 * 8 * (2 + 7 + 9 + 4 + 5) + (8 * (3 * 8 + 3 * 6)) + ((7 * 2) * 7) + 2
      9 + 4 + 9 * 2 * (5 * 2 + 6 + (9 * 5 + 8 + 2 * 5) + 6 + 6) + 7
      9 + 9 * ((3 * 6) + (5 + 5 + 7 * 9 * 4 + 9) + 2 * 6 * 6) * 5
      (9 * (3 * 2) + 8 * 2) * (6 * (8 + 3 * 8 + 5 * 5 * 9) + 4 + 9 * 6) * 5 + 9 * 6
      7 * 9 * (2 * 9 + 4 * (3 * 2 * 6 * 9 + 5 + 6) + 2) + 7 * 6 + 3
      9 + (5 * (3 * 9) + 4 * 6 + 6) + 9 + 8 * 5
      8 + 8 * 8 + (9 + 4 * (5 + 3 * 8 * 3 * 4 * 4) + 2 * 8 * (9 * 6 * 4 * 6 + 9))
      (6 + 6 * 3 + (5 + 8 * 4 + 4) + 3) * (3 + 7) * (6 + 5 * 4 * 6 + 6 + (5 * 7))
      (9 + 7 + 6 + (8 * 8) * 4 * 6) + 3 * 5
      5 * 2 * 3 + (3 + (6 + 9 * 2) * 3 * 3)
      2 + (8 * 7 * (4 * 8 * 2 + 6 * 7)) * 5 + 5 + (8 * 5)
      9 * 6 * 4 * 2 * ((6 * 3 + 9 * 8) * 5) * 3
      ((8 + 6) + 6 * (8 * 5 * 5 * 7) + (2 + 9 + 6 * 7) * 7 * (2 + 9 * 7 * 3)) * ((7 * 5 + 2 * 5) + (8 * 7))
      (7 + 8) * 6 * 9 + 6 + 6 * 8
      7 * 8 + (8 * 2) * 7 * 8 * 9
      9 * ((8 + 6 * 9 + 2 + 3 * 2) * 2 + 4 + 3 + 7) * 2 * 6
      2 + (8 + (2 + 9) * 7 * 4 + (8 * 6 + 8 * 6 * 4 + 5) + 6) + 4 * (9 + (3 + 4) * (3 * 9 * 7 * 6 + 9 + 7) + 9 * 6 * 3)
      2 + (8 + (8 * 2 + 3 * 8 * 4 + 2)) + 3 * 6
      (5 * 5 + (3 + 5 * 6 * 9 + 4 * 4)) * 8 * (7 * 7 + 3 * 7)
      9 + 6 * (9 * 2 + 2 * (9 + 6 * 8 + 5 + 3))
      5 * 5 + 5 * (7 * 3 * 3 * 5) + 6
      7 * 5 * 7 * ((6 * 3 + 5 * 8 * 6) * 7 + 4 * 5) + 7
      (5 * 4 + 8) * 3 + 6 + 9 + 6
      (7 + 7 + 4 + 8) * (2 * 7 + 9) * 2 * 4
      2 + ((7 + 2 + 9 * 4 + 2) * 6 + 9)
      9 + 2 + (7 + 6 + 4) * 3 * 8
      5 + 7 * 2 + 2 * (3 * 4 + 4 * 6) + 2
      2 + (7 + 2 + (9 * 3 * 5 + 7 + 7) + 5 * (7 + 7 * 2 * 4 * 7 * 4) + 5) * (6 + 2 * 5 + (4 * 6 + 4 * 8 * 4) + 7) * 2 * 5
      ((5 * 9) * 9) * 2 + 4 + ((3 + 6) + (9 + 3 + 6 + 3) * 3 * 5 + (9 + 9 + 6 * 7) + 7) * 2 + 9
      4 + (4 + 4 + 6) + 6 + 5 * 9 + 8
      (3 * 8 * (7 * 5 * 7 * 5 * 7 + 2)) * 7 * 4 + 3 + 2 + 4
      (4 * 8 + 7 + 8 * 5 * 9) + 2 * (5 * 2 * 3 * 9) + 7 + 7 * 2
      6 * 3 * (4 + 6) + 7 + 2
      (5 + 4 + 4 * 8 + (4 * 6 * 5 + 7 * 4 * 2)) * 9 * 6 * 6 * (3 + 5 + 9 * 7)
      4 * (5 * 2 + 8 * 9 * 3) + 3
      7 * 3 + 7 + 5 + ((6 * 8 * 8 + 8 + 2 * 4) * 8 * 2 * 9)
      4 * 2 + 6 + 2
      (2 * 2 + 2 * 9 * 6) + (5 + 9) * 3 * 9 * 5
      (5 + 5 * (3 + 3 + 7) * (3 * 7 * 7)) + 4
      9 + 7 + 9 + 5 * 6
      (6 + 9 + (4 + 7 * 7 * 4) + (9 * 2 + 9 + 5 + 6)) + 7 * 8
      5 + (5 * (6 * 5 + 8 + 2) + (4 * 7 * 4 * 7 * 7 * 8) * 8)
      4 + (6 + 6 * 2 + 9) + 7 * ((3 + 8) * (3 * 5 + 5 * 3 * 3 * 6) + (4 + 9 * 3 + 3 * 2 + 9))
      (7 * (7 + 7 + 5) * 8 * 2 * 8 + 3) + (2 * 4 + 8 * 7 + 3 + 2) + 7 * (7 + 2 + 2)
      (3 * 7 * 4) * 5 + 4 * 7 * 8 * 4
      7 * 3 * (2 * 7 * 9 * 7 * (8 + 4 + 2) * 3) + 7
      7 * (9 + 2 + 5 + 5 + 7 * 5)
      5 + 2 * 9 * (8 * 6 * 8 + 2 * 7)
      4 + (2 + (2 * 5 * 7 * 5) + 3 + 6) + 8 * ((3 + 4 * 4 + 5 * 3 + 6) + 2 + (5 * 8)) + 8 + 3
      2 * (9 * 7 * 3) + (8 + 7 + 8 + 2 * 3 + 9) + 8 + 9
      8 * (7 + 5 * 3 + (2 * 6 * 6 * 4) + 9) + 3 * 4
      3 + 4 * 8 * 9 + (6 + (5 + 7 * 7 * 4)) + 7
      (5 + 6 + 8 + 5 * 7) * 2 + 5 * (9 * (9 * 9 + 8 + 5) + 7 * 4)
      8 + (8 * 2 * 4 * 2 + 3 * 2) * (3 * 3) * 6 + 8
      (6 * 2 * (6 + 5 * 9 * 3) * 6 * 2 + 7) + 7 * 8 + 2 * 4
      5 + (2 + 4 + 5 * 4 + 6) + 9 * (5 * (5 + 9) * 9 * 9 * 2 + 8) * 5
      7 * 4 + 9 * (9 + (5 * 7 + 8 * 7 * 5) + 9 + 2 + 2 + (2 * 4 * 7 + 5))
      7 + 2 + (4 + (8 + 8 + 5 * 9)) + 9
      (8 * 3 * 9 + 6) * 8 + 8 + 7
      (2 + 3 * 9 * 4) * 7 + (8 * 6 * 9) * (8 + 6 + 9 + 4)
      ((7 * 6 + 4) + 6 + 6) + 5 * 4 + 3 * 4
      5 * (2 + (3 * 9 + 9 * 6) + (7 + 3 + 2 * 4 + 9)) + 7 + 8 * 5 * 5
      7 * 9 * (5 + 6 + (7 * 6) + 4 + 4 + 3) * 2 + 9
      (3 * 2 + 5) * 8 * (2 + 4 + 2 + 8 + 7 * (8 * 9))
      ((2 + 7 * 8 * 9) * 4 * 3 + 6 * 7) + 9 * 8 + 5 + 9
      5 + 4 + 9 + 8
      7 + 7 + 9 + 2 + 5 + ((6 * 3 + 3 + 6) * 4 * (4 + 4))
      (9 * 4) + 8 * 4
      3 * 9 * (9 + 5 * 4) + 8 + 9 * 3
      3 + (9 * 8 * (6 + 4 + 6 + 5 * 2) + 9 + (4 * 7 * 7) + (3 + 4 + 6 * 8 + 9)) * (2 + 8) + 7 * 5 * 6
      9 + 6
      6 * 8 * 8 + 6 * 6 * ((9 + 3) + (4 + 2 * 8 + 8 * 2 + 5) * 2)
      4 + (4 * (2 + 2)) + 7 + 4 + (4 * (8 + 8 + 4 + 4 + 2) + (3 + 2) * 9 + 8) + (4 + 9 * 9)
      (2 * (3 * 7 + 6 + 2)) + (4 * 6) + 6 + (6 + 9 + (2 * 2 * 2 + 6 + 4) * 4) * 5 + (4 + 2 * 5 * 8 + 6 * 2)
      4 * ((5 + 5 + 9 + 3 * 5) * 7 * (7 + 9 * 5 * 7 + 2) + 5 * (4 + 7)) + 7 * 9 * 6 * 4
      4 * 7 + 4 + ((4 + 8 * 6 * 3 + 2) + 3 + 8 * (6 * 7 * 2 * 8) * 7) * 9 + 6
      4 * 6 * ((3 * 6) + 4 + (2 * 2 * 2 * 7) + 9 + 8) * (3 + 8 * 2 * 4 * 2 * 3) + (4 + 3 * (7 + 5) + 6 + 9) + 5
      (7 + 2 + (8 + 6 * 4)) + 8 + (7 + 5 + (2 * 5 + 9) * 5)
      (7 * 8 + 5 + 8 + 8 * 2) * 6 + (5 + (8 + 3 + 4 * 5) * 9)
      8 * (4 + 5 * 9 + 6 + 8 * 7) * 3 * 6 + 4 * 5
      6 + ((6 * 3 * 4) + (8 * 8 * 2 * 5 * 8 + 2)) + 3
      9 * 3 + 6 + 5 + ((5 * 2 + 9 * 5 + 6) + (4 * 9)) + (4 * 2 * 3 + 7)
      6 + 8 + (6 + 2 + 8 + 2) * (5 + 8 + 5 * 5 + (4 + 9 + 3))
      (2 * 7) + (6 + 4 + (8 + 6 + 5 + 9 * 4 + 7))
      8 * 3 + 3 * (3 * 5) * 6 + 2
      9 + 4 + ((5 * 4 + 2 + 6) + 5) * 5 + 6 + 4
      (9 * 9 * 3 + 5 * 3 * 4) * 3 + (2 + 3 * 5 + (3 + 6 * 6 + 2) * 8 + 3)
      (6 + 9 * 9 + 7 * 6) + 7
      4 + 6 * (3 * 3 * 7 * 7 * 5 + 2) * (7 + 7 * 2 * 5 + 4 + 9) + (8 * 2 + (8 + 8 * 4 + 7 + 3 + 5) * 7) + 9
      4 + 8 + 6 * 7 * 7 + (2 * 8 + 3 + 5)
      4 * 8 * 4 * (9 * 9 * 5) + 9
      8 * 5 + (2 + 3 + 2 + 8 * (3 + 5 + 8 + 7 * 9))
      3 * ((8 * 6 + 9 * 8 * 5) + 9 * 6 + 6) * 6
      5 * 8 + 6 * (5 * 2 * 8)
      8 + (6 + 7 + 7 * (6 + 7 + 3) * (5 + 2 * 7) + 7) + (2 * 8)
      6 + 5 + 4 + 2 + ((3 + 8) * (5 + 4 * 9) * (4 + 9 + 3 + 9 + 9 * 7) + (5 + 2 + 9 + 3) + (6 + 6) + 3)
      3 * (2 + 5 * (5 * 6 * 5 + 9) + 9 * 5) * 8 * 5 + 4
      2 * 4 + 6 + (9 * 4 * 3 * 4)
      (4 * 6 * (3 + 9) + (3 * 5 + 8 * 5 * 9) + 6) + 9 + 8
      (3 * 5 + 8) + 8 * 8 * 7 + 8 * (9 + 6)
      (5 * 4 * 6 + 3 + 3) * (3 * 8 * (7 + 2 * 9 * 6)) + ((9 * 9 * 4 + 8 * 7 * 7) + 4 * 4) + 8 * 6
      3 + 5 * 5 * 6 * ((3 + 3) * 8 + (6 + 9 + 2 + 6 * 7)) * 4
      9 + ((4 * 7 + 2 + 2 * 6) + 8 * (4 * 6 + 7 * 5) * 7 + 4) + 6 + 6
      (9 + 9 * (8 * 2 + 9 + 5) * 3 * 4 * 4) + 5 * ((3 + 6 * 3 * 7) + 6 * 4) * (3 + 8 * 9 + 6 + (4 + 3 * 4 + 2) + 6)
      5 * 5 * (9 + 8 * 3 * 3 + 3) * 4 + 8
      3 + (6 * 9 * 8) + 9 + (3 + 3 + 2 + 5 + 2)
      3 + 4 * (4 * 8 + 2 * (3 + 3 + 4 + 9 * 7) + 4 + (6 + 9 + 4 * 6 + 9))
      6 * 5 * 9 * ((6 + 9 * 6 + 7) + (6 * 4 + 2 * 4 * 6 * 6) + 9 + 6 + 8)
      7 + 7 * 2 * 4 + 7 * 2
      2 + 8 * (7 + 3 * 3 + 2) + 6 + 3 * 7
      2 * 2 * 9 + 8 + 8
      9 + 5 + 4 * (6 * 7 * 5 * 7 * 4 * 3) * 3
      7 * 5 * 7 * 6 + 4 * 9
      ((9 + 7 + 9 + 7 * 7 * 4) * 6 * 5) + 5 + (8 + (2 + 6 * 9 * 7 * 9 + 9) * (9 * 3 * 8 + 9 * 6 * 2) + 6 * 7) * (2 + 6 + (3 * 4 + 4) * 6 * 5) * 9 * 5
      (7 * 5 * (3 + 2 + 6 * 5 + 9) + 5 + (2 + 9) + 2) + 2 * 6 + (5 * (7 * 2 + 3 + 8 * 9) * 7 + 6 + 6 * (5 + 3 + 9)) * (4 * 6 + (9 + 7 + 4))
      ((3 + 5) + 7 + 3) + 6
      3 * 4 + 4 * 6 + (5 + 8 * 2)
      7 + (7 * (4 + 4)) * 3 + 7 * 4
      (5 + 4 * 6 + 3 * 9 + 7) * (7 * 3 * 5) + 6
      3 + 4 * (8 + 2) + ((6 * 7) + 8 + 8 * (9 * 3 + 2) * 9)
      7 + (7 + 9 * 2 * 9 * 2 * (2 + 5 + 9 * 6)) * 4
      (7 + 6 + 3 + 5 * 2) + 2 * 3 + 4 * (3 + 4 * 5 * (3 + 7) + (7 + 2 * 7 * 6) + 8) * 8
      4 + (9 * 2 * 6 * (6 + 6 * 7 * 7 + 5) * 9) * 9 + (4 + 8 * 7) * 4 + 4
      6 * 6 * 2 + (4 * 2 + 7 + 4 + (2 + 2 * 5)) + 2
      8 + 2 + 3 + 3 + 8 + (6 + 4)
      8 + 9 + 2 + ((3 * 9 * 3 * 5) * 3 + (2 + 2 + 8 + 6 + 9 + 7)) + 9
      6 * (3 * (5 * 4 + 4 * 4 * 4 + 4)) * 8 * (5 + (5 + 2 * 7 + 8) * 3)
      3 + ((3 + 7) + 2 + (5 * 3 + 4 * 9))
      4 + 5 * 6 * (7 * 5 + 6 * 5 * 5 + (4 * 6 * 9 * 6)) + 7 + 3
      7 * 4 * 7 * 5 * (3 + (5 + 6 + 9 * 9 * 3 * 7) * 2 * 8 + 2 + 6)
      5 * 4 + (4 * 7 * (3 + 8 + 9)) + 4
      (7 + (3 * 5 * 7 + 5 * 8) * (6 + 9 * 6 * 7) * (3 + 9 * 8 + 4) + 8 + 7) * 2 + 4
      8 + 7 + (9 + 6 + 7 + 9 + 6 + 4) + (7 * 8 * 6 * 5 * 3) + (3 * 4 * 3 * 8)
      7 * ((4 + 4 + 9 + 3 * 3) + (9 * 6 * 8 * 7 * 3 + 6) * 7 + (7 + 2 * 3 + 6 * 2)) + 3 + (5 * 8) + (4 + 9 * 6 * (2 * 5 + 7 * 3 * 5) * 3 * 8)
      8 + (3 + (8 + 3 * 4 + 4)) * 2 * 4 * (3 + 4)
      (4 + 9 + 4 + (4 + 7 * 3 * 2 * 5 + 7)) * 2
      2 * 8
      6 * (7 * 5 + (4 * 5 * 2))
      (3 + 3 * 4 * 3 + 7 + 4) + 8 + 2 + 4 * 4 * 5
      7 * 2 * 5 + (2 * (9 + 6) * (3 * 8 + 4) + 3) * 9
      (6 * 9 * (6 + 6 + 7 + 7) * (5 * 7 + 8 * 5 + 2 * 8) + 5 + 8) * 9 * 9 + 3 * 2 + 7
      (2 * 3 + 8 + (4 * 2 + 7 * 2 + 7 + 7) + 3 + 6) * 3 * (6 + (9 + 7 * 2 + 2) + 6) + (4 + 3) * 3
      4 * (6 * (8 * 8 + 7) + 7) * 6 * 9 * (4 * (6 * 4 * 3 * 2 + 6) + (8 * 9) + (9 + 3) * 4)
      5 * 7 * 2 * (2 * 2 * 5 * (8 + 6))
      (8 * (7 + 7 + 8)) + 2 + 9 * 9 + 9 * 4
      6 + 6 + 5 + (2 * (8 * 7 + 3 * 6) + 9) * 7 + 6
      8 + 6 + (8 * 4 * 8 * 2 + (6 * 6 + 9 * 3 + 6)) + 3
      9 * 5 * 4 * (5 + (3 + 7) + (4 + 4 * 5 + 5 * 8 * 9) * 5)
      5 + 4 * 9 * ((2 * 4 + 8 * 2 * 7 + 2) + 4) * 9 * 3
      3 + 6 * (6 + 2 + 3 * 2) + (3 + 5) + ((3 * 6 + 5 + 7 + 8 * 8) * 9 + (5 + 8 * 7 + 6 * 8 + 7) + 7 + 8 + 5)
      7 * ((7 + 5 * 3 * 5 + 4) + 7 + (6 * 3 * 6 * 6 * 7 * 7))
      ((5 + 7 * 9 * 2 + 3) * 2 + 9 * 3) * (4 + 6) * (4 * 7 * (9 + 6 * 8 + 5) * (3 + 8 + 9) + 9 * (8 + 6 * 9 + 6)) * 6 + 4
      2 * (6 * 3 + 4 + 4 * 2) + 6 + 7 + 5
      2 + 7 + 5 + 5 * ((7 * 6 + 2) * 8 * 8) + (2 * 5 * 4 * 5)
      3 + (7 * 8 * 5) + 5 + (5 + (4 * 4 + 7 + 2) + 6 + (8 + 3) * 9 + 4)
      6 * (9 + 5 + 6) * 5 + 5 * (7 * (3 * 5 + 6 + 2 * 5) * 2)
      (9 + 6 + (9 + 7 * 5 + 6 * 7) * 7 * 3) + 3 + 4 + 6
      7 * ((5 + 4 + 6) + 7) + 2 * 3 + (6 + 9 * (6 * 4 * 8 * 3 + 4 * 2) + 7) + 3
      7 * (8 + 9 + 5 * (9 * 7 * 9) + 2) + (7 + 7 + (7 + 3 + 2 * 2) + 4 * 2)
      3 * (2 * (4 + 6) * 8)
      8 + ((6 * 4 * 8 + 4) + 7) * 7
      (7 + (2 + 8 * 8 * 6 * 2)) + 7 * 2
      (4 * 2 + 4 * 8 + 4) + (3 * 9 + (2 * 4 + 2 * 7) * 9 + 7) * (5 + 9) * (7 + (3 * 4 * 4 * 6) + 8 * 7) + 8 + 8
      8 + (2 + 2 * 8 * 7 + 7) + 2 + 4 * 3 + (8 + 9 * 9)
      3 + 2 + 8 + (9 + 3 * (3 + 2 + 9 + 7) + 4 * 4 * 7) * 3
      5 * 4 * 7 * 3 + (9 * (6 * 6 + 2 * 4) * 8 + 2 * 9)
      4 * 7 * (3 + 6 * 6 + 4 + 8) + 5
      (8 + (8 + 2 * 9 + 3 * 3) * 8 * 8 * (9 * 6 + 2 * 6)) * 9 + 5 * 2 + 5
      5 + 6 + 3 + 4 * 9 + (4 + (4 + 3 + 9 * 2) + 4)
      3 * (5 + 3) * 7 + 6
      ((3 * 2 + 2 * 6 + 5 + 3) + 4) + (4 * (8 + 2 + 9 * 2) * 8 + (8 * 3 + 6 + 2 + 6) * 2) + 7 + 2 * 9
      6 * 8 * ((8 * 8 + 3 * 7 + 8 * 4) * 4 + 9) + (8 * 7 * (6 * 8 * 3 * 2) * 3) * 2 * 8
      (2 * 7 + 4) * 2 * (5 * 4 + 2 + 5 + (7 + 5) * (4 * 3 + 6 * 8 * 5 + 9)) * 5
      (6 * 8) * (7 * 7 + 8 * 3 * 3) * (5 * 8 + 2)
      (6 + 6 * 2) * 5 + ((7 * 6 * 8) * 8 * 9 * 4)
      (8 * 4 * 9) + ((5 * 3 + 6 * 3) + (9 + 9 + 9 * 2 * 2) * (4 + 9 * 7 + 2 * 2 + 3) + 6) * 7 * 6 * 6
      ((4 + 3) * (2 * 5 + 8 * 2 * 4) * 9) + 6 + (6 + (5 * 4 * 5 * 3 + 3) * 6 * (5 * 3 + 7 * 2 + 4 + 8)) + 4 + 7
      (6 * 9 + 9 + 7) * (6 * 2 * (4 * 7 * 4 + 4 * 5 + 7) + (7 + 5 * 8 * 4 + 9 + 6)) * 7 + 8 * 5 + (9 * 5 * 2 + 7)
      6 * 6 + ((2 + 9) * 5 + 2 * 3 + 3 * 3) + 9 * ((4 * 7) + 5 + 8 + 4)
      5 + (4 * 5 + 5 * 3 + 7) * 6 * 5 + (2 + (9 + 7 + 8 + 4))
      (2 * 4 + 5 + 8 + 6 * 9) + 3 + 6
      9 + (9 + 8 * 3) * (3 + 8 + 6 + 5) * 5
      (3 + 9 * 5 + 7) + 2
      ((9 + 5 * 2) * 5 + (4 + 2 * 7) + 9) * 8
      (4 + 4) * 9 + 3 + 2 * 8
      (3 + 4 * 8 + 9 * 6 + 6) * 5 + 8 + 9
      3 * 8 + 9 * (3 * 4)
      6 + 7 * 6 * 7
      3 * 3 + (8 * 8 * 9 + 3 * 8 + 2) * 3 + 8 * 2
      5 + ((6 + 2 * 6) * 8 + 6 * 5) + ((7 + 3) + 2 * 5 + 7 + 6 + 5) + (8 + 2 * 7 + 4 + 2 * 3) * 2 * 5
      (6 * (6 * 7) * 7 + 3) * 9 + 6 * 3
      (5 * 9 + 7) + 8
      (3 + 9 * 3 * 9 * 2) * 9 * 5
      5 + (6 * 3)"""

}