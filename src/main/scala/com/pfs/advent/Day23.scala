package com.pfs.advent

import scala.collection.mutable.ListBuffer

object Day23 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    // day20 - lots of grid functions
    Console.out.println("2020 23...")
    val ls = toLines(test)
    
    val testData  = "3,8,9,1,2,5,4,6,7"
    val inputData = "1,6,7,2,4,8,3,5,9"
    
    val cl = makeCList(inputData)
    
    println(cl)
    
    solve(cl)
  }
  
  def solve( cl : CList ) = {

    var currentCup = cl.head
    
    for( m <- 0 until 1000000 ) {
      
      if( m % 1000 == 0 ) {
        println( s"Move: ${m + 1}" )
      }
      // println( s"Cups: ${cl}")
      // println( s"Point: ${currentCup}" )

      var c1 = cl.remove( currentCup.next )
      var c2 = cl.remove( currentCup.next )
      var c3 = cl.remove( currentCup.next )
      // println( s"Pick up: ${c1} ${c2} ${c3}" )

      // find the destination
      var cs = List(c1.data, c2.data, c3.data )
      var found : Option[Int] = None
      for( i <- ( currentCup.data - 1 ) until 0 by -1 ) {
        if( !cs.contains( i ) && found.isEmpty ) {
          found = Some( i )
        }
      }

      var destination = found match {
        case Some( i ) => i
        case _         => 1000000 
      }

      var fd = cl.findValue(destination).get
      // println( s"Destination: ${fd} " )
      cl.insert( fd, c1 )
      
      cl.insert( fd.next, c2 )
      
      cl.insert( fd.next.next, c3 )

      currentCup = currentCup.next
    }

    println( cl )
    val one = cl.findValue(1).get
    val a = cl.cw(one)
    val b = cl.ccw(one)
    println(a.data)
    println(b.data)
    
    val l = a.data.toLong * b.data.toLong
    println(l)
  }


  case class CNode( val data : Int, var next : CNode = null, var prev : CNode = null ) {
    override def toString : String = {
      data.toString
    }
  }
  
  class CList( var head : CNode ){

    override def toString() : String = {
      val is = toList()
      is.mkString(",")
    }
    
    def insert( current : CNode, next : CNode ) = {
      next.prev = current
      next.next = current.next

      current.next.prev = next
      current.next = next
    }
    
    def remove( cn : CNode ) = {
      
      // if we are removing head 
      // update head
      if( cn == head ) {
        println("removing head")
        head = cn.next
      }
      
      cn.prev.next = cn.next
      cn.next.prev = cn.prev
      
      // null out next and prev
      cn
      
    }
    
    def cw( cn : CNode ) = cn.next
    
    def ccw( cn : CNode ) = cn.prev
    
    def findValue( value : Int ) : Option[CNode] = {
      
      var found = if( head.data == value ) {
        Some(head)
      }
      else {
        None
      }
      
      var curr = head.next
      while( curr.data != head.data ) {
        if( curr.data == value ) {
          found = Some(curr)
        }
        curr = curr.next
      }
      
      found
      
    }
    
    def toList() = {

      var nl = ListBuffer[Int]()

      val start = head.data
      nl += start
      
      var curr = head.next
      while( curr.data != start ) {
        nl += curr.data
        curr = curr.next
      }

      nl.toList
    }

  }
  
  def makeCList( src : String ) = {
    
    val is = src.split(",").toList.map( _.toInt )
    
    val rest = (10 to 1000000).toList
    
    val combined = is ++ rest
    
    val cns = combined.map( CNode( _ ))
    
    val zd = cns.zip( cns.tail ) 
    zd.foreach( t => { 
      t._1.next = t._2
      t._2.prev = t._1 
    })
    
    // link head back to last
    cns.head.prev = cns.last
    
    // and link last to head
    cns.last.next = cns.head
    
    new CList( cns.head )
    
    
  } 
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """
      |
      |""".stripMargin
  
  val input =
    """
      |
      |""".stripMargin

}
