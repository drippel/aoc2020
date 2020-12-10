package com.pfs.advent.test

object MTest {
  
  case class Person( name : String, gender : String, salary : Int )

  def main(args: Array[String]): Unit = {
    println("mtest...")
    val ps = List( Person("Joe", "M", 101 ), Person( "Lucy", "F", 102 ), Person( "Bob", "M", 103 ), Person( "Alice", "F", 104 ) )
    val m = ps.groupBy( _.gender )
    println(m)
    val m2 = ps.groupMap( _.gender )( _.name )
    println(m2)
    val m3 = ps.groupMapReduce( _.gender )( _.salary )( _ + _ )
    println(m3)
    
    val l = List("a")
    val l2 = l.padTo( 10, "b")
    println(l2)
  }

}
