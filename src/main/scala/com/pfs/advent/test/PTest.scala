package com.pfs.advent.test

object PTest {

  def main(args: Array[String]): Unit = {
    
    // n * (n -1)
    val as = ( 'a' to 'h' ).toList
    println("combinations of 2")
    for( c <- as.combinations(2) ) {
      println(c)
    }


    // n!
    val as2 = ( 'a' to 'd' ).toList
    println("permutations")
    for( p <- as2.permutations ) {
      println(p)
    }
    
  }

}
