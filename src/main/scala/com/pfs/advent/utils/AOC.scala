package com.pfs.advent.utils

import java.time.{Duration, Instant}

object AOC {
  
  def iTobs( i : Int, sz : Int = 32 ) = {
    val s = i.toBinaryString
    s.reverse.padTo(sz,'0').reverse
  }
  
  def bsToi( s : String ) = { java.lang.Integer.parseInt(s , 2) }

  def lTobs( i : Long, sz : Int = 32 ) = {
    val s = i.toBinaryString
    s.reverse.padTo(sz,'0').reverse
  }
  
  def bsTol( s : String ) = { java.lang.Long.parseLong(s , 2) }
  
  def timeIt( exp : () => Unit ) = {
    val start = Instant.now()
    exp()
    val end = Instant.now()
    println( Duration.between(start,end).toMillis)
  }
}
