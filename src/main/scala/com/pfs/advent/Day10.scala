package com.pfs.advent

import scala.collection.mutable

object Day10 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    Console.out.println("2020 10...")
    val ls = toLines(input).map( _.toInt )
    
    // val iss = ls.sorted
    // val (l,h) = (iss.head,iss.last)
    // println(iss)
    // println( s"${l}, ${h}")
    
    val amap = toAdapters(ls)
    // println(amap)
    // println(amap(0))
    
    // val ds = findDiffs(amap)
    // println( ds(1) * ds(3) )
    
    
    // println( amap.size )
    // findSingles( amap )
    println( walk( amap ))
    
  }
  
  def findSingles( as : Map[Int,Adapter] ) = {

    val ks = as.keySet.toList.sorted
    for( k <- ks ) {
      val a = as(k)
      if( a.children.size < 2 ){
        val ps = findParents( a.jolts, as )
        if( ps.size < 2 ) {
          println(s"single: ${k}")
        }
      }
    }
    
  }
  
  def findParents( id : Int, as : Map[Int,Adapter] ) = {
    val ps = as.filter( t => hasChild( id, t._2 ) )
    ps.values.toList
  }
  
  def hasChild( id : Int, a : Adapter ) = {
    val cs = a.children.map( _.jolts )
    cs.toSet.contains(id)
  }
  
  def walk( amap : Map[Int,Adapter] ) : Long = {

    val ks = amap.keySet.toList.sorted
    val start = ks.head
    val target = ks.last
    
    val cache = mutable.HashMap[Int,Long]()

    def innerWalk( idx : Int ) : Long = {
      
      val a = amap(idx)
      if( a.jolts == target ) {
        println("end")
        1
      }
      else {
        
        if( cache.contains(idx) ) {
          println("cache hit")
          cache(idx)
        }
        else {
          val is = a.children.map(c => innerWalk(c.jolts))
          val sum = is.foldLeft(0L)(_ + _)
          cache += ( idx -> sum )
          sum
          
        }
      }
    }
    
    innerWalk(start)
    
  }
  
  def findDiffs( amap : Map[Int,Adapter] ) : Map[Int,Int] = {
    
    val ks = amap.keySet.toList.sorted
    println( s"${ks.head} to ${ks.last}" )
    
    def innerDiffs( ids : List[Int], accum : Map[Int,Int] ) : Map[Int,Int] = {
      
      if( ids.isEmpty ) {
        accum 
      }
      else {
        
        val (h,t) = (ids.head,ids.tail)
        
        val nids = List(h + 1, h + 2, h + 3)
        val as = nids.map( i => (i,amap.get(i))).filter( _._2.isDefined )
        val nextaccum = if( as.isEmpty ) {
          accum
        }
        else {
          val next = as.head._1 - h
          val curcount = accum.getOrElse(next, 0)
          accum + (next -> (curcount + 1))
        }
        
        innerDiffs(t, nextaccum )
        
      }
      
    }
    
    innerDiffs( ks, Map() )
    
  }
  
  def toAdapters( is : List[Int] ) : Map[Int,Adapter] = {
    
    val iss = is.sorted
    
    // root adapter
    val outlet = new Adapter( 0, List() )
    val device = new Adapter( ( iss.last + 3 ), List() )
    
    val as = is.map( new Adapter( _, List() ) )
    var amap = as.map( a => ( a.jolts -> a )).toMap

    amap = amap + ( outlet.jolts -> outlet )
    amap = amap + ( device.jolts -> device )
    
    val ks = amap.keySet.toList.sorted
    
    def linkAdapters( ids : List[Int], accum : Map[Int,Adapter] ) : Map[Int,Adapter] = {
      
      if( ids.isEmpty ) {
        accum
      }
      else {
        
        val (h,t) = (ids.head,ids.tail)
        
        val js = List( h + 1, h + 2, h + 3)
        val as = js.map( accum.get(_)).flatten
        
        val updated = new Adapter( h, as )
        val next = ( accum + ( updated.jolts -> updated ))
        
        linkAdapters( t, next )
        
      }
      
    }
    
    linkAdapters( ks, amap )
    
  }
  
  class Adapter( val jolts : Int, val children : List[Adapter] ) {
    override def toString() = {
      var s = "(" + jolts +" cs:[" + children.map(_.jolts).mkString("(", ",", ")") + "])"
      s
    }
  }
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """16
      10
      15
      5
      1
      11
      7
      19
      6
      12
      4""".stripMargin
      
  val test2 =
    """28
      33
      18
      42
      31
      14
      46
      20
      48
      47
      24
      23
      49
      45
      19
      38
      39
      11
      1
      32
      25
      35
      8
      17
      7
      9
      4
      2
      34
      10
      3""".stripMargin
  
  val input =
    """66
      7
      73
      162
      62
      165
      157
      158
      137
      125
      138
      59
      36
      40
      94
      95
      13
      35
      136
      96
      156
      155
      24
      84
      42
      171
      142
      3
      104
      149
      83
      129
      19
      122
      68
      103
      74
      118
      20
      110
      54
      127
      88
      31
      135
      26
      126
      2
      51
      91
      16
      65
      128
      119
      67
      48
      111
      29
      49
      12
      132
      17
      41
      166
      75
      146
      50
      30
      1
      164
      112
      34
      18
      72
      97
      145
      11
      117
      58
      78
      152
      90
      172
      163
      89
      107
      45
      37
      79
      159
      141
      105
      10
      115
      69
      170
      25
      100
      80
      4
      85
      169
      106
      57
      116
      23""".stripMargin

}
