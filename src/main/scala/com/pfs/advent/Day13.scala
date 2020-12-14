package com.pfs.advent

object Day13 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    Console.out.println("2020 13...")
    val ls = toLines(input)
    ls.foreach(Console.out.println(_))
    
    val estimate = ls(0).toLong
    println(estimate)
    
    val bs = parseBusses( ls(1))
    bs.foreach(println(_))
    
    // println( part1( estimate, bs ))
    
    part2( ls(1) )
    
  }
  
  def part2( s : String ) = {
    
    println(s)
    
    val ps = s.split(",").toList
    
    val bs = ps.filter( !_.equalsIgnoreCase("x")).map( i => Bus( i.toInt ))
    val busToOffset = bs.map( b => ( b.id -> ( ps.indexOf( b.id.toString ) ) ) ).toMap
    println(busToOffset)
    
    def innerPart2( time : BigInt, increment : BigInt, currentBus : Bus, remmaining : List[Bus] ) : BigInt = {
      
      println(currentBus)
      
      if( remmaining.isEmpty ) {
        time
      }
      else {
        
        val (nextBus,nextRem) = (remmaining.head,remmaining.tail)
        val nextOffset = busToOffset( nextBus.id )
        if( ( time + nextOffset ) % nextBus.id == 0 ){
          // move up a bus
          println("move up a bus")
          println(time)
          if( nextRem.isEmpty ) {
            time
          }
          else {
            val nextIncrement = increment * nextBus.id
            innerPart2(time + nextIncrement, nextIncrement, remmaining.head, remmaining.tail)
          }
        }
        else {
          // stay on current bus
          innerPart2( time + increment, increment, currentBus, remmaining )
        }
        
      }
    }
    
    val answer = innerPart2( 0, bs.head.id, bs.head, bs.tail )
    println(answer)
    
  }
  
  case class Bus( id : Int )
  
  def parseBusses( s : String ) = {
    val ps = s.trim.split(',').toList
    val bs = ps.filter( !_.equalsIgnoreCase("X") ).map( _.toInt ).map( Bus( _ ))
    bs
    
  }
  
  def part1( estimate : Long,  bs : List[Bus] ) = {
    
    var t = 0L
    
    def innerPart1( time : Long, found : Option[Bus] ) : (Long,Option[Bus]) = {
      if( found.isDefined ){
        // was found on previous tick
        (time - 1,found)
      }
      else if( time == Long.MaxValue ) {
        (-1,None)
      }
      else  {
        
        val inStation = bs.filter( b => time % b.id == 0 )
        // if( !inStation.isEmpty ) {
        //   println(inStation)
        // }

        val nextFound = if( time >= estimate ) { 
          inStation.headOption 
        } 
        else { 
          None 
        }
        
        innerPart1( time + 1, nextFound )
      }
    }
    
    val res = innerPart1(0, None ) 
    if( res._2.isDefined ) {
      println(res)
      val b = res._2.get
      val wait = res._1 - estimate
      println(wait)
      wait * b.id
    }
    else {
      -1
    }
  }
  
  def earilesBus() = {
    1
  }
  
  def waitMinutes() = {
    1
  }
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """939
      7,13,x,x,59,x,31,19"""

  val test2 =
    """939
      67,7,59,61"""

  val test3 =
    """939
      17,x,13,19"""
      
  val input =
    """1000299
      41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,971,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,23,x,x,x,x,x,29,x,487,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19"""

}
