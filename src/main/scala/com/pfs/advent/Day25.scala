package com.pfs.advent

object Day25 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    // day20 - lots of grid functions
    Console.out.println("2020 25...")
    
    val testCardPK =  5764801L
    val testDoorPK = 17807724L
    val cardPK     = 18499292L
    val doorPK     =  8790390L
    
    val lp1 = findLoopsize(cardPK, 7)
    val lp2 = findLoopsize(doorPK, 7)
    val k1 = findEncryptionKey( lp1, doorPK )
    val k2 = findEncryptionKey( lp2, cardPK )
    println(s"${lp1} ${lp2}")
    println(s"${k1} ${k2}")
    
  }
  
  def findEncryptionKey( loops : Long, subNo : Long ) = {
    
    def innerFind( rem : Long, accum : Long ) : Long = {
      if( rem == 0 ) {
        accum
      }
      else {
        val na = transform( accum, subNo )
        innerFind( rem - 1, na )
      }
    }
    
    innerFind( loops, 1 )
  }
  
  def findLoopsize( pk : Long, subNo : Long ) = {
    
    def innerFind( loopSize : Long, accum : Long ) : Long = {
      if( accum == pk ) {
        loopSize
      }
      else {
        val na = transform( accum, subNo )
        innerFind( loopSize + 1, na )
      }
    }  
    
    innerFind( 0, 1 )
  }
  
  def transform( lval : Long,  subNo : Long ) : Long = {
    val s1 = lval * subNo
    val s2 = s1 % 20201227
    s2
  }
  
}
