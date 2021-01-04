package com.pfs.advent

import scala.collection.mutable

object Day24 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    // day20 - lots of grid functions
    val s = "nwwswee"
    val sds = parse(s)
    println(sds)
    val ids = resolve(sds)
    println(ids)
    
    Console.out.println("2020 24...")
    val ls = toLines(input)
    // ls.foreach(Console.out.println(_))
    
    val ds = ls.map( parse( _ ) )
    // ds.foreach(println(_))
    
    val ts = ds.map(resolve(_))
    // ts.foreach(println(_))
    
    val cnt = ts.map( t => ( t, ts.count( t2 => t2 == t ))).toMap
    val sz = cnt.size
    val evens = cnt.filter( kv => kv._2 % 2 == 0 ).size
    println( s"Total: ${sz} flipped back: ${evens} black: ${ sz - evens }")
    
    life( cnt.filter( kv => kv._2 % 2 != 0 ).keySet )
    
    live( cnt.filter( kv => kv._2 % 2 != 0 ).keySet, 100 )
    
  }
  
  def live( start : Set[(Int,Int)], days : Int ) = {
    var gen = start
    for( d <- 0 until days ){
      gen = life(gen)
      println(s"Day ${d + 1}: ${gen.size}")
      
    }
    
    println( s"Final Size: ${gen.size}")
  }
  
  def neighbors( c : (Int,Int) ) : Set[(Int,Int)] = { dirs.map( d => (c._1 + d.row, c._2 + d.col ) ).toSet }
  
  def life( start : Set[(Int,Int)] ) : Set[(Int,Int)] = {
    
    val nextBlack = mutable.HashSet[(Int,Int)]()
    
    
    for( s <- start ) {
      val ns = neighbors(s)
      val bcount = ns.intersect(start)
      if( bcount.size == 0 || bcount.size > 2 ) {
        // dies
      }
      else {
        nextBlack += s
      }
    }
    
    val allns = start.map( neighbors( _ ) ).flatten
    val whiteNs = allns.diff(start)
    
    for( wn <- whiteNs ) {
      val ns = neighbors(wn)
      val bcount = ns.intersect(start)
      if( bcount.size == 2 ) {
        // flip
        nextBlack += wn
      }
      else {
        // stay white
      }
    }
    
    // println(nextBlack)
    
    nextBlack.toSet
    
  }
  
  def resolve( ds : List[Dir] ) : (Int,Int) = {
    
    def innerResolve( is : List[Dir], coord : (Int,Int) ) : (Int,Int) = {
      
      if( is.isEmpty ) {
        coord
      }
      else {
        
        val h = is.head
        val nc = (coord._1 + h.row, coord._2 + h.col )
        innerResolve(is.tail, nc)
      }
      
    }
    
    innerResolve( ds, (0,0))
    
  }
  
  def parse( line : String ) : List[Dir] = {
    
    def innerParse( cs : List[Char], curr : Option[Char], accum : List[Dir] ) : List[Dir] = {
      
      if( cs.isEmpty ) {
        accum
      }
      else {
        
        val h = cs.head
        
        val (nextC,nextAccum) = h match {
          case 'n' => {
            (Some(h), accum)
          }
          case 's' => {
            (Some(h),accum)
          }
          case 'e' => { 
            
            val d = curr match {
              case Some(c) => {
                curr match {
                  case Some('n') => { NorthEast() }
                  case Some('s') => { SouthEast() }
                }
              }
              case None => { East() }
            }
            
            (None, accum :+ d)
            
          }
          case 'w' => {
            val d = curr match {
              case Some(c) => {
                curr match {
                  case Some('n') => { NorthWest() }
                  case Some('s') => { SouthWest() }
                }
              }
              case None => { West() }
            }

            (None, accum :+ d)
          }
        }
        
        innerParse( cs.tail, nextC, nextAccum )
        
      }
    }
    
    innerParse( line.toList, None, List() )
    
  }
  
  // x, y
  abstract class Dir( val row : Int, val col : Int )
  case class West() extends Dir( -1, 0 )
  case class East() extends Dir( 1, 0 )

  case class NorthWest() extends Dir( 0, -1 )
  case class NorthEast() extends Dir( 1, -1 )

  case class SouthWest() extends Dir( -1, 1 )
  case class SouthEast() extends Dir( 0, 1 )
  
  val dirs = List( West(), East(), NorthWest(), NorthEast(), SouthWest(), SouthEast() )

  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """sesenwnenenewseeswwswswwnenewsewsw
      neeenesenwnwwswnenewnwwsewnenwseswesw
      seswneswswsenwwnwse
      nwnwneseeswswnenewneswwnewseswneseene
      swweswneswnenwsewnwneneseenw
      eesenwseswswnenwswnwnwsewwnwsene
      sewnenenenesenwsewnenwwwse
      wenwwweseeeweswwwnwwe
      wsweesenenewnwwnwsenewsenwwsesesenwne
      neeswseenwwswnwswswnw
      nenwswwsewswnenenewsenwsenwnesesenew
      enewnwewneswsewnwswenweswnenwsenwsw
      sweneswneswneneenwnewenewwneswswnese
      swwesenesewenwneswnwwneseswwne
      enesenwswwswneneswsenwnewswseenwsese
      wnwnesenesenenwwnenwsewesewsesesew
      nenewswnwewswnenesenwnesewesw
      eneswnwswnwsenenwnwnwwseeswneewsenese
      neswnwewnwnwseenwseesewsenwsweewe
      wseweeenwnesenwwwswnew""".stripMargin
  
  val input =
    """nenwwwnwsenwneswnwnwnwnwnw
      senwwneeneeneneneenenesweneswenenene
      newnwnwnwnwnwnwesenwenenwenwnwnwwnwsw
      wswnesewwwnwwnewswswnesw
      wnwwwwswwwwswwswswwswnesweww
      esenwneswseeseseseneseeweseseseseswse
      neseesweenweeeewwswee
      senwnesewnwwsenenwsewwewswwnwwswew
      nwesesewswnwnwsenw
      nwnenewnwnwwsenwnwswnwwnww
      eeseeeeweneeeseneweneeeeeee
      nwweeeeeeeswnwneneneeesenenese
      wewwwwwwwwswnwnewswwwwseww
      sweneenwswswneewswnwnwneeneneswnew
      swsenewwsenesewseneswsenwseneswsesenee
      nenwnenwneswsenenenenenewnwne
      wwwwwwwwwew
      nenenenwnesenwnenesewswnenenenwne
      seeswnwsewwnweswneswswseseseseseswsw
      swswwenwnweswwseneneewsenwwwe
      eeneneenewneeewneeseneneeeneene
      nenweenwenenenenesenesenee
      eswweeswsenenwnwnwwseswwnwnwneenene
      nwswnenwnwneenenwnwneweneswneneenwnene
      newneneneeneneneneseenenenenewnwnewne
      swwwswwswwwwwwswneww
      swnwsenwsewnwnwneeswneeswneneswwnesene
      neneneenenwneeswswnenenenenene
      wnwswnwsenwnenwnwwnwewnwwnwwnwnwnww
      seswnwwesenwwnwneswwnwweenwnwenw
      wseseseseseseseseseewesenwseese
      seesesesesenwesesweneseweeesesesw
      ewnewnesenwswnesenwwsenenwenenenenew
      eeenenenenenwneneneswneneneenee
      seeeseeneeeseseseseseeeswe
      seswseseswneswseswswswswswseswswswse
      newwwwwswwswsenenewwwsenwswwww
      nenenenwnenwswenenenenenene
      nwnwnwnwnwnwnwsenwwwnwnwnwnwnwnwnwnesw
      seenewnwnenesewnenesweeneeneneesw
      nenewswenwnwnwwnewnwseneneneswenwswsene
      nwseesenwnenewnwswswsesenesewsesesesw
      sewseseeseseswseswse
      wswwswswsweswswswswsenwswneneseswswse
      eeeweneseeeeeeneneeweeneee
      enesweenwweneneeeseeeeneenewe
      wwenwnenwnwswsenwnenwwwnwweswww
      nenwenwnwnwnwnwneswnwnwswnwnwnwsenwnwnw
      nenwewneeneseswswesw
      nwswenwseswswswswsewswswswseseseneswsese
      nwswsesesenewsweswsweseswnesew
      nwnwnwneneenenwnenenewnenwsenwnenenwnew
      nwewwwswswswwwsenwwesweswswswnew
      sweseneswswswseswseswseswseswnwswswnwsw
      sweswenewneneneneneseneneeenenwnene
      swswswswswswswswnwswswswswswswswseeswnwse
      swseswenwseseseseswswsesewswseswswsese
      enwwwwwseenwewwwnwwwnwwwww
      wwswwwwwnwwwwwnenw
      eeswenwseeesenweeneeswseseeeese
      sweenwwneneenene
      enweswseeseeeeeeenweeeseee
      wswwwswsweswswswswnewwwsw
      nenwswnewnenwnwnwsewnenesenenenenenenw
      swwneweseswswswswswwswswneswswswnesw
      nenwnenwnwnwnenwnwsenenwnwnwnwnenwnw
      wnenenewneenenenwseneneneseneswwe
      wseenwnenwneswseneneenwneswnenwswnew
      enwwenenenesewsweneswseswsewnenewsw
      eweeeeeseneeseswsesenwswsenwweee
      seseeeenwseeeseenwseeeeeeee
      nwnwneneenenenenwnwnwnwnwnwneneswswnenw
      seeeseseseswnwseseneswnwseseswnenwsesee
      swswswswswswswswswswswswseewswswswswne
      wswwwswwswnwwsew
      wwnwwnewwwwswsewwwwwwnwseww
      enwnwswewneneseswnenenwswnwnwenwsenenene
      seseesesewneswnwsewswenwseneswwsesee
      swseswswswsenwswswswswswseswseswnw
      seseseswseeseseeseswnwnewseeesesese
      nwnenenwnewsewnwswenenwenenene
      ewwsesenwneneeesweeeneneeneenenew
      swswwneeneswswswswnwswsw
      neswswwwswnewwseneswnewwnwweswsew
      nwnenewwswnwwwwwesewweswwwsesw
      enweewwseeneeseneseenweseneee
      weswneeneseneenwnwswweeeneeswwe
      enweneeeseseeseww
      nenwswnenwnwnwswnwnwenwnenwnwsenenwnenene
      wweswseesenwsesenwe
      neswnwswnwneneenene
      nwnenenewnwswenenwenwnenwnwnenenwnenw
      eneneewswnwwseswneswsewnenwwswnewwse
      nwsesweeneeweseseseseseenesesesee
      seeseseseeeewsenwsesesenwseesesese
      neneswnenenwneneeneneneswnwnenenwnenenene
      seseswwsesenesesesesesesesesesesenesesese
      seneswesenesewneweseseneseseweeesee
      eeswswswseesenwsenwnweeeneeneesesw
      eeeseeseseesesenwseseseseeseenwnw
      swnwswswenwswsewneswseseswwswseswnwe
      nwneeenenewsenene
      nesesenwneseswseswswwnwseswswseswseseswsw
      sesenesewnesesesesesewseseseseesesesw
      wnesewsewseseswnwswswsenwswwneswnenewne
      eeseeeeeeseesweeeeeenwe
      neswsewnwneswwsewneeseswsee
      neswseseenenwswneneenwenweneneswew
      eeeeswseeeeeenweeeeewene
      nwwnwnwnwnwnwnwnwnwnenwswnwnwnw
      weeeesewseswsenwnwnwneswnwwnwnwne
      weneswswewewswseswswswswwnenwswnesw
      nwewnwenwnwnenwnwnwswnenenwnwnwnwnwnw
      swswswseswneeswswswwswwswswswswswswswsw
      sesewswenwsesewsesewnwnesesesenenwne
      eeeeseesweenweseeeneesweee
      newswsewnenwnwsewsenw
      wswnewwwsewsewwwwswwwsewwnenw
      seenwnwnwesenewseeswsew
      nenwwwwnwnwnwwsewwnwnwwwwwsenew
      eseeeeeeeeneeneeeeeeewseew
      nwneweeswnwnenenwweeweswseswene
      swswwwswneswswswswswseswsweswswnwswswswsw
      nwswesenweeenwsenweewnweseswneee
      swswseseswswnwswswswswswswseswswnwseswe
      wenewnwwwewwswwweesw
      wswswswwwswwnewswswneswseswwwswswsw
      nwswwesewsesewnenenwswwwwwwww
      eeneswnwseeeeenwweneneswnwswne
      wwwswwwnwwewwwnwwwwweswww
      sesenwseseseeseswnwsesewsesesenesesese
      seseswnwswswswswsesweswswnwswswswseswswsw
      enwseseswwseneseeneeseneseewseseew
      nwsenwnwnwnewnenwwseswnwsenwww
      ewneeeneweeenenweeneeeswenene
      swswswswwswneeswesesewneswswnwnewswsw
      sewseswnewseneswwnw
      wwnwnenenwnweeneneneenwnenenesw
      eseeneneneeswnweswesewsewneswwnene
      nwnwwnwnwnwnwwnwenwnwswnwnwswnwwnwe
      nwsenwseswwewnenwnewnwnwsenesenwnwnw
      newswenwseweswwswswswswswneswnwsesww
      swwswswseswwswnwsweswnenewwswswswsw
      neneneeswneneewneneneeneeeneswenenw
      eneeneneeeseweeneene
      swseswseseswwneseseswswsweswseseseswsw
      eeseeeseeeneseeeeeswseee
      eswnwswwswsenwseswswwswseswswneeswsenwsw
      swwnwnwswwseswwswwswenewswswwwswsw
      seseswseswseneseneeseswseswsesewswswsw
      nenenenenenwneswnwnwene
      wwwswneweswswwwseswswneswswswswwsw
      wswwswneneswwswwsenwnenesewwwswse
      nwewwwwwnwnwnwenwsenwnwwnweswswsw
      seseeeeeeseseswsenwsenwewseseese
      seenwsewseseseseeseseswseswseswsesenw
      nwswewwseneneseswseswnwswsenwsenwswnw
      swswswswswswneswnweswswswswnwswswswwsesw
      eeseeeseseswseweeesenwseeeeese
      wnwwwwwwwwwwenwnw
      eseseseseswswwneneseneeeswenesesee
      swneswnwnewwneeneseseneneneewnenenw
      weeeeeenweeeewswneeeeeese
      eeenwsweneneesweneneeeeeeee
      sewwewwswnwwswswesw
      newswneneneneeneeswnew
      nenenwnwwseswnenwnwnenwnenwseswneewnenw
      nenwnwwnwnwnwsenwnwwnwnwnwnwsenwnwnwnw
      wwnwnwwswwwnwwnwnwneewnwnwwsenw
      sweswswwneneswesesenwsenwwswseswsesesese
      wseseswseseseseseseseesenenwseseswwnwse
      wnwnwswwnwnwwnenwwnwwseenwwwwnwsew
      wenwseeseseswenwnweeeswneseesese
      eenenwewewneneneeswe
      swswseswswswnwseeswnwswswsenwswswsenesenw
      nwwnwseewnesweeeeeseeeeeeswee
      neneswnenenenenenenwenwwseswnenee
      swswswewswwwswnewsewswswsw
      nwnwwnwnwnwnweeswsenwnwnwnw
      swseseswneseswseswseseseseswseswsesw
      swsewsenwswswseseswswswswsweseswswswnw
      sesenwnwnwnenwnwnwnwnwnwnwnwnwnwnwnwnww
      swnesweswswswswswnwwwswswswswwswwsesw
      seeeeeeseeseeesweeswnweseswnwenw
      neesweeeweeewneeeseneeee
      nwwsenenwsenenwwnwnenwwnwnwnwsenenwnenene
      seseeseeseeseeneenwseweneswwnwse
      sewseseseseseesesesewseseseswseenesese
      wnwnesenwwwnenwwneswswwnenwwwwwsw
      nwneenenenenenwneseneseneneneswnenwnene
      wnenwseewwwwwwnenwswwwswnwwnese
      eneewwwsewwnenwnwwnewswnwswsese
      sweseswseeneswnenwneseesewewnwwse
      eeneeenewweeseseseseseeseseseeee
      wnwwnwnwnwwwwsewnwwenwnwnwwwnwne
      nwnwnwnwnwnwnwnesewnwnwnwnwnwnwewnwnwnwsw
      nenwwneeenenwnwnenwnwwswnenenesenenwne
      ewneeneeseweenweeeseeenweenesw
      enwweeseesweenwnesweweeeee
      seeeseseswseseswswsesewseweswsenwswsene
      eneeweeeeeeseswseeeeswenwee
      swswnwseswswsenwnwnwewwswswswwnwswese
      esenwnwseeeswswenwnwseneenwsenwswsene
      nwnwwnwnwnwnwnwsenwnwww
      swsesenwseseneswswesesesesenwseseseswsese
      eseswswseenwseseseseseseswwse
      wneenwnwnwnwnwnenenwswswnenwnesenenwsw
      seseeseneeseeseeeswneeswsee
      wswwswnewswnwweneseewwwnwwwww
      nwwwwwwnewwswwwwweeswnwwsw
      sewwwewwseewnewswwwnwnewnww
      weseseseneswesenwsesenwswseseswswsenene
      nwewnwswnwnenwnwwwnwwweewseswnww
      nenenwswwnwswnwnwsenwwnenwwenwnwsew
      wseeeseseseeeneseseseswseenwseee
      sesesesesesewseseeseseseseesesesenwse
      swseeseeeseeeeseswseweenwnwneee
      nweswnenwswnwnwweneswswnwsenwnweswnwnenw
      newwsewnwnwnwnwnw
      wwwswwswwwswwwswwnweswwswwe
      wswneswneseseseswsesesenwseseeswnwswsesesw
      nwnweneseesesweeneneeneswnewwnew
      enwnwwnwnwnwnwnwnwnwnwnenwwnwsesenenwnw
      nwwnwwnwwnwnewnwnwsenwnwwesenwnwswnw
      neenwewewesweneweeeseswnwee
      nwneseseseesweeeeeseweesesesee
      nwswenwswnwnwnwnwnwnesenenwswnenwsenwnw
      wwwwwswsewenwwwswnesenwwnwwnw
      wneswnwnwwwwwnewsewwwwww
      swnenenewswwsenwseseswnwseseswseseeseswe
      wewwwnwwseewwnwwnenwnwwwww
      swwwwwwwwwwnewwwweewww
      eeeneeewseweseeeeeeeewe
      wnwnwnwnenenenenenenwnesenwnw
      nenwswwnwsweneeseweneesw
      nenenewnewneeneneeneeneneneene
      seseseeseseseenwseenwseseeeseeesww
      nenwwsenwsewsewnwnwnwnwwnwnw
      seneeseseseseweeeeweseswse
      wswswswsweseseswsenwsese
      wnewnweenewsewswnwsenwwswswnwnwnese
      wwsenenwnewwwwwwswwse
      nenwswswswsewseseenwnwsweswswswsenwe
      wnwseeswseenwwnenweseswswswswnesww
      nenewneneeneseswnenesewnwnesenenenenwnwnw
      wenwnenesenwnenewnenenewnenesenenenwne
      swswnwswswswseswswswswswswswswswnwseswe
      sesesesesesesesesesenwseeseswsewsenenese
      wnwwweewwwewwswwwwwwwne
      weenwseeeeweswwneesewwwene
      wwswwwwwwwwwnwwwnwwnew
      swswseswswswnewswsewnenewswswswwwwsww
      wewwwnesenwswenwwswwwenweswnewsw
      eneneseneswnwneseneswneswenenenwneswwnwne
      eneseeneneneeweneneneneneneene
      swsewnwewseeneseeswseweenwne
      sesenwwseeswswseseseswsweswseswswswswsw
      neneeneneneneneneneneneswneswnwneneneene
      seseesesenwsesesesesesesesese
      neneenwnenwnwnwnewnwenenwnenwseneneswnw
      neeweeewneeeeeeeeeseeesee
      nwwwswswnwewnwnwwwnwewnwe
      wwwwwnewwswwwwse
      seswneneenewwnenenenenwnenesenenesewne
      wsesenwnwswneeswswseseswsweseenwswnwsw
      wseseneneswnenenwwnenwnwnwnewnwnwnwsenw
      seseseseswseseseseneswsesesesesenwsesese
      eeswswswnwseswsewnwswnwswnwswe
      wswswesewswweswswswwnwnwnesw
      neeneeneeewnewseeneseeneneeeee
      seesenwnesenwseseseseneswsesesenwsesesesw
      neswswwsenwseseseseswseseneseswnwwswnese
      nenesenwneneenenenewnwnenenwnwnenenenew
      wwswswwswnwswwswwswwwswseswsw
      wnwsesenewsewwswswnwsweswswenewswnesw
      eeeseswneesweenweenw
      senwswenesewnesenwswswweswswswseswnee
      wnwwwsewnwnwwswnenww
      neneneswswnenenenenweswneswneenenenenwne
      seseseseseenwsesee
      nwnwneneswenwwwsenewnesenewseswwse
      neswseweewenwewwwnenwsenesesesee
      wsewnenewewwwswswnwswwwewnwwwsw
      nweeeseweswseseewneeeseeenenwe
      wswnwenwenwwnenwswseswenwwnwnwesenwne
      seneswswneseseseswwseseseseseseseseseswse
      ewnweseseseseneeesewneewsweeneese
      seneneneneneneewnenewsenenewwsesew
      wwnwswwswsesweswwwwwswswswswnesw
      nwswnwswnwnwnwenenwnwnenwwnwnwenwnwnw
      seeneseeeeneneeneneneweneeenwnwe
      nenesewnwnwnwseswsenwnwsweswwwsenenwne
      seseseeweeeseseeeneseneseseesesesw
      nwneeneneneeseneeeneneenenewnesenee
      sesenwneswseswswswenenwswnwswswswswswsw
      nwwwnwnwsenwwwnwnwwnwwnwnwewnwsw
      nenenwseneswnenesenenenenwnenenenenwnwwne
      swswswswwweswwwswneswswwwswswswsw
      swswwswswwswnwswneseswswnewswwseseswswsw
      nwsenwswsweseseseseseseenwseswswseswsesesw
      ewnwswswneneswneseneeswswwnwseneswne
      wwwwwsewwwwsewneewnwwwww
      sewneswseseswnweeseeeeeseeneesewse
      seswneswswwswswseneswse
      nwneswnwnwnwnwnwnwnwnwenenwne
      wwewwwwwwwnwweseswswwwwnwsw
      eeswseneenweeenwnweeseeeenenwsw
      swswwwnwwswnwwwewweswewnwnwsewsw
      nwwsenwnwnwwnwwwnewesesw
      wwwnewnewwwnwwsenwsewwwwwww
      swsewswswseswsewseeneswseneswseswswswsw
      nwnenwnwnwnenwsenwnwenwnwnwswswnwnwwnwnwnw
      sweeswnewwwnwnweseneswsewnesewswnw
      swneseseeswseneswnwsw
      swweswnewswswwwswseneswnwwswnwsesesw
      eenwnwnenenewswswnwneenwnenwnwswnwne
      eseseseseeseswseeseseseneseeseseenwew
      swsweeneswswswnwwswswswwswwnwswsweswsw
      swsweswseswseswnwsesenewnenwswneswswswsw
      seswsesenweseseeeseenwseee
      seseeseeneeseseswsenweseewsesesesee
      swnwnwnewnesenenwnenesenwnwnwwneneenwnw
      seswswsesenwswseseseneswseseweenwswsw
      wnwnwnwnwnwnwwwnwwnenwnwnwsw
      nwnwswnesenwneewnwnwnwnweneseneeww
      seswwswneswswseswswswneseswswswswswswsw
      newwnwsesenwnenenwnenenwwenwsenenwswnwe
      eenewneseesweenwneesenwwsenweese
      seewswwwnwwseswswwnwswenenewnew
      seseseseseeewnenwee
      eeeeswwneneweneewenenweewe
      wsenenwswenenwesenwneswsenesenwneswnw
      nwnenenenenenenenwneswnwnenewswnwenee
      newswseseenweesenweswsewwseeenenw
      swsenenwnwnesewwwnwwwnwwsewnewnww
      neneneneneneneenenwnenwweneswnenewne
      swswseenwwsweswswswswswswewenwswwsw
      neeeeneneenewnewneneneseeenene
      senwnesenwseswswsesesesenesewwswseseswne
      wwsenewswewnwwwwwwnwwwswswswww
      eswwwwnenweweswewwwswnwnwsww
      nenwnwwnwnwsenwnenwnwsenwwnwnwnwnwnwesw
      eneseeneeneeseeneneeewnewneee
      eenenwnwneneneeswnweswseeenwneeesw
      senwseswswswnenwnwswwseesweeseswswsw
      swwnewswsenwwwwwwwnwsenwnwwnewnew
      nwnwnwnwnwnwseswnwnwnwnwnenwnenwnwnwnwnw
      swnwwswsenwnwwnwnwwnwenenwnwsenwnenwne
      nenenwnenewewnenwneeeeswswneese
      eeneeneeneneneeesweenwneneeswee
      swseswswnwseswseseseswswseswswswswneswsw
      wwewwwenwewnewwswwwswnwww
      wwnenwwsenwnwnwwwnwwwwwenwnwnww
      nwnenenenwnwneenenwnenwwsenwwnwnwnwnene
      swswsweswwwwswwwwwwsw
      eeneeeneswenenweenenene
      eseeswnwswneeeseenwsewseeeenwnwe
      seseseseseswsesesenesesesesewwwseesee
      esweeseseseseeenwsesesenweeeswe
      wwnewwenwnwsewswwnwnwwnwwwnww
      nenenesenenenenenenewnwnenewenenenene
      nweeeseeseweeenwnwsweeeeee
      senwnwnwnwwswnwenwnwwnwnwnwenwneswnw
      nwnwswnenenwnwsewsenwswsewsewnwnenenwnene
      wneewwwsenwewnwnwesewewnwwwsw
      swswneswswswwenwwseswseswnwswswseswswse
      swwswswswswswneswseswswswwswswneswsww
      wwseswswswswneswswswswswswswswswnwsww
      newwwsenewswswswwsw
      neneenenweneneneeneneenwneneswseswne
      eeesesesesesewseseenew
      weweneswsweeenenwewneeswseenene
      swnewneneseeeeweneeenwnenwweswsw
      seswsenwseesesenwseswsesesese
      eeweeseeeeeeneeee
      nwnwnesenenenenewnenwnwnwwene
      neseswswseeswseswswswseseneswnwseswsesw
      nwnwenwnwwwwnweweewwsewswswwnwe
      nesesenwewenwseseeseswseswseneeese
      ewnwwsenewenwwweewwnwwnwwnw
      nesewnwnwnwswwwswnwwnwsenwnwwnwnenewnw
      sewsesesewswseseneesesesesese
      newnwnwsesenenenwsenenwwwnwsenwnesenwnwse
      esesenwesesenweseseseseseswswseesesese
      sewwwswswneswwnewwsweewwwwnwsww
      swsewwwswwwswnwwnwwwwwewwww
      nenwswneneneneenwneewnesesenenenenenee
      nwnwnwnwneswsenwnwnwnwnwnwnwwnwenwnwnwnwnw
      eeeeesweeeeewseeeneneeswe
      nwnwnwnesesewnwnwnwwnwsenwwnwwnwnwnwne
      nenwneeneneewneeneeneneneswneeee
      seneswneneeswswswseseswseseseenwwswsesw
      swnesweswseswsenwsenwewswsenwnesenwse
      seseseeswsesesesewsesesenesesesenwsenesese
      wnwnenwnwwnwwwwnwseewwnwwewsew
      nwnwwnwnwnwwnwenwnwwnwnwnwnwnww
      sewsenwnenwnewnwnwnenwnwnwnwwnenwnwenw
      nwewwnesewwwwwsenwwwnwnwwwwsww
      swnwswswwswswsesewswswwwewnwswswsw
      swseneseseseswseswseswwswseswsesw
      swwnwwswnwnenwnwnwewsewewwnwwnw
      nwnwwnwnwwwwnwsesewnwnwnwnw
      wwnwwwenwnwseswwenwnwnwnwenwnwnwnw
      nwnwnenwnwwnwnwneesenwwnenenenwseswne
      swneenwswwsenwseswswswsenweseswneswseswse
      swsweswsewsweneswswswsenwseswsese
      eseeswnweeeseeeseswneesesesesesee
      nweswwewswnwewnwwnewsenwswwwwnw
      nwnenenwswneeeswewseneswneswseeenw
      wnwwswwnwnwswenenwwenwwnwnw
      eeeeneeeswnweeswenweeneeene
      swseeeweswnwneewwswnwswswesenwswnw
      seseswsesweseswsewswesesenwseseswsesw
      wnwwwnwnwwsenwnwnwnwnwnwwwwnenw
      wswnwwwwswwewswseswwwwneneseswww
      neseeeneenenwnweswene
      wwwwwwwwnewwwwswnwesewwe
      swnesewenesesewneweswnwswenw
      wsweeenwseswenwsenenweenwsee
      neneenwnwnwsenesenenenenenwnewsenwseswnesw
      wnwnwnwswnenenenwneneneneneenwsenenenenw
      swswswswswseneswswswswswwswswswswswnesw
      sewnenwneneneenenenwnwnwwnwnenenenenwne
      swwnwswewswswnwswneswswwswswswwsesw
      wseenwseesewseesesesene
      wsenweweeneeeneeneewseeeenee
      wswswwwswswwswswsweswwswnewswewsw
      wnewwwwnwseweswwswwwswwswwww
      wnwswnwseswswwswwwewewwswnew
      senweeeeeeewsweneeneeeeee
      sweeseeseeneswenwenenwnwenesenenwsw
      seenweseseseseseseseseseseesenewneswsese
      ewnweneeseeneeeeeneeeneenee
      nenwwsenwenwswnesewnwnenenesewswnene
      sewseswswswseseswswsenwseseswsesesesene
      nesenenwswswseswsenwneswswswseewwsew
      wnwnwnwnwwseeneneeswnewenwnwnwsenwne
      neneeneneswwneeenwneenee
      esenwenewneeweneneesesweesewe
      nenwnwnwnenenwnwnwnesenwnewnw
      nwenwswenwnwnwnwswenwsenwnwnwnwwnenwwnw
      wnenesenenenwnesenwnwnenenesenene
      eseeeeseewenenewnwsweeneenwee
      swswnewneseswseswwseneneneneswwswneswne
      enwenwwwswnwsenewe
      enwwswwseswwww
      nwnwswswnenwnenwnenwnwnwsenenenenenenenenwe
      wswwneswsenwnwnewwsweew
      seswseseseseeesesenweesesewesesenwe
      newswswswnenenwnwenenenwsenenwswnwwe
      nwneneeeneneneeswwenenenweeesesene
      wneneneseeneneeswwneneneswnenenenenene
      nwneeneneewneneneneneneneneswnenene
      nwnwneswnenwenewenenwnenewnwnesenene
      nwnenwenwswsenesesenwseneenwnwnwwnwsw
      swneweswswwwwswwwweseseneeww
      nwwnenesesenenenenenwnenwnewneneenene
      sesenwswseseseseseswenwseswswseswsesese
      newneeneneeweseneewneneneweeee
      newnwwswswwwswswwewesewwwwwnw
      nenwenenenewnwnenwnenwsenenenewnwnenwne
      nwnenwsenwnenwnwnwnwnenwnenenesenewnwnene
      wnweswnwneenwwwsenwnwsenwenwsenesesw
      enesenesenwwsweswneneswnenesenewswswnww
      nwnenwnesenwnenenenwnenenwseswnwnwnewnwnw
      nwswseneseseseswseseswswsesewnwswseseesw
      nwnwnwnwnwwsenwnwnwsenesesewnw
      neneneeneneeneneneenewnenenesenenenesw
      wsenenenwnenwnwnwwnenwnwsenwneenwnenww
      swwsenewneswseswnewww
      wswnwswswseswswswsweseneneseeswswnesew
      seneenenewswneneneenenenenenenenenene
      wnwnwnwsewesenwwwnewwwesww
      newswsewweenwewwnwswneeenwnwswnw
      neneneeswneneneneeeenene
      nwswwsenwwnwnwnweenwnwwewnwnwswnwne
      nwnwnenwneeeenesewnwswswsenwnwnwwnw
      ewnwneswseneeneenweneseesenenenww
      nwnenenwnwenwnwnesewnenewnwswneseenene
      neeweneneeneeneenenewneeeneneswse
      nwneswnwseseswswesewnwwswswswneesewe
      swswneswswsenwwswswwswswnwswswswswese
      swswneswswenewsweneswwwswnwse
      eeeeseeeeeeseewenese
      wewwwseswwwswwwwnwewnwwwww
      senwnwnwnwnwnenwwnenwswnwnwnenwnwnenwnwnw
      wwwsenwwswwweweswneswnwswwsww
      newwwwewwwwwwwewwwwwwse
      eeneeeeeweneneeeeeneweee
      seseswneseswnewsewseseseneseneseseswsew
      seeswswseswswswswseswswswseswswwswnwse
      sesewwseseseseseseneseseseesesesenwsese
      swewnwnenewswwewseswesenenwnwwnwwnw
      swenwseswwwswswwswswwwwenwnewsw
      swsesenwesesesenesesenesenwsew
      swwswswwnwswwwswewswwnwswewwwe
      wswseeneswesewneswswwsewenew
      swwsewnwwewwswwwnwwswswsw
      enweeeseneeeeneeeeee
      sweeeseeswenwenweeeeeeeeeee
      sewnwnwswswseeswsweswswsweswswswswsenesw
      swwswwswswwswwneswwswswsw
      seswnewneenesenwnesenwweneeneswew
      wnwesenwsenenweseseseswsesenesewswsene
      swwwwswwsewswwwneswwww
      wswwwswwswswewwwswwwewswwsw
      esewswswswswswswnewswswneswswnwswseswsw
      nwsenwnwnesenenenenewnwnwnenenwnenenenene
      swneneeneeseswnwneswwwneneenenenenenw
      swswswswwnwsweswswswswswesenwswswnesw
      eswswswwwswswnwweswswswwnweswswswnw
      seneneneeeneeneewswneeeenenenee
      swswswswsesweswswswnewswswswswswswnesw
      nwswnwewswwswwnwwneweenwnewnwe
      newseneswewnwseswnesesenwswneswswswse
      swwwwnwwewenenwwseww
      swsewneswseseseseseswnwsesesewenenwese
      sewsesweewseneeeseeseeeweee
      eeeewneseeeeeeswneeesweswne
      nenenwsewsenesesenwwwsenenwnwseneswnenww
      eeseesesesesewsesesesese
      newneenenwnesweseneneenenene
      swswnwweswneseseswnwswseswseseswswswse
      nwswwnwsenweseswswseswswswsw
      nwwnwnwwnwwnwnesewnesewsenewnwwswnw
      swnenwwnenenenenweseswsenenwenewnwswe
      nwnewnwseseswsewwneswneseseeswsesesenwse
      nwwnwwswnwnenwnwnwnwwsenwenwnwnwnwsw
      seeseeesenwweeeneweseee
      nwwwwwwsewwwswnewwwwwwww
      nwnwnwwwnwwwsewwneswnweewwsww
      esenwnenewnenenwnenenwnenweeseesenese
      wnenenenenenenwneenenenesenenenenenenew
      eswneeneeeneeneswnenenwneeeeenee
      wswseswsenewswwnwswwwswswwwswnesw
      swswsesesenewswneseseswswwneswswsw
      sesewswsenwswneenenenweswee
      seswswswswswswswneseswnwewswseswswswsw
      sesenwseseswswseswswsweseswswsenwsesesw
      eswswswseswnwswswswswseseswswswnesewsesw
      enenweeneweneneeeesweenenesene
      esenenwneswsenweswwwnwnenwnwnwwnwwnw
      swswesewswswswnwnwswwswswswweswswwsw
      seeseseseseneenwsesewnesenesewsesew
      eseswseseeswesenweenenweeseeeese
      senwnwswwwwwwseseneswsewswswneswnw
      esenwwwseswseseneenwe
      swneswseseswseswneneswwswswswseswswswnwsw
      swwwneswwswswswswneswesw
      nweeeneseseeeeeenenweeenwesw
      nesenwswnenwnwnwnwsenwneenenwnenenwnenw
      neeeeeswneesewnenenwe
      swsesesesesesesesenesewsesesese""".stripMargin

}
