package com.pfs.advent

import com.pfs.advent.grid.Grid

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day20 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    // day19 - regex goodness
    Console.out.println("2020 20...")
    val ls = toLines(input)
    
    val ts = parse(ls)
    
    ts.foreach( t => { 
      // println( t.id ) 
      // Grid.print(t.data)
    } )
    
    // val as = arrangements(ts.head.data)
    // as.foreach(Grid.print(_))
    
    solve(ts)
    
  }
  
  def arrangements( src : Grid ) : Set[Grid] = {

    // solve(ts)
    val all = ListBuffer[Grid]()

    all += src

    all ++= rcw(src)
    all ++= rccw(src)

    val fx = flipX(src)
    all += fx

    val fy = flipY(src)
    all += fy

    val dp = diagP(src)
    all += dp

    val dn = diagN(src)
    all += dn

    all.toSet
  }

  def rcw( o : Grid ) : List[Grid] = { List( o, cw(o), cw(cw(o)), cw(cw(cw(o)))) }
  def rccw( o : Grid ) : List[Grid] = { List( o, ccw(o), ccw(ccw(o)), ccw(ccw(ccw(o)))) }

  def solve( ts : List[Tile] ) : Unit = {
    
    // find the four corners

    // calc the n x n size
    val side = math.sqrt( ts.size ).toInt
    
    println( s"puzzle side: ${side}" )
    
    val loc = ts.map( t => { ( matchCount( t, ts ), t ) } )
    
    var corners = loc.filter( _._1 == 2 )
    val sides = loc.filter( _._1 == 3 )
    val middle = loc.filter( _._1 == 4 )
    
    val cids = corners.map( _._2.id ).map( _.toLong )
    println(cids)
    val prod = cids.foldLeft(1L)( _ * _ )
    println(prod)
    
    // start with one of the corners and find the sides on either side
    
    val anchor = corners.head
    val size = anchor._2.data.rows
    
    var remaining = others(anchor._2,ts)
    
    // lets put this corner in 0,0 - but we also need to rotate it so that the two non matching sides are north,top and west,left
    val solution = Array.fill[Option[Tile]](side,side)(None)
    
    val orig = anchor._2.data
    val possibles = arrangements(orig).toList.map( cloneTile( anchor._2, _ ) )
    val sideCounts = possibles.map( t => (t, matchCounts( t, remaining.toList ) ) )
    val found = sideCounts.filter( t => t._2._1 == 0 && t._2._4 == 0)
    
    solution(0)(0) = Some(found.head._1)
    
    
    for( i <- 0 until 10 ) {

      // lets move to the east / left
      val right = solution( 0 )( i ).get.data.col( size - 1 )

      val m = matchSide( right, remaining.toList )

      // orient top is non matching left match right of previous
      val lps = arrangements( m.get.data ).toList.map( cloneTile( m.get, _ ) )
      val lpcounts = lps.map( t => (t, matchCounts( t, remaining.toList )) )
      val lpfound = lpcounts.filter( t => t._2._1 == 0 && right.equals( t._1.data.col( 0 ) ) )
      
      solution(0)(i+1) = Some(lpfound.head._1)

      remaining = others( lpfound.head._1, remaining )
    }
    
    // top / right corner 
    
    val right = solution( 0 )( 10 ).get.data.col( size - 1 )
    val m = matchSide( right, remaining.toList )
    val lps = arrangements( m.get.data ).toList.map( cloneTile( m.get, _ ) )
    val lpcounts = lps.map( t => (t, matchCounts( t, remaining.toList )) )
    val lpfound = lpcounts.filter( t => t._2._1 == 0 && t._2._2 == 0 && right.equals( t._1.data.col( 0 ) ) )
    solution(0)(11) = Some(lpfound.head._1)
    remaining = others( lpfound.head._1, remaining )

    for( i <- 0 until 10 ) {

      // lets move to the east / left
      val bottom = solution( i )( 0 ).get.data.row( size - 1 )

      val m = matchSide( bottom, remaining.toList )

      // orient top is non matching left match right of previous
      val lps = arrangements( m.get.data ).toList.map( cloneTile( m.get, _ ) )
      val lpcounts = lps.map( t => (t, matchCounts( t, remaining.toList )) )
      val lpfound = lpcounts.filter( t => t._2._4 == 0 && bottom.equals( t._1.data.row( 0 ) ) )

      solution(i+1)(0) = Some(lpfound.head._1)

      remaining = others( lpfound.head._1, remaining )
      
    }

    val bottom = solution( 10 )( 0 ).get.data.row( size - 1 )
    val bm = matchSide( bottom, remaining.toList )
    val bmas = arrangements( bm.get.data ).toList.map( cloneTile( bm.get, _ ) )
    val bmasCounts = bmas.map( t => (t, matchCounts( t, remaining.toList )) )
    val bmFound = bmasCounts.filter( t => t._2._3 == 0 && t._2._4 == 0 && bottom.equals( t._1.data.row( 0 ) ) )
    solution(11)(0) = Some(bmFound.head._1)
    remaining = others( bmFound.head._1, remaining )
    
    // bottom row
    for( i <- 0 until 10 ) {

      // lets move to the east / left
      val left = solution( 11 )( i ).get.data.col( size - 1 )

      val m = matchSide( left, remaining.toList )

      // orient top is non matching left match right of previous
      val lps = arrangements( m.get.data ).toList.map( cloneTile( m.get, _ ) )
      val lpcounts = lps.map( t => (t, matchCounts( t, remaining.toList )) )
      val lpfound = lpcounts.filter( t => t._2._3 == 0 && left.equals( t._1.data.col( 0 ) ) )

      solution(11)(i+1) = Some(lpfound.head._1)

      remaining = others( lpfound.head._1, remaining )

    }

    // right col 
    for( i <- 0 until 10 ) {

      // lets move to the east / left
      val bottom = solution( i )( 11 ).get.data.row( size - 1 )

      val m = matchSide( bottom, remaining.toList )

      // orient top is non matching left match right of previous
      val lps = arrangements( m.get.data ).toList.map( cloneTile( m.get, _ ) )
      val lpcounts = lps.map( t => (t, matchCounts( t, remaining.toList )) )
      val lpfound = lpcounts.filter( t => t._2._2 == 0 && bottom.equals( t._1.data.row( 0 ) ) )

      solution(i+1)(11) = Some(lpfound.head._1)

      remaining = others( lpfound.head._1, remaining )

    }
    
    val bt = solution(10)(11).get.data.row(size - 1)
    val bl = solution(11)(10).get.data.col(size - 1)
    val bcm = matchSide( bt, remaining.toList )
    val bcas = arrangements( bcm.get.data ).toList.map( cloneTile( bcm.get, _ ) )
    val bcasCounts = bcas.map( t => (t, matchCounts( t, remaining.toList )) )
    val bcFound = bcasCounts.filter( t => t._2._2 == 0 && t._2._3 == 0 && bt.equals( t._1.data.row( 0 ) ) && bl.equals( t._1.data.col(0)) )
    solution(11)(11) = Some(bcFound.head._1)
    remaining = others( bcFound.head._1, remaining )
    
    
    // now lets fill in the inside pieces, we are starting at 1,1 and going through to 10,10
    // matchging all three sides for the last in the row as a checksum
    // and then matching all four for the last 
    for( currR <- 1 to 10 ) {
      for( currC <- 1 to 10 ) {

        // above cell is
        val above = solution( currR - 1 )( currC )
        val aboveRow = above.get.data.row( size - 1 )
        val left = solution( currR )( currC - 1 )
        val leftCol = left.get.data.col( size - 1 )

        val interiorMatch = matchSide( aboveRow, remaining.toList )
        val interiorOrientations = arrangements( interiorMatch.get.data ).toList
                                                                         .map( cloneTile( interiorMatch.get, _ ) )
        val interiorFound = interiorOrientations.filter( t => {
          t.data.row( 0 ).equals( aboveRow ) && t.data.col( 0 ).equals( leftCol )
        } )

        val foundTile = interiorFound.head
        solution( currR )( currC ) = Some( foundTile )
        remaining = others( foundTile, remaining )
      }
    }

   for( r <- 0 until 12 ) {
      for( c <- 0 until 12 ) {
        solution(r)(c) match {
          case Some(t) => print( s"${t.id} " )
          case None => print( "UNKN " )
        }
      }
      println(" ")
    }
    println(" ")
    
    
    val i = solution(10)(10)
    val t = solution(9)(10)
    val r = solution(10)(11)
    val b = solution(11)(10)
    val l = solution(10)(9)

    println("test")

    val interiors = Array.ofDim[Tile](side,side)
    for( ri <- 0 until 12 ) {
      for( ci <- 0 until 12 ) {
        val t = solution(ri)(ci)
        val i = interior(t.get)
        interiors(ri)(ci) = i
      }
    }
    
    // now we need to combine into one big 120 x 120 grid
    val combined = Grid( 96, 96 )
    
    for( ro <- 0 until 12 ) {
      // make a combined row
      for( ri <- 0 until 8 ) {

        val rc = ListBuffer[Char]()
        for( gc <- 0 until 12 ) {
          
          val i = interiors(ro)(gc)
          rc ++= i.data.row(ri)
          
        }
        val ridx = (ro * 8) + ri
        for( i <- 0 until rc.size ) {
          combined(ridx, i, rc(i) )
        }
      }
    }

    // Grid.print(combined)
    
    val combinedOs = arrangements(combined).toList
    // combinedOs.foreach(Grid.print(_))
    
    val mon = makeMonster()
    
    /*
    val msh1 = Grid.parse(mash1)
    val msh2 = Grid.parse(mash2)
    
    println("matches")
    println( matches( mon, mon ) )
    println( matches( mon, msh1 ) )
    println( matches( mon, msh2 ) )
    println("done")
     */

    val monMap = mutable.HashMap[Int,List[(Int,Int)]]()
    var ccnt = 0  
    for( cmb <- combinedOs ) {
      var mcnt = 0
      for( r <- 0 until( cmb.rows - mon.rows + 1 ) ) {
        for( c <- 0 until( cmb.cols - mon.cols + 1 ) ) {
          val sub = subGrid( cmb, r, c, mon.rows, mon.cols )
          val mt = matches( mon, sub )
          if( mt._1 ) {
            // Grid.print( sub )
            val l = monMap.getOrElse(ccnt,List())
            val adj = mt._2.map( t => { (t._1 + r, t._2 + c) })
            monMap(ccnt) = (l ++ adj )
            mcnt = mcnt + 1
          }
        }
      }
      println( ccnt )
      println( mcnt )
      ccnt = ccnt + 1
    }
    
    // 
    val sol = monMap.toList.filter( t => !t._2.isEmpty )
    
    println(sol.head._2)
    Grid.print(combinedOs(sol.head._1))
    
    val monCoords = sol.head._2.toSet
    val hashCoords = charCount( combinedOs(sol.head._1), '#' )
    println(hashCoords.size)
    
    println(monCoords.size)
    println(hashCoords.size)
    println(hashCoords.size - monCoords.size)
    
    
  }
  
  def charCount( grid : Grid, ch : Char ) : List[(Int,Int)] = {
    
    val res = ListBuffer[(Int,Int)]()
    
    for( r <- 0 until grid.rows) {
      for( c <- 0 until grid.cols ) {
        if( grid(r,c) == ch ) {
          val t = (r,c)
          res += t
        }
      }
    }
    
    res.toList
    
  }
  
  // assume that pattern and target are the same size
  def matches( pattern : Grid, target : Grid ) : (Boolean,List[(Int,Int)]) = {
    var m = true
    val ts = ListBuffer[(Int,Int)]()
    
    for( r <- 0 until pattern.rows ) {
      for( c <- 0 until pattern.cols ) {
        if( pattern(r,c) == '#' ) { 
          if( target(r,c) != '#' ) {
            m = false
          }
          else {
            val t = (r, c)
            ts += t
          }
        }
      }
    }
    
    (m,ts.toList)
  }
  
  def subGrid( src : Grid, rs : Int, cs : Int, height : Int, width : Int ) : Grid = {
    val out = Grid( height, width )
    
    for( r <- 0 until height ) {
      for( c <- 0 until width ) {
        out(r,c, src( rs + r, cs + c ))
      }
    }
    out
  }
  
  def makeMonster() : Grid = {
    val ls = monster.split("\n").toList.map( _.trim )
    
    val m = Grid( 3, 20 )
    
    for( r <- 0 until 3 ) {
      val line = ls(r)
      for( c <- 0 until 20 ) {
        m( r, c, line(c) )
      }
    }
    
    
    m
  }
  
  val monster =
    """..................#.
       #....##....##....###
       .#..#..#..#..#..#..."""

  val mash1 =
    """..#......#........#.
       #....##.#..##...####
       .#..#..#..#..#..#.#."""

  val mash2 =
    """..................#.
       #.....#....##.....##
       .#..#..#..#..#..#..."""
       
  def interior( src : Tile ) : Tile = {
    
    val ng = Grid( src.data.rows - 2, src.data.cols - 2 )
    // println( s"${ src.data.rows - 2 } , ${ src.data.cols - 2 }" )
    
    for( r <- 1 until src.data.rows - 1 ) {
      for( c <- 1 until src.data.cols - 1 ) {
        ng( r - 1, c - 1, src.data( r, c ))
      }
    }
    
    Tile( src.id, ng )
  }
  
  def matchSide( target : List[Char], allTiles : List[Tile] ) : Option[Tile] = {
    allTiles.find( t => { edges(t).exists( lineup( target, _ ) ) })
  }

  def others( src : Tile, all : List[Tile] ) = { all.filter( t => { !t.id.equals(src.id) } ) }
  
  def cloneTile( src : Tile, g : Grid ) = { Tile( src.id, g ) }

  def matchCounts( target : Tile, allTiles : List[Tile] ) : (Int,Int,Int,Int) = {

    // get the top
    val top = target.data.row(0)
    val bottom = target.data.row(target.data.rows - 1)
    val left = target.data.col(0)
    val right = target.data.col(target.data.cols - 1)

    //
    val others = allTiles.filter( t => { !t.id.equals(target.id) } )

    val allEdges = others.map( edges( _ ) ).flatten

    val t = allEdges.count( e => lineup( e, top ))
    val b = allEdges.count( e => lineup( e, bottom ))
    val l = allEdges.count( e => lineup( e, left ))
    val r = allEdges.count( e => lineup( e, right ))

    (t,r,b,l)

  }
  
  def matchCount( target : Tile, allTiles : List[Tile] ) : Int = {
    
    // get the top
    val top = target.data.row(0)
    val bottom = target.data.row(target.data.rows - 1)
    val left = target.data.col(0)
    val right = target.data.col(target.data.cols - 1)
    
    //
    val others = allTiles.filter( t => { !t.id.equals(target.id) } )
    
    val allEdges = others.map( edges( _ ) ).flatten
    
    val t = allEdges.count( e => lineup( e, top ))
    val b = allEdges.count( e => lineup( e, bottom ))
    val l = allEdges.count( e => lineup( e, left ))
    val r = allEdges.count( e => lineup( e, right ))
    
    t + b + l + r
    
  }
  
  def lineup( a : List[Char], b : List[Char] ) : Boolean =  { a.equals(b) || a.equals(b.reverse) }
    
  
  def edges( t : Tile ) = {
    // top, bottom, left, right
    List( t.data.row(0), t.data.row(t.data.rows - 1), t.data.col(0), t.data.col(t.data.cols - 1))
  }

  def diagN( src : Grid ) = {

    val g = Grid(src.rows, src.cols)

    val cs = src.toCols()

    for( r <- 0 until src.rows  ) {
      val col = cs(r)
      for( c <- 0 until src.cols ) {
        g(r,c,col(c))
      }
    }


    g
  }
  
  // TODO: move these to Grid class
  
  def ccw( src : Grid ) = {

    val g = Grid(src.rows, src.cols)

    val cs = src.toCols().reverse

    for( r <- 0 until src.rows  ) {
      val col = cs(r)
      for( c <- 0 until src.cols ) {
        g(r,c,col(c))
      }
    }


    g
  }

  def cw( src : Grid ) = {
    
    val g = Grid(src.rows, src.cols)
    
    val rs = src.toRows().reverse
    
    for( c <- 0 until src.cols  ) {
      val row = rs(c)
      for( r <- 0 until src.rows ) {
        g(r,c,row(r))
      }
    }
    
    
    g
  }
  
  def diagP( src : Grid ) = {

    val g = Grid(src.rows, src.cols)

    val rs = src.toRows().reverse

    for( c <- 0 until src.cols  ) {
      val row = rs(c).reverse
      for( r <- 0 until src.rows ) {
        g(r,c,row(r))
      }
    }


    g
  }

  def flipX( src : Grid ) = {
    
    val g = Grid(src.rows, src.cols)
    
    val rx = src.rows
    val cx = src.cols
    
    val xr = (0 until rx).toList.reverse
    val ridx = xr.zipWithIndex
    
    for( c <- 0 until cx ) {
      for( r <- ridx ) {
        g(r._1,c,src( r._2,c) )
      }
    }
    
    g
  }
  
  def flipY( src : Grid ) = {

    val g = Grid(src.rows, src.cols)

    val rx = src.rows
    val cx = src.cols

    val xc = (0 until cx).toList.reverse
    val cidx = xc.zipWithIndex

    for( r <- 0 until rx ) {
      for( c <- cidx ) {
        g(r,c._1,src( r,c._2) )
      }
    }

    g
  }
  
  def startGrid() : Grid = {
    val g = Grid(4,4)
    
    g(0,0,'A')
    g(0,1,'B')
    g(0,2,'C')
    g(0,3,'D')

    g(1,0,'E')
    g(1,1,'F')
    g(1,2,'G')
    g(1,3,'H')

    g(2,0,'I')
    g(2,1,'J')
    g(2,2,'K')
    g(2,3,'L')

    g(3,0,'M')
    g(3,1,'N')
    g(3,2,'O')
    g(3,3,'P')
    
    g
  }
  
  // everything is row,col
  
  def isSolution( sol : mutable.HashMap[(Int,Int),Tile], side : Int ) : Boolean = {
    
    if( sol.size < ( side * side  ) ) {
      // not enough tiles in sol
      false
    }
    else {
      // are the tiles arranged in a square?
      val rs = sol.map( kv => kv._1._1 ).toList.sorted
      val cs = sol.map( kv => kv._1._2 ).toList.sorted
      
      val (minR,maxR) = (rs.head,rs.last)
      val (minC,maxC) = (cs.head,cs.last)
      
      val diffR = Math.abs( maxR - minR )
      val diffC = Math.abs( maxC - minC )
      
      if( diffR == side && diffC == side ){
        
        // make sure all the elements are in the grid
        val bs = for{
          r <- minR until maxR
          c <- minC until maxC
        } yield sol.contains( (r,c) )
        
        bs.forall( _ == true )
        
      }
      else {
        false
      }
      
    }
  }
  
  
  def parse( raw : List[String] ) = {
    val gs = raw.grouped(11).toList
    gs.map(parseTile(_))
  }
  
  def parseTile( lines : List[String] ) : Tile = {
    val ps = lines(0).split(' ')
    val t = lines.tail.mkString("\n")
    Tile( ps(1).replace(":", "" ), Grid.parse(t) )
  }
  
  case class Tile( id : String, data : Grid )
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """Tile 2311:
      ..##.#..#.
      ##..#.....
      #...##..#.
      ####.#...#
      ##.##.###.
      ##...#.###
      .#.#.#..##
      ..#....#..
      ###...#.#.
      ..###..###
      
      Tile 1951:
      #.##...##.
      #.####...#
      .....#..##
      #...######
      .##.#....#
      .###.#####
      ###.##.##.
      .###....#.
      ..#.#..#.#
      #...##.#..
      
      Tile 1171:
      ####...##.
      #..##.#..#
      ##.#..#.#.
      .###.####.
      ..###.####
      .##....##.
      .#...####.
      #.##.####.
      ####..#...
      .....##...
      
      Tile 1427:
      ###.##.#..
      .#..#.##..
      .#.##.#..#
      #.#.#.##.#
      ....#...##
      ...##..##.
      ...#.#####
      .#.####.#.
      ..#..###.#
      ..##.#..#.
      
      Tile 1489:
      ##.#.#....
      ..##...#..
      .##..##...
      ..#...#...
      #####...#.
      #..#.#.#.#
      ...#.#.#..
      ##.#...##.
      ..##.##.##
      ###.##.#..
      
      Tile 2473:
      #....####.
      #..#.##...
      #.##..#...
      ######.#.#
      .#...#.#.#
      .#########
      .###.#..#.
      ########.#
      ##...##.#.
      ..###.#.#.
      
      Tile 2971:
      ..#.#....#
      #...###...
      #.#.###...
      ##.##..#..
      .#####..##
      .#..####.#
      #..#.#..#.
      ..####.###
      ..#.#.###.
      ...#.#.#.#
      
      Tile 2729:
      ...#.#.#.#
      ####.#....
      ..#.#.....
      ....#..#.#
      .##..##.#.
      .#.####...
      ####.#.#..
      ##.####...
      ##..#.##..
      #.##...##.
      
      Tile 3079:
      #.#.#####.
      .#..######
      ..#.......
      ######....
      ####.#..#.
      .#...#.##.
      #.#####.##
      ..#.###...
      ..#.......
      ..#.###..."""
  
  val input =
    """Tile 3557:
      .#...##.#.
      #.#..#...#
      ##....#..#
      .#...##..#
      ..###....#
      ..##..#...
      #.#...##.#
      ..##..##..
      .#.....##.
      #.###.....
      
      Tile 2203:
      ....#.#..#
      ##........
      #.#...#...
      #.#.##.###
      .#....#..#
      ..#.#....#
      #.....#...
      #.#.#.....
      .#...##.#.
      ##....##.#
      
      Tile 3251:
      ...#.#.###
      ##.#...#.#
      .#..##...#
      ....##...#
      .#.......#
      ..#......#
      #..#......
      ##.#.#.#..
      #......#..
      ##.##.#...
      
      Tile 3917:
      .#.#...#.#
      ..........
      #.#.#.####
      #...#...#.
      ..#.##..#.
      #.#.#.####
      ..#....#..
      ###.#....#
      ...##..#..
      ##.#####.#
      
      Tile 2663:
      .####.###.
      ....##..#.
      ......#..#
      ...#......
      #.#.#..#.#
      ##.##.....
      ###.###...
      ..#..#....
      ##..##....
      .#..##.#..
      
      Tile 1093:
      ###.##...#
      #....#....
      #...#..#..
      ..##.#####
      #.....##..
      #....#.#..
      #..#..#.##
      #.#......#
      ...#.....#
      #.##..###.
      
      Tile 1279:
      ..###.##..
      #.....#...
      ##..#....#
      #...####.#
      ....###..#
      #....#..##
      ..##...###
      .##.#.....
      ##......#.
      ...##.#..#
      
      Tile 3881:
      ##...#....
      #..##.#..#
      ...###...#
      ....#...#.
      ###..#.###
      ..#...#..#
      ......#...
      ..#..#.#..
      ...##.##.#
      #.#.#...#.
      
      Tile 3793:
      ##...#.##.
      ...#..#..#
      ..#...##..
      ..###..#..
      ##......##
      ##......##
      #..###.#.#
      #.....#..#
      .#.#..#..#
      .#.##.###.
      
      Tile 3919:
      #####..###
      ##....###.
      .####.#.##
      #..#......
      ..#.#...##
      .##..#...#
      .....#...#
      ..#.....#.
      ...#......
      .##.##.#.#
      
      Tile 3769:
      .#.####..#
      ......#..#
      #......#.#
      .#....#..#
      .........#
      ..........
      #.......#.
      .#.....#..
      #...#....#
      ####.#.#.#
      
      Tile 1747:
      ...#...#.#
      ..#..#.#.#
      .....#...#
      #.##......
      #.......##
      .#........
      #####.....
      #.#..#....
      ..#.......
      ...#....##
      
      Tile 2711:
      .##..#.#.#
      #.##......
      ......##..
      .###...#..
      .#......#.
      #.#....#..
      ......#.#.
      #....#.#..
      #...#.#..#
      #......##.
      
      Tile 2801:
      ###....##.
      #..#...#.#
      ....##.#.#
      ..........
      ..........
      ........#.
      #....#....
      #........#
      ....#..#..
      ##.#.#.###
      
      Tile 2287:
      ####.#..#.
      #..#..#...
      ....###..#
      ..#.......
      #....#...#
      ......#...
      ##.....#..
      .#......#.
      #...##....
      .#.####...
      
      Tile 1607:
      ##.#.#.#.#
      ....#....#
      ......##.#
      ........##
      #..#....##
      #........#
      #.#..#....
      ...#..#...
      #....#.##.
      .#...##.#.
      
      Tile 1069:
      ####...###
      #..##..#..
      .........#
      #.###.....
      .#.##.#..#
      #.#..#....
      #.##..#..#
      #...#...#.
      .....#....
      ..#..#.#.#
      
      Tile 1721:
      ####..#.#.
      #.......##
      .#.##.....
      ..........
      ..#..#...#
      ##..#..#.#
      .....##..#
      .#..#..#.#
      ##.#..#...
      .#.##.#...
      
      Tile 3049:
      ...#.####.
      ##.##..##.
      .#........
      .#.....#.#
      .##.......
      #..#.#...#
      .#....#..#
      ###....#..
      ##..#..#.#
      ..###.#..#
      
      Tile 1871:
      ####.#.##.
      .....##...
      ####...##.
      .........#
      ###.....#.
      #.#......#
      #..#...#..
      #........#
      #.##...#..
      #......###
      
      Tile 1723:
      .#..######
      .###....##
      .........#
      .........#
      .....#..#.
      ##.#....##
      ...#.#..#.
      ....#...##
      ...#..#..#
      ##..##...#
      
      Tile 2237:
      .#.##.#.##
      .#.......#
      #.......##
      ..#...#...
      ....#...#.
      #...#..##.
      ...##.#...
      #..#.#....
      ##...###.#
      #.#....###
      
      Tile 1549:
      #..###.###
      .#..##...#
      ....##.#.#
      #..#.##.#.
      ......#..#
      #.#..##...
      #..#....#.
      #.#......#
      ....###...
      ##....#..#
      
      Tile 3119:
      #.#...####
      #....##..#
      ..#....##.
      #..#....##
      #..#...#..
      #...#...#.
      ....#..#..
      .##...#..#
      ..#.#...#.
      ##..#.#.##
      
      Tile 2129:
      ##.#.....#
      #....#....
      #.........
      ..##..##.#
      #.#####.##
      ##......#.
      #....#...#
      .......##.
      ....##....
      #.....####
      
      Tile 3499:
      #.#.##..##
      .....##..#
      ##...#.#.#
      ......##..
      #....#....
      ##......##
      ..#.#.....
      ###.###.#.
      #..#.##...
      ######.#..
      
      Tile 1249:
      .##.#...##
      ###..#...#
      .......#.#
      .#.#..#..#
      #.#...#..#
      #..#..#..#
      .....#....
      ....#.#.#.
      ....#....#
      ....##.#.#
      
      Tile 2789:
      ##.#....#.
      #....#.##.
      ...#....#.
      #........#
      ###.....##
      #........#
      ##.....#.#
      ####..##.#
      .###.#.###
      ##.###.###
      
      Tile 1447:
      ##..#.##..
      ...#...###
      ......#..#
      ...#..#...
      ..#....##.
      ...#.#....
      ..........
      #..##.#...
      ##...#..#.
      ##..#..##.
      
      Tile 1031:
      .##..###.#
      ...#.##.#.
      #....#...#
      .......###
      #.#......#
      #.....##..
      #....##.##
      #..#.....#
      ........#.
      .#..##....
      
      Tile 1949:
      #....#....
      .#.#......
      .#..##....
      ...###.#.#
      ....#...#.
      #..#.....#
      .#....#..#
      .#.......#
      ..#.......
      ##.#..####
      
      Tile 3079:
      .###.#.#.#
      ..........
      ##......#.
      .......###
      #......#.#
      #..#......
      .##.......
      ..#.......
      ...##.....
      #..#..#.##
      
      Tile 2803:
      .##.....#.
      .#.....###
      #...#....#
      ..#..#....
      .......#.#
      #......#.#
      .#.##.####
      ..##....#.
      #.#.#..#..
      ####.....#
      
      Tile 1481:
      ..###.##.#
      .#.#.#...#
      ##........
      ..#......#
      .###.#.###
      ...##....#
      .##.#....#
      ###...##..
      .#.#.....#
      ##.###.##.
      
      Tile 2003:
      .###...##.
      ..#...##..
      ..#......#
      ###.......
      #.##...#..
      #..####...
      ##...#..#.
      .....#...#
      #......#..
      ..###.#.##
      
      Tile 3697:
      #...#...##
      ##...#....
      .#...##...
      .........#
      #.#.#.#.#.
      #.#......#
      #..#.#..#.
      #...#.#...
      #...#.....
      #..#.#..#.
      
      Tile 3727:
      ##.#######
      .#...#...#
      .#...##..#
      ..##.....#
      #..#....##
      .........#
      ##.#...#..
      ##.#.....#
      ..#.......
      ..####..##
      
      Tile 2699:
      #.#..#.##.
      ##..#..#.#
      ####...#..
      .......#..
      #.#.....##
      .#.......#
      .....#.#.#
      #..#.#....
      .....#..#.
      .....#..##
      
      Tile 2011:
      ##..#.##.#
      ...#....##
      #........#
      ..#.#.#.#.
      .....#...#
      #......##.
      #.##..##.#
      ..##...##.
      #.#.....#.
      .#...###..
      
      Tile 2777:
      ...####..#
      #.##....#.
      ..........
      ##...###..
      #.##....#.
      .#.#.....#
      ..#..#.#.#
      ##...#..##
      #.....#..#
      #....#..#.
      
      Tile 1787:
      #.##....#.
      ##...#.#.#
      #......#..
      ......##..
      #....##..#
      .....#....
      ....##....
      #.#..#.##.
      ##....#..#
      #.#.###.#.
      
      Tile 1783:
      #.#...#.#.
      #..#.#...#
      #..##..###
      #..#.##...
      .#....#...
      ##..##...#
      #..#.....#
      #.......##
      #.#.....##
      ##..#.#..#
      
      Tile 1423:
      #.###.#..#
      ..#..##...
      ####..#.#.
      #..##..#.#
      .........#
      ##....#.##
      #.##..#...
      .........#
      ...##....#
      #..#.###..
      
      Tile 2917:
      ###...####
      #....##..#
      ......#.#.
      ....#.##.#
      ....#....#
      #........#
      .#.....#..
      ...#.##..#
      #...#.#..#
      ##...#..#.
      
      Tile 1531:
      #..#..#.##
      ...##.##.#
      .......#.#
      ......#..#
      ....#....#
      .#........
      ....#..#.#
      ...#....##
      .#.#.##...
      ..##...#.#
      
      Tile 1867:
      #.#.#####.
      .##...#..#
      ..........
      #.##..#..#
      #.###..#.#
      .......#..
      ...#.#.###
      ...##.##..
      .......#..
      .##..###.#
      
      Tile 1901:
      ##....####
      .....##..#
      #.#...#.##
      .#.#..##..
      ...#...#.#
      #..#...#.#
      ...##.#..#
      #........#
      ...###...#
      #####..#.#
      
      Tile 2879:
      .####.#..#
      #.#.#..#.#
      .##...#..#
      ####.##..#
      ..##.##...
      ##.......#
      ..........
      #.#..##.#.
      .#.#...#.#
      .#..#.#.##
      
      Tile 1187:
      .#####.###
      ##.......#
      .#...##.#.
      .#..#.#.#.
      .#..#..#.#
      ##....#...
      #..#......
      .##....###
      .#.#...#.#
      .#......##
      
      Tile 1657:
      ..##.#..##
      .#...###.#
      ....#.#..#
      ...#..#..#
      ..#......#
      ..#...#..#
      ...#...#.#
      #.....#.#.
      #..##..###
      ###.#.#..#
      
      Tile 3533:
      #####..#..
      ..#.#.##.#
      .#........
      ........#.
      .#...#.#..
      .#........
      .##.......
      #......#..
      #......###
      .###.#.#.#
      
      Tile 3037:
      .#...#..##
      #.#....#..
      .....##...
      #....####.
      #..###.##.
      #.......##
      #..#..#...
      #....###.#
      #.....##.#
      ..#.###.##
      
      Tile 2729:
      .#.##.....
      ##...#..##
      .#..#...#.
      .#...#..#.
      #.#...#...
      ###.#.#..#
      #.........
      .##...#.#.
      ##.......#
      ###..#.##.
      
      Tile 3823:
      ...#...###
      #..##.##..
      ...#......
      ...#....#.
      ....#....#
      ...#......
      ....#...##
      #........#
      ...#....#.
      #..#.#.#..
      
      Tile 2081:
      #..##..#..
      ...#....##
      ..........
      #..#.#...#
      #.#....#.#
      #..#..#.#.
      #####.#..#
      ....###.##
      ...#.#...#
      .##..#.#..
      
      Tile 1759:
      #.#.#..#..
      #........#
      ##........
      #.#......#
      .......#..
      .#.#....##
      ....#.....
      #......#..
      #..##.#..#
      ..###...##
      
      Tile 3467:
      .#.####..#
      #....###.#
      ...#..#...
      #....#....
      ....##..##
      #.#.......
      #....#...#
      .##...#.##
      ...#..#..#
      ##....#..#
      
      Tile 3041:
      ...###.#..
      .##.####..
      .#.##..##.
      #...##....
      ###......#
      #...#....#
      #..#.#.###
      ##.##.#..#
      ##.#.#...#
      #..###..#.
      
      Tile 2473:
      ###..#.#.#
      #.##.##.##
      #..#..#..#
      ##.......#
      .#..#..#..
      ..##......
      #.#...#...
      ...#.....#
      .#....##..
      #..##.#.##
      
      Tile 3191:
      .....#..##
      ##..#....#
      #........#
      .......##.
      ..#.#...##
      ...#.....#
      .........#
      ..#.#.#...
      ##.#....##
      #.#..#####
      
      Tile 2269:
      ##..#.###.
      #..#.#...#
      #.....#..#
      .#........
      #.##..##.#
      .....##.##
      .......#.#
      .#.......#
      ##.#..#.#.
      .#.#.##.#.
      
      Tile 1847:
      ...#...#.#
      ##.......#
      #.......##
      ##........
      ##.......#
      #..#..#...
      ...#....##
      #.#.##...#
      #.###....#
      ..##.####.
      
      Tile 2399:
      ###....#.#
      ....#....#
      ....##..##
      ####.#...#
      #........#
      .....#...#
      #..##.####
      .#..#.....
      ##...#.#..
      #.......##
      
      Tile 1051:
      .#..#....#
      .......#..
      .#...####.
      #..#..#.##
      ##.....#..
      ##.#.#.#..
      .#........
      #..##..#.#
      #...#.#.#.
      ##.#....##
      
      Tile 3709:
      #.....###.
      #..#.#....
      ##..#..#.#
      .......#.#
      .....#..#.
      .#..##.#.#
      .###.##..#
      #.##.#..##
      ..#......#
      ......##.#
      
      Tile 1019:
      ##...##...
      ....#.....
      .....#..#.
      ..........
      ##........
      .........#
      ..........
      #.#..#..#.
      ##....###.
      #.####.#.#
      
      Tile 1163:
      ...##...##
      ..#......#
      ......##..
      ..##..#...
      #...#....#
      #...#.##.#
      ....##....
      #........#
      #.......#.
      ##..####.#
      
      Tile 1061:
      ###.#..#.#
      .##.......
      #.##..#...
      .#..##..##
      #...#.....
      #......#.#
      #..#.#...#
      ...#.#####
      #...##.#.#
      ####.#.#..
      
      Tile 1811:
      ....#..##.
      .##.#....#
      #.#.#..#..
      ###.....##
      #.#......#
      #...#.....
      #...#.###.
      #...#.....
      ..##......
      ##.#.####.
      
      Tile 1301:
      ##.##.#..#
      #.##.##...
      #...#..##.
      ...#...#..
      .#.#......
      ....#....#
      ....#....#
      ...#.....#
      #.#.#.###.
      #..#..####
      
      Tile 1933:
      .##.#.##.#
      #...#.....
      #.....###.
      ..##....#.
      ###...#...
      ##..#....#
      #.....#..#
      #......###
      #.....#.##
      .##.###.#.
      
      Tile 1889:
      ##.#.##...
      ....#.....
      ..........
      ..##.##...
      ....##...#
      #.#.#....#
      ...##.#...
      .##.....#.
      ....##...#
      ..########
      
      Tile 2411:
      #...#.#..#
      ......#..#
      #...#..#..
      ..#.....#.
      #.#..#...#
      ..#......#
      #...#.#...
      .......#..
      ..........
      ..##..#.##
      
      Tile 1861:
      .##...#.##
      .....##...
      #........#
      ..#..#....
      ##.....#.#
      .#####...#
      .#.##..#.#
      ##..#....#
      #.......#.
      ##..#..#..
      
      Tile 2671:
      ..#..#....
      ##..#....#
      #.#.......
      .#.#.....#
      ...#......
      ..#......#
      ##........
      ###.....##
      ##.##.#.#.
      ..#.##..#.
      
      Tile 2347:
      #.####....
      #.....#...
      #...#.####
      .###..#..#
      #.#.#.....
      #.#.......
      ........#.
      ..##..#...
      #..###....
      #..#...#..
      
      Tile 1913:
      #..#.#..##
      #.#.#.#.#.
      ....#.#.#.
      #.....#..#
      #.#.###...
      #.#..#.#..
      .#.....#..
      .##...#..#
      #.......##
      ..#.#.##..
      
      Tile 2887:
      #####..###
      ....##...#
      #......#..
      ###.#.###.
      #........#
      ....#.....
      ####..###.
      ....#....#
      ....#.#...
      .###..####
      
      Tile 3449:
      ..###.#...
      ##.#.##..#
      ...#......
      #..#......
      ..#.#...#.
      #...#...#.
      #.......##
      .........#
      ...#.#...#
      #.##.##..#
      
      Tile 3947:
      .......###
      .#.#.....#
      #.#.#..##.
      ..#...#.#.
      #..#...###
      ....#..###
      ......#...
      #..##.###.
      ..#...#..#
      ..##.#.##.
      
      Tile 1109:
      #.##..#.##
      .#.......#
      #.....##.#
      .........#
      #.#..#...#
      #...#....#
      #.........
      #.#......#
      .....#.#.#
      .#.#####.#
      
      Tile 1153:
      #.#.###..#
      ####....##
      ......#...
      ........##
      ........#.
      ##..#....#
      ....#.#..#
      #.#....#..
      .#.#....##
      .#....#.##
      
      Tile 3259:
      .#.#...###
      ##....##..
      #...####..
      .#...#...#
      ..#.#.#..#
      #....###..
      ...#.###..
      .....#.##.
      #...#..#.#
      .###...#.#
      
      Tile 3659:
      ##.#..#.#.
      #.......##
      #.....##.#
      .#..#.....
      #...##.#.#
      #.##...#..
      .##.#..#..
      .##.#..#.#
      #.##..#.#.
      #..#######
      
      Tile 3833:
      ##.##....#
      .###.....#
      ##.......#
      ##.#.##..#
      ..##....#.
      ##.......#
      #....#....
      #.#......#
      .###.#.#..
      ..#..#....
      
      Tile 3529:
      ##..#####.
      #..#....##
      ...#.....#
      #..##....#
      .#.#.###.#
      #.##......
      .#.#...#..
      #...##..##
      .......#.#
      ..#.#.##.#
      
      Tile 2819:
      #.#..###.#
      .......#.#
      #...#..##.
      ##.#.....#
      .#..#.....
      .#..#.####
      #.......##
      .#...#....
      ####.....#
      ##.....##.
      
      Tile 1801:
      #####..##.
      ####..###.
      #.#..#..#.
      ..#......#
      ...#....#.
      #.#......#
      ...#.##..#
      #...#.....
      ##....##..
      .#.#..###.
      
      Tile 1307:
      ######.##.
      ..#......#
      .......##.
      #.#....#..
      #.#.#..#..
      .........#
      ....##.#..
      ..##..##..
      #....#...#
      #...##.#.#
      
      Tile 2719:
      ..#######.
      #......#..
      ###....##.
      #.....#.##
      #..#..#.#.
      ....#....#
      ......##..
      ##.#...#.#
      #.....#.##
      #..##.##..
      
      Tile 1973:
      ####..##.#
      ......#...
      ##.....##.
      #..#..#...
      ......#...
      .......#..
      ....#.#..#
      ##.#..###.
      #.##...#.#
      ##.###.#.#
      
      Tile 1553:
      #....#..##
      ...#......
      #........#
      #..#.....#
      ..#.......
      #..#.#.#..
      .......#..
      .#.#...#..
      ##.#.....#
      .###..#.#.
      
      Tile 3271:
      .#..#..#.#
      #.#...#.##
      #......#.#
      ###......#
      #....#....
      ##.......#
      #..#.##..#
      #.......#.
      ...#.#....
      .###.#####
      
      Tile 3797:
      #.##..####
      .....#....
      #...#...#.
      #...##....
      #....#..#.
      #...##.##.
      ##...##.#.
      #.......#.
      #.##...#.#
      #...##.#..
      
      Tile 2089:
      ##..###..#
      ##...#..#.
      #....##..#
      ..##.....#
      .......#.#
      #.....#..#
      #....#....
      .....#.##.
      ..##..#.##
      .....##..#
      
      Tile 3821:
      .###.....#
      ..#.#..##.
      .#...#....
      .........#
      #...#.....
      #....##..#
      #...#....#
      #...#...#.
      ..#..#....
      ##.####...
      
      Tile 2549:
      ...###.#..
      ..#.##..#.
      #.#.......
      ..##......
      #.#......#
      ..#...#..#
      .#..#.##..
      ..#....#..
      #...##..#.
      ##..#..#.#
      
      Tile 1697:
      .##.#.##..
      .......#..
      .#....####
      #......#.#
      .......#..
      ...#..#..#
      #......#.#
      .#.....###
      #..#...#..
      #...###...
      
      Tile 2749:
      ..#...#..#
      ..#...#..#
      #.##.###..
      ..##......
      #..#.###..
      #.####....
      #.....#...
      #.#.#.....
      #.........
      #...###.##
      
      Tile 1063:
      .##..#..#.
      ..#....#.#
      ..##..#..#
      ........#.
      ...#.....#
      .........#
      #.....#.##
      .......###
      ##..#.#...
      .#..####..
      
      Tile 1471:
      ...##..#.#
      ##.#..#...
      #...#....#
      ..#......#
      #.#.#...##
      ..##.....#
      .........#
      ......##.#
      .#..#.#..#
      #...#####.
      
      Tile 1609:
      #..##.####
      ##...#....
      #.#...#...
      .#.###.###
      .....#.#..
      ##....#...
      ...##...#.
      ##........
      #...#.#...
      ##...#....
      
      Tile 2437:
      ..##.#.#.#
      ..#.#....#
      ...#......
      #.....#..#
      .#....#..#
      ##..##....
      #.#......#
      .....#..#.
      ....##.###
      ..#####.##
      
      Tile 1823:
      ###..###..
      ##...#....
      #.....###.
      #........#
      .....###.#
      .....#..##
      #..#..#...
      ##........
      #.........
      ..########
      
      Tile 2273:
      #.#.#.#.##
      ....#..#..
      #..#......
      #....##...
      .#.#..#.##
      #.........
      .#.##..#..
      ..#......#
      #....##.#.
      ##.##.####
      
      Tile 3607:
      ##.#.###.#
      ...#.....#
      ##..##..##
      ...###....
      ..##.#...#
      #.#...#.#.
      #...#....#
      #..####...
      ....#.#.#.
      ...#...###
      
      Tile 1303:
      .#..######
      ...#...#.#
      ..##...##.
      ....#..#..
      #.#.###.##
      ##.#..####
      ..#...#...
      #.#.#..#.#
      #.........
      #.#.#####.
      
      Tile 1283:
      .#.#.#....
      .#..##....
      .#...#....
      #.........
      ##.###...#
      ##.####..#
      #....#...#
      .#.#...#.#
      #.##.....#
      ##.#..#.#.
      
      Tile 2791:
      #....###..
      ....#....#
      #......#.#
      #....#.##.
      .#.##....#
      #......#.#
      .......#..
      ....#....#
      #..#......
      ..#####.##
      
      Tile 2797:
      .#####..#.
      .......#..
      #..##....#
      .##...##.#
      .#...#.#.#
      #.#...##.#
      #.#.......
      ##.##...##
      ..#....#..
      ###..##...
      
      Tile 1087:
      ......#.##
      #.#......#
      .#....#..#
      #.#.#.#..#
      #...#.....
      #..#...#..
      .....#...#
      ...#......
      #.....#..#
      ###..##.##
      
      Tile 1409:
      #####.###.
      .#.#.#...#
      #....#...#
      .....#.#.#
      #....#..##
      ###..##.##
      ##..####..
      ..........
      #........#
      .####.#.##
      
      Tile 3593:
      #.######..
      ##...#.#..
      ..#......#
      #.....##.#
      .#...#....
      #...#.#.#.
      ...#..#...
      ##.......#
      .....#...#
      ...#..#..#
      
      Tile 3347:
      .#.##.#.#.
      ..........
      ....#...#.
      #.#..#..##
      ...#..##..
      ##.#....#.
      #.#..#...#
      .#.###.#.#
      #...#.#..#
      .#.####.##
      
      Tile 3637:
      .#.##....#
      ##...###.#
      .#.#.#....
      ....##...#
      .#........
      #......#..
      ##.#...#..
      ###..##..#
      .#........
      ...#.###..
      
      Tile 1907:
      #.#.#..###
      .##..#....
      ##..#..#.#
      .......#..
      #..#......
      ..##.#..#.
      .###..##.#
      .......#.#
      .#....#...
      #..#...#.#
      
      Tile 3371:
      #.##...###
      .....#....
      #......#..
      .#.#..#...
      ##.#...#..
      .##......#
      ..#..###.#
      #.....#.#.
      #.#..#.#.#
      ...#..####
      
      Tile 1367:
      ##.....#..
      .#..#..#..
      .###....##
      .#.#..#.##
      ##.....##.
      ###.....##
      ...#...#.#
      #....#...#
      ##.....#.#
      #....#.##.
      
      Tile 1559:
      #..#.##..#
      #..#.##...
      #.#..#...#
      .......#.#
      ....#.####
      #...#...##
      ..#.#...##
      #....#...#
      ..#..#.#.#
      .........#
      
      Tile 2713:
      ..#.###.##
      #.##.....#
      #......#.#
      #..#...#..
      .#.......#
      .......##.
      .........#
      ..#...##.#
      #....#.#.#
      #.#..##...
      
      Tile 3203:
      ###.#####.
      ###...#...
      ###..#...#
      .#..##..##
      #....#.##.
      #.....#..#
      #....#...#
      ..........
      .##...#...
      .#.###.##.
      
      Tile 1979:
      #...#.#..#
      #.#.....#.
      .#.###...#
      .....#.##.
      ##.#.#..##
      .##....###
      .##......#
      #......#.#
      #.....#.#.
      ......##.#
      
      Tile 3307:
      ..##...#.#
      #......#..
      ##..#.##..
      ...##.#.##
      .##.......
      .##....###
      #..##.#..#
      .#.#.#....
      #...#.#..#
      #.#..###.#
      
      Tile 1667:
      ###.#.##..
      .......##.
      ..#...##..
      ........##
      #.......##
      #.....##..
      ...##.#.#.
      #.......#.
      ........##
      ##....#...
      
      Tile 1699:
      ..##...###
      .....#....
      #..#.###..
      ...##.....
      .##.#..#..
      ......#...
      #.........
      ####.....#
      #.#..#....
      #...#..#.#
      
      Tile 2591:
      #...##....
      ###..###..
      ##.#..#...
      ..#.#..##.
      .....#.#.#
      ##..###...
      #..####.##
      ##........
      ..#......#
      .###.#..#.
      
      Tile 3169:
      ####.##...
      ##.##.....
      ......#...
      #.##..#.##
      ....###.#.
      #.#..#..##
      #..###....
      #..#.....#
      ..###..#..
      ##..##..#.
      
      Tile 1459:
      ..#...####
      .....##...
      ..#.......
      ...#.#....
      ##....#..#
      .##...##.#
      ..#..#.###
      #.##..#...
      #..#.#.#.#
      ..#.#.##..
      
      Tile 3373:
      ..#####..#
      ..#..#...#
      #.###..#.#
      ...##.....
      .#.##.....
      #..#..#...
      ....#.....
      #.#.#....#
      ...#...#..
      .##.......
      
      Tile 3461:
      ######...#
      ....##....
      ##........
      #.#..#.#.#
      ###.....#.
      .....#....
      .....###.#
      ##.......#
      ....###.#.
      ..#.#..##.
      
      Tile 2609:
      #####...##
      #.........
      .....##...
      ##....##..
      ..##.#....
      ##.......#
      ........#.
      ........##
      ####......
      .#.###....
      
      Tile 2459:
      #..##.#...
      .#...#####
      ##........
      #....#....
      #...##.#..
      ##...###.#
      ##........
      ...##....#
      #...#....#
      .##.....#.
      
      Tile 1831:
      ###..#..##
      .#......##
      .#.#..#...
      #......#.#
      #.........
      ..#....#..
      .####...#.
      ....#..#..
      ......##.#
      ###...#..#
      
      Tile 2063:
      .###...#.#
      ###......#
      ##..#..###
      ##........
      .###.##.##
      ##.#.#..#.
      ..##....##
      ..#......#
      #####...#.
      #####..#..
      
      Tile 2281:
      #...####..
      ..#.#....#
      ........#.
      #.........
      ..#..#..##
      ##.##..#.#
      ........#.
      ....##...#
      ####.#.#.#
      .###..#..#
      
      Tile 1583:
      #####.##.#
      #....##.#.
      #..##.....
      ...#......
      ..###.#...
      ..##.....#
      ##..#....#
      .....#...#
      .......##.
      #.##...#.#
      
      Tile 2539:
      ..##......
      .#..#..#..
      #...#...#.
      #..##.#.#.
      ......####
      #.##...##.
      ##...#..#.
      #.#......#
      ##....#..#
      ###..#...#
      
      Tile 2909:
      ..#.....##
      ##...##..#
      #........#
      .#........
      ###....###
      ...#.##..#
      #..#..##..
      #..##...#.
      ....#....#
      #..##..#..
      
      Tile 2939:
      ..#...#.#.
      ..#.......
      ..........
      .#.##..#.#
      #.##.##.#.
      ###.#....#
      ..#..#....
      .##......#
      ##...##...
      ##.#..#...
      
      Tile 3889:
      ##..#.#.##
      ##.####...
      #.#.####..
      ..####..#.
      ##.#.....#
      #.......##
      ........##
      ##.##....#
      ...###...#
      #..###..##
      
      Tile 1427:
      #.#.#....#
      ##.##.####
      .....#...#
      #.#..#....
      ...#.#..#.
      .....#...#
      .#...###.#
      ####.#....
      ........##
      #..#..##..
      
      Tile 1319:
      .#####...#
      #...#....#
      #........#
      #....##...
      .##......#
      ..#.....##
      .....#..##
      ......#..#
      ...#....#.
      .##....###
      
      Tile 1523:
      .....###.#
      ..#......#
      ...#.....#
      #..#..#..#
      .........#
      #..#..#.#.
      ####..#..#
      ..#......#
      ...##.##.#
      ####....##
      
      Tile 2381:
      .#.#..##.#
      #..#..#.##
      ..........
      ..###..#..
      .###.##.##
      ##....##.#
      ...##....#
      #...#.....
      .#.......#
      #....###.."""

}
