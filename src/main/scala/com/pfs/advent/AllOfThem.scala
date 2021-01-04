package com.pfs.advent

object AllOfThem {

  def main( args : Array[String] ) : Unit = {
    
    Day01.main(args)
    Day02.main(args)
    Day03.main(args)
    Day04.main(args)
    Day05.main(args)
    Day06.main(args)
    Day07.main(args)
    Day08.main(args)
    Day09.main(args)
    Day10.main(args)
    Day11.main(args)
    Day12.main(args)
    Day13.main(args)
    Day14.main(args)
    Day15.main(args)
    Day16.main(args)
    Day17.main(args)
    Day18.main(args)
    try {
      Day19.main( args )
    }catch {
      case t : Throwable => { 
        // carry on
      }
    }
    Day20.main(args)
    Day21.main(args)
    Day22.main(args)
    Day23a.main(args)
    Day24.main(args)
    Day25.main(args)
    
  }

}
