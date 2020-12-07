package com.pfs.advent.grid

object ABCs {

  def main(args: Array[String]) =
    val abc = ('a' to 'z').toList.zipWithIndex
    abc.foreach(Console.out.println(_))
    val ds = com.pfs.advent.grid.Dirs.map(d => (d.row, d.col))

}
