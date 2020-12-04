package com.pfs.advent

object ABCs {
  
  def main( args : Array[String] ) =
    val abc = ( 'a' to 'z' ).toList.zipWithIndex
    abc.foreach( Console.out.println(_))
    val ds = Dirs.map( d => (d.row, d.col))

}
