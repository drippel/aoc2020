package com.pfs.advent.test

import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.generate.GridGraphGenerator
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}
import org.jgrapht.nio.dot.DOTExporter
import org.jgrapht.util.SupplierUtil

import java.io.{File, FileWriter}
import java.util.function.Supplier

object JGT {

  def main(args: Array[String]): Unit = {
    Console.out.println("jgt...")
    val gr = new SimpleGraph[String, DefaultEdge](classOf[DefaultEdge])

    val a = "a"
    val b = "b"
    val c = "c"
    val d = "d"
    val e = "e"
    val f = "f"
    val g = "g"

    gr.addVertex(a)
    gr.addVertex(b)
    gr.addVertex(c)
    gr.addVertex(d)
    gr.addVertex(e)
    gr.addVertex(f)
    gr.addVertex(g)

    gr.addEdge(a, b)
    gr.addEdge(b, d)
    gr.addEdge(d, g)

    gr.addEdge(a, c)
    gr.addEdge(c, e)
    gr.addEdge(e, f)
    gr.addEdge(f, g)


    val dsp = new DijkstraShortestPath[String, DefaultEdge](gr)
    val paths = dsp.getPaths(a)
    val sp = paths.getPath(g)
    Console.out.println(sp)
    val p2 = dsp.getPath(d, e)
    Console.out.println(p2)

    val exporter = new DOTExporter[String, DefaultEdge]()
    val writer = new FileWriter(new File("g.dot"))
    exporter.exportGraph(gr, writer)

    val vs = new Supplier[String] {
      var i = -1

      def get(): String = {
        i = i + 1
        i.toString
      }
    }

    // lets try a grid
    val ggg = GridGraphGenerator[String, DefaultEdge](10, 10)
    val gg = new SimpleGraph[String, DefaultEdge](vs, SupplierUtil.createDefaultEdgeSupplier(), false)
    ggg.generateGraph(gg)

    val e2 = new DOTExporter[String, DefaultEdge]()
    val w2 = new FileWriter(new File("g2.dot"))
    e2.exportGraph(gg, w2)
  }

}
