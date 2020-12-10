package com.pfs.advent.test

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object FTest {

  def main(args: Array[String]): Unit = {
    println("ftest...")
    
    val fs : Future[String] = Future { 
      println("hang on")
      Thread.sleep(10000)
      "Welcome from the past" 
    }
    
    fs.onComplete( t => {
      
      t match {
        case Success(msg) => { println(msg)}
        case Failure(f) => { println(f)}
      }
      
    })

    
    Thread.sleep(20000)
    println("done 1")
    
    
    val amount1 = Future {
      Thread.sleep(10000)
      println("ok 1")
      101
    }

    val amount2 = Future {
      Thread.sleep(5000)
      println("ok 2")
      103
    }

    
    val total = for {
      a1 <- amount1
      a2 <- amount2
    } yield a1 + a2
    
    Thread.sleep(20000)
    
    total.foreach( println(_))

    println("done 2")
  }

}
