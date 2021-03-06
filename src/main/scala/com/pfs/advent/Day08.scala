package com.pfs.advent

import java.util.Date
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day08 {


  def main(args : Array[String] ) : Unit = {
    Console.out.println("2020 08")
    val prog = parse(input)
    val start = new Date()
    val cpu = Computer()
    cpu.boot()
    cpu.load(prog)
    Console.out.println(cpu.run())
    val result = optimizer(prog)
    Console.out.println(result)
    val end = new Date()
    Console.out.println( end.getTime - start.getTime )

  }
  
  def optimizer( prog : List[Line] ) = {

    val nops = prog.filter( _.code.equalsIgnoreCase("nop") ).map( _.lineNumber ).map( i => ( "nop", "jmp", i ) )
    val jmps = prog.filter( _.code.equalsIgnoreCase("jmp") ).map( _.lineNumber ).map( i => ( "jmp", "nop", i ) )
    val combined = nops ++ jmps

    def innerOptimizer( changes : List[(String,String,Int)], result : (Boolean,Long) ) : (Boolean,Long) = {

      if( changes.isEmpty ) {
        (false,-1)
      }
      else if( result._1 == true ) {
        result
      }
      else {

        val (h,t) = (changes.head, changes.tail)

        // Console.out.println(s"changing line ${h._3} from ${h._1} to ${h._2}")

        // start with a copy of original program
        val inMem = ListBuffer[Line]()
        inMem ++= prog

        // Console.out.println(inMem)

        // modify the program
        val cl = inMem(h._3)
        val nl = Line(cl.lineNumber, h._2, cl.args)
        inMem(h._3) = nl

        // Console.out.println(inMem)

        val cpu = new Computer()
        cpu.boot()
        cpu.load(inMem.toList)
        val nextResult = cpu.run()
        // Console.out.println(nextResult)

        // call inner result here
        innerOptimizer( t, nextResult )
      }
    }

    innerOptimizer( combined,(false,-1) )

  }
  
  class Computer() {

    // disk vs mem?
    
    var program = ListBuffer[Line]()

    // write to stdout
    val stdout = new mutable.Queue[String]()

    // read from stdin
    val stdin = new mutable.Queue[String]()

    def boot() : Unit = {
      // nada for now
    }
    
    def load( lines : List[Line] ) = {
      program.clear()
      program ++= lines
    }
    
    def run() : (Boolean,Long) = {
      
      // make a copy of the currently loaded program
      val inMem = ListBuffer[Line]()
      inMem ++= program
      
      val debug = false
      
      // computer state here
      val executionCounter = mutable.HashMap[Int,Int]()
      
      def innerRun( ip : Int, accum : Long ) : (Boolean,Long) = {
        
        val cnt = executionCounter.getOrElse(ip,0)
        if( cnt > 0 ) {
          // loop detection
          (false,accum)
        }
        else if( ip >= inMem.size || ip < 0 ){
          // went past end or beginning of the progam - normal termination
          (true,accum)
        }
        else {

          // get the instruction
          val instruction = inMem(ip)

          // increment the count
          executionCounter(ip) = cnt + 1

          // if it gets complex create a state class and functions?
          val (next, naccum) = instruction.code match {
            case "acc" => { (ip + 1, accum + argToInt(instruction.args.head)) }
            case "jmp" => { (ip + argToInt(instruction.args.head), accum) }
            case "nop" => { (ip + 1, accum) }
          }

          innerRun(next, naccum)
        }
        
      }
      
      innerRun(0,0L)
      
    }
    
    def argToInt( s : String ) = s.toInt
    
  }
  
  case class Line( lineNumber : Int, code : String, args : List[String] )
  
  def parse( raw : String ) : List[Line] = {
    
    def innerParse( lines : List[String], ln : Int, prog : List[Line] ) : List[Line] =
      if( lines.isEmpty ) then
        prog
      else
        val (h,t) = (lines.head,lines.tail)
        val ps = h.split(" ").toList.map(_.trim)
        innerParse( t, ln + 1, ( prog :+ Line( ln, ps.head, ps.tail ) ) )
      end if
    end innerParse
    
    innerParse(raw.split("\n").toList.map(_.trim), 0, List() )
  }
  
  val test =
    """nop +0
      acc +1
      jmp +4
      acc +3
      jmp -3
      acc -99
      acc +1
      jmp -4
      acc +6"""
      
  val input =
    """acc +7
      acc +23
      acc +41
      jmp +173
      acc -17
      acc +42
      acc +31
      jmp +349
      jmp +1
      jmp +252
      nop +574
      jmp +298
      acc +45
      acc +7
      jmp +338
      nop +5
      nop +528
      jmp +547
      jmp +313
      jmp +387
      acc +43
      acc +48
      acc +38
      jmp +45
      jmp +438
      acc +15
      acc +21
      acc +25
      acc +25
      jmp +168
      jmp -5
      acc +49
      acc +43
      jmp +99
      acc -8
      acc +16
      acc -7
      jmp +513
      jmp +484
      jmp +270
      nop +422
      acc -4
      nop +242
      jmp +1
      jmp +11
      nop +122
      nop +263
      acc +2
      jmp +474
      jmp +501
      nop +38
      acc -7
      acc +0
      nop +85
      jmp +496
      acc +11
      acc -13
      acc +40
      acc +29
      jmp +519
      jmp +409
      acc +41
      jmp +1
      acc -17
      jmp +16
      nop +485
      acc -7
      jmp +58
      acc +16
      acc +1
      jmp +123
      jmp +157
      acc +43
      jmp +422
      jmp +1
      acc -19
      acc +48
      jmp +80
      jmp +500
      jmp -59
      acc +34
      acc +11
      jmp +75
      nop +467
      acc -16
      acc +9
      acc +32
      jmp -69
      acc -13
      jmp +422
      jmp +96
      acc -10
      acc -19
      jmp -68
      acc +31
      nop +102
      acc +25
      jmp +140
      acc +34
      acc +45
      acc -9
      acc -17
      jmp -34
      nop +262
      jmp +236
      acc +0
      acc +32
      jmp +269
      acc +16
      jmp +1
      jmp +382
      jmp -39
      acc +45
      nop +166
      nop +408
      acc +10
      jmp +379
      jmp +1
      acc +44
      jmp +249
      nop +334
      acc +36
      nop +442
      acc +5
      jmp +440
      acc +0
      acc +44
      jmp +432
      acc +48
      acc +4
      acc +50
      jmp +355
      acc +31
      jmp +1
      acc +46
      nop -74
      jmp +33
      jmp +91
      nop +463
      acc +41
      nop -2
      jmp +132
      acc +41
      acc +43
      acc +28
      jmp -65
      acc -17
      acc +33
      jmp +183
      acc +11
      jmp +181
      jmp +450
      acc -18
      acc -2
      acc +44
      nop +416
      jmp +108
      acc -18
      acc +12
      acc -1
      acc -19
      jmp +321
      acc +50
      acc -17
      jmp +1
      nop +161
      jmp -41
      jmp +52
      jmp +84
      acc +11
      acc +19
      acc +40
      jmp +293
      acc +29
      jmp +1
      jmp +311
      nop +91
      acc +1
      acc +0
      acc +16
      jmp -42
      acc +0
      acc -16
      acc +41
      nop +348
      jmp -39
      nop -114
      nop +320
      acc +46
      acc -1
      jmp +55
      nop +278
      jmp -94
      acc +47
      jmp +365
      acc +44
      jmp -58
      jmp +1
      jmp +114
      acc -13
      acc -5
      acc +12
      jmp +183
      nop +237
      acc +26
      acc +49
      acc +1
      jmp -189
      acc +7
      acc +2
      jmp -190
      acc -17
      acc +18
      acc -1
      jmp -47
      nop -39
      acc -18
      nop +354
      jmp +264
      acc +46
      jmp +179
      acc +22
      acc +24
      jmp +309
      acc +45
      acc -9
      jmp -206
      jmp +34
      nop +254
      acc +9
      acc +32
      jmp +391
      acc +9
      acc +20
      acc +7
      acc +48
      jmp -85
      acc +27
      acc -3
      jmp +146
      acc -12
      acc +37
      acc +23
      jmp +1
      jmp +48
      acc +46
      jmp +99
      acc -12
      acc -2
      acc +49
      jmp +1
      jmp +293
      jmp +1
      acc +38
      jmp +13
      jmp -215
      jmp -145
      acc +7
      nop +73
      nop +189
      jmp +167
      jmp +332
      acc +29
      jmp -146
      jmp +198
      acc +10
      jmp +342
      acc +31
      jmp -136
      acc +16
      acc +33
      acc +26
      jmp -48
      acc +14
      jmp +91
      acc -15
      nop +274
      acc -2
      jmp -75
      acc +14
      acc +21
      acc +4
      jmp +332
      jmp -243
      acc +25
      acc -5
      jmp +250
      acc -17
      acc +32
      acc +28
      acc +34
      jmp -80
      acc +23
      acc +30
      acc +10
      nop -98
      jmp -205
      acc -16
      acc -15
      acc +49
      acc +15
      jmp +11
      nop +97
      acc -2
      acc +31
      jmp +1
      jmp -130
      acc +25
      jmp +129
      nop -231
      jmp +274
      jmp -280
      acc +0
      acc -14
      acc +8
      nop -224
      jmp +328
      acc +6
      acc +29
      acc +9
      jmp -229
      acc +8
      jmp -284
      acc +4
      acc +0
      jmp -200
      acc +18
      acc +33
      jmp -76
      acc -2
      jmp +139
      nop -70
      acc -6
      acc +9
      jmp -25
      nop +21
      acc +37
      acc +15
      acc +45
      jmp +130
      acc +45
      acc -5
      jmp -86
      acc -15
      jmp +55
      nop -305
      acc +24
      jmp -275
      jmp +1
      acc +31
      acc -19
      jmp -148
      acc +27
      jmp +279
      acc +11
      jmp +253
      acc +17
      nop -1
      acc -15
      jmp -57
      acc +12
      acc +10
      acc -7
      acc +18
      jmp -100
      acc +39
      jmp -180
      jmp +155
      acc -14
      acc -10
      acc -14
      nop -202
      jmp -267
      acc +11
      acc +0
      jmp -130
      acc +19
      acc -18
      jmp +166
      jmp +61
      jmp +13
      acc -2
      jmp +1
      acc +19
      jmp -160
      acc +23
      jmp +1
      acc +37
      acc +40
      jmp +86
      acc +17
      acc -18
      jmp -195
      acc +11
      nop -149
      acc -13
      jmp +41
      acc -16
      jmp -30
      acc +34
      acc +13
      acc +38
      jmp +46
      acc -13
      acc +34
      jmp -273
      acc -9
      acc -8
      acc +23
      acc +8
      jmp +82
      acc +3
      acc +43
      nop +137
      jmp -46
      acc -15
      acc +41
      acc +25
      acc +3
      jmp -208
      acc +0
      jmp -169
      acc +20
      acc +12
      jmp -221
      acc -14
      jmp +96
      acc +47
      acc +25
      acc +7
      jmp +141
      acc -19
      jmp -294
      acc +28
      jmp -94
      acc +35
      jmp +33
      jmp -349
      acc -17
      jmp +193
      jmp +1
      acc -16
      jmp -169
      jmp +1
      nop -258
      acc +44
      nop -13
      jmp -330
      jmp +189
      acc +20
      acc +31
      nop +35
      acc +42
      jmp +64
      acc +9
      nop -406
      acc -14
      jmp +1
      jmp +74
      acc +34
      acc +0
      jmp -285
      jmp -422
      nop -338
      jmp +47
      nop -445
      jmp -145
      jmp +1
      jmp -116
      acc +41
      acc +44
      acc +34
      jmp -146
      acc +44
      jmp -434
      acc +44
      acc +34
      jmp -185
      acc -17
      nop -187
      nop -5
      jmp -96
      nop -20
      jmp -199
      acc +33
      jmp -229
      nop +50
      jmp -263
      acc -5
      acc -4
      acc +16
      jmp -340
      jmp -77
      nop -71
      jmp -168
      acc -18
      nop -447
      nop -479
      jmp -118
      acc +49
      nop -35
      jmp -264
      acc +21
      jmp -76
      acc +25
      acc +46
      jmp -339
      jmp -382
      nop -54
      nop -169
      jmp -208
      acc -8
      jmp -395
      acc -8
      acc +45
      nop -312
      jmp +92
      jmp -31
      acc +45
      acc +42
      nop -259
      jmp -169
      nop -255
      nop -69
      acc +47
      acc +35
      jmp -428
      acc +15
      acc +47
      acc +50
      acc +13
      jmp -491
      jmp -386
      acc +32
      acc +36
      jmp -73
      acc +22
      acc +0
      acc +35
      jmp -531
      acc +21
      nop -365
      acc +16
      jmp +89
      acc +50
      jmp -467
      acc +42
      nop -167
      acc +39
      jmp -481
      acc -13
      acc +49
      acc +8
      acc -11
      jmp -47
      acc +22
      acc +23
      nop +14
      jmp +56
      jmp -57
      acc +0
      acc +45
      acc -12
      jmp -339
      acc +41
      jmp -286
      acc +24
      acc -14
      acc +7
      nop -481
      jmp -539
      acc +14
      jmp -511
      acc +1
      acc -14
      jmp +1
      acc -12
      jmp -123
      acc -17
      acc +11
      jmp -16
      nop -148
      acc -14
      jmp -485
      nop -258
      nop -123
      acc +22
      jmp -359
      nop -527
      nop -443
      acc +43
      jmp +1
      jmp -406
      acc +39
      acc +13
      acc +3
      acc -5
      jmp -585
      acc +41
      acc +26
      jmp -83
      acc +30
      acc +8
      acc +36
      jmp -150
      acc +36
      acc +43
      jmp -305
      acc +10
      acc +33
      jmp -188
      nop -285
      acc -4
      jmp -385
      acc -1
      jmp +1
      nop -23
      jmp -471
      acc +24
      acc +16
      acc +29
      jmp -114
      nop -471
      acc +4
      nop -360
      nop -294
      jmp -220
      acc -18
      acc +21
      acc +10
      acc +0
      jmp -166
      jmp -192
      acc +37
      acc +24
      nop -198
      jmp -425
      acc -19
      acc +43
      jmp -608
      acc +17
      acc +32
      acc +0
      jmp -424
      acc +50
      acc +46
      nop -555
      acc -16
      jmp +1"""
}
