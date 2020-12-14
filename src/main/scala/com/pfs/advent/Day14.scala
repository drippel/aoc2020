package com.pfs.advent

import java.time.{Duration, Instant}
import scala.collection.mutable

object Day14 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    Console.out.println("2020 14...")
    val ls = toLines(input)
    // ls.foreach(Console.out.println(_))
    
    /*
    var i = 7
    println( i.toBinaryString )
    println( toBits( i ) )
    
    i = 8
    println( i.toBinaryString )
    println( toBits( i ) )
    */
    
    // part1(ls)
    val start = Instant.now()
    part2(ls)
    val end = Instant.now()
    println( Duration.between(start,end).toMillis)
    
  }
  
  def part1( input : List[String] ) = {
    
    def innerPart1( lines : List[String], mask : String, accum : Map[Int,String] ) : Map[Int,String] = {
      if( lines.isEmpty ) {
        // dones
        accum
      }
      else {
        val step = lines.head
        println(mask)
        val nextMask = if( step.startsWith("mask")) {
          val ps = step.split('=').toList.map( _.trim ) 
          ps(1)
        }
        else { mask }
        
        val nextAccum = if( step.startsWith("mem")) {
          val ps = step.split('=').toList.map( _.trim )
          val data = ps(1)
          val ps2 = ps(0).split('[').toList.map( _.trim ) 
          val loc = ps2(1).replace(']', ' ').trim.toInt
          println( s"writing ${data} to ${loc}")
          val res = applyBitMask( nextMask, toBits( data.toInt ) )
          accum + ( loc -> res )

        }
        else { accum }
        
        innerPart1(lines.tail, nextMask, nextAccum )
      }
    }
    
    val res = innerPart1(input, toBits(0), Map())
    val is = res.map( kv => { java.lang.Long.parseLong( kv._2, 2 ) } ).toList
    val sum = is.fold(0L)( _ + _ )
    println(sum)
  }
  
  def part2( input : List[String] ) = {
    
    val memory = mutable.HashMap[Long,String]()

    def innerPart2( lines : List[String], mask : String ) : Unit = {
      if( lines.isEmpty ) {
        "done"
      }
      else {
        
        val step = lines.head
        
        val nextMask = if( step.startsWith("mask")) {
          val ps = step.split('=').toList.map( _.trim )
          val m = ps(1)
          m
        }
        else { mask }

        if( step.startsWith("mem")) {
          val ps = step.split('=').toList.map( _.trim )
          val data = ps(1)
          val ps2 = ps(0).split('[').toList.map( _.trim )
          val loc = ps2(1).replace(']', ' ').trim.toInt
          
          val locs = decodeMemoryAddress( nextMask, loc )
          locs.foreach( l => memory(l) = data ) 
        }
        
        innerPart2(lines.tail, nextMask )
        
      }
    }

    innerPart2(input, toBits(0) )
    val is = memory.values.map( _.toLong )  
    val sum = is.fold(0L)( _ + _ )
    println(sum)
  }
  
  def decodeMemoryAddress( mask : String, address : Int ) : List[Long] = {
    val bs = toBits(address)
    val wm = applyBitMask2(mask, bs)
    val exp = expandAddress(wm)
    exp.map( s => java.lang.Long.parseLong( s, 2 ) )
  }
  
  def expandAddress( start : String ) : List[String] = {
    
    def innerExpand( cs : List[Char], accum : List[String] ) : List[String] = {
      
      if( cs.isEmpty ) {
        accum
      }
      else {
        
        var c = cs.head
        
        val nextAccum = if( c == '0' || c == '1' ) {
          accum.map( s => s + c )
        }
        else {
          // c == X
          val comb = accum.map( s => {
            List( ( s + '0'), ( s + '1') )
          })
          comb.flatten
        }
        
        innerExpand( cs.tail, nextAccum )
      }
      
    }
    
    innerExpand( start.toList, List( "" ) )
  }

  def applyBitMask2( mask : String, src : String  ) = {
    val z = mask.zip(src)
    val res = z.map( t => {
      if( t._1 == 'X' ){
        t._1
      }
      else if( t._1 == '1') {
        t._1
      }
      else {
        t._2
      }
    })
    res.mkString
  }


  def toBits( i : Int ) = {
    val s = i.toBinaryString
    s.reverse.padTo(36,'0').reverse
    
  }
  
  def applyBitMask( mask : String, src : String  ) = {
    val z = mask.zip(src)
    val res = z.map( t => {
      if( t._1 == 'X' ){
        t._2
      }
      else {
        t._1
      }
    })
    res.mkString
  }
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      mem[8] = 11
      mem[7] = 101
      mem[8] = 0"""
      
  val test2 =
    """mask = 000000000000000000000000000000X1001X
      mem[42] = 100
      mask = 00000000000000000000000000000000X0XX
      mem[26] = 1"""
  
  val input =
    """mask = 1101000X0110100X1X1X00001010XX00X0X0
      mem[48482] = 6450058
      mem[5309] = 2219920
      mem[27274] = 43286042
      mem[40233] = 504849
      mask = 1X1101XX1110X000101100XX001X000100X1
      mem[40763] = 53122377
      mem[24930] = 244
      mem[60497] = 16546
      mem[60898] = 654
      mask = 1011100X10111XX0011XX010001X1XX0X100
      mem[35453] = 378
      mem[445] = 17680
      mem[19816] = 1045213
      mem[496] = 222512
      mask = X00100X100100011001011000X0X0110000X
      mem[25953] = 120906
      mem[42177] = 14177504
      mem[12302] = 31911950
      mem[33869] = 46903
      mask = 10011X1111100X1110001X0000110000XX01
      mem[43400] = 15586
      mem[5165] = 24662
      mem[3053] = 496403
      mask = 10110110X1X0011XX0110010X11011X0010X
      mem[23273] = 359
      mem[8290] = 1906218
      mem[4777] = 33822
      mem[24904] = 92027
      mem[55979] = 348
      mask = X0X1100010X1XX010X0000X00001XX100000
      mem[63411] = 198864361
      mem[2323] = 12235447
      mem[53128] = 3559
      mem[37957] = 978071
      mem[12837] = 57030
      mem[18230] = 7330
      mask = 11X100X111X0001101X01101X11XX0110001
      mem[20551] = 95829554
      mem[52592] = 3255
      mem[14690] = 66824
      mem[53729] = 16600
      mem[14246] = 48015
      mem[49654] = 13036614
      mem[12199] = 78275448
      mask = 100XX00100X0X011001X1010010110X00X11
      mem[11978] = 57478
      mem[23642] = 1593
      mem[26097] = 6120
      mem[54294] = 1426
      mem[25730] = 2662370
      mem[48051] = 283445930
      mask = 0X01111X111X00X11010000010XX001X000X
      mem[48807] = 1811127
      mem[54732] = 130682
      mem[14556] = 127239
      mem[8434] = 329378888
      mem[65311] = 30415
      mem[38753] = 129645
      mask = 1101X1X11110X011X0X000XXX00010000110
      mem[58126] = 5447027
      mem[61916] = 262525830
      mem[29247] = 53096
      mask = X010011001100001101111001000X0X0X000
      mem[27274] = 20355
      mem[24280] = 94406194
      mem[46969] = 32599
      mask = 10110110111XXXXX1011X11X111010101XX1
      mem[13208] = 1476
      mem[17525] = 174267
      mem[40650] = 47095
      mem[6880] = 23668444
      mem[18597] = 210524
      mem[61423] = 11469
      mem[28797] = 72808658
      mask = 1101X11X11X001111X101XX001X100X00000
      mem[10402] = 382156
      mem[11615] = 91849
      mem[55387] = 38274
      mem[41608] = 99166858
      mask = 10XXXX1X111000X1101X0X10110010101010
      mem[64410] = 16272
      mem[53605] = 39
      mem[43920] = 231262355
      mem[24494] = 101085
      mem[2266] = 7434
      mem[6405] = 490
      mem[20768] = 56259
      mask = 1101111X11100X11100001X00X00100000X1
      mem[12148] = 1542
      mem[14674] = 32572259
      mem[13208] = 1048025
      mem[10573] = 2522
      mem[25144] = 65748456
      mask = 1X0X1X01X11000XX0X10101100X111X01000
      mem[216] = 3249
      mem[61137] = 125226363
      mem[20373] = 8025
      mem[10889] = 447307190
      mask = 1X01111111100X1110X00110XX1110XX1X0X
      mem[43138] = 1515
      mem[59301] = 99565475
      mem[24915] = 103737871
      mem[20768] = 514918
      mem[11898] = 685393
      mem[36252] = 2583474
      mem[5165] = 129314
      mask = 1011100011X1X00XX1X00X1XX01X01111011
      mem[34243] = 6266385
      mem[32449] = 3199
      mem[37594] = 12713
      mem[33859] = 85810
      mem[61637] = 4581168
      mem[40941] = 2511
      mask = 10011XX1XX1X0X011X0100110011110X0000
      mem[11494] = 359697
      mem[45551] = 128326843
      mem[12702] = 427072
      mask = 1X01X0X111100X110010001X001111X000X0
      mem[19264] = 1388
      mem[14556] = 2634
      mem[56860] = 913
      mem[46144] = 410717
      mem[26110] = 286330629
      mem[13224] = 70224078
      mem[51706] = 1781457
      mask = 11X10011111X0X11X0X00XX100XX01000011
      mem[1827] = 352974
      mem[53395] = 1253519
      mem[5593] = 204616
      mem[24059] = 9680329
      mem[51280] = 893
      mem[18437] = 1179
      mask = 10110X101110X001001111X01X1110011000
      mem[27171] = 219265
      mem[47619] = 677
      mem[36434] = 924
      mem[10402] = 23151360
      mask = 1001X1111110001X10000X0X001110101101
      mem[19629] = 665
      mem[52439] = 250
      mem[8361] = 28482
      mask = 101110X110X0X0100110X0110X1010011X11
      mem[20558] = 558
      mem[26949] = 7777
      mem[22160] = 171308
      mem[15631] = 328260
      mem[45856] = 230942
      mask = 110100111100001X101X010000110X0X0010
      mem[30840] = 683
      mem[41251] = 652
      mem[10584] = 103692712
      mem[5860] = 3950330
      mem[61507] = 647961
      mem[43181] = 13247
      mask = X0110110X11011X01011X10001X010111X00
      mem[19238] = 39433935
      mem[19155] = 2409
      mem[16137] = 5550
      mask = 101X1XX0111XX01110100010X00110X01100
      mem[43920] = 947024008
      mem[2335] = 2498
      mem[15363] = 65058906
      mask = 110100X1X11X0011001X01X1000001010000
      mem[31189] = 13626
      mem[41212] = 161
      mem[53454] = 12060062
      mem[47394] = 8828
      mask = 1011X0X011100011X01X0X101000X1110010
      mem[64452] = 5096
      mem[4245] = 745302
      mem[46144] = 3353
      mem[25610] = 1258
      mask = 0101001XX110001101X00X10X1X001010X01
      mem[1677] = 23846073
      mem[50713] = 1754903
      mem[34243] = 619756
      mask = X10110111X1101110010001111111X0X001X
      mem[20746] = 11327
      mem[60677] = 55286634
      mem[18904] = 33122
      mem[42335] = 4994
      mask = X101100X1X10001X0X100011X011X0X01001
      mem[32832] = 20230
      mem[54205] = 1405755
      mem[27010] = 1483
      mem[25730] = 416967
      mask = 11110X11111X0X11000001X11XX0X0X00101
      mem[20547] = 993
      mem[32667] = 3520
      mask = 11010XXX011XX000101X1X0011X000001001
      mem[24634] = 245639
      mem[59394] = 184364
      mem[60948] = 20401794
      mem[9342] = 60279080
      mem[17142] = 10401929
      mem[40232] = 3268
      mask = XX011111111000X110XX00101X1X10100000
      mem[6405] = 2064
      mem[31624] = 208587070
      mem[2258] = 3965844
      mem[14742] = 6124264
      mem[2985] = 1824
      mem[4950] = 3019502
      mem[8036] = 157
      mask = 1X01001X11X00011X01001X01011XX01101X
      mem[41340] = 163017
      mem[46907] = 1047
      mem[2512] = 1340
      mem[33913] = 20303
      mem[58546] = 153629023
      mem[19536] = 109607
      mask = X01X0X10X1100001X01110101X0X1X00101X
      mem[25394] = 131964
      mem[18030] = 99092
      mem[19264] = 12572
      mask = X011X0X110101010011000X00010XX111000
      mem[12088] = 165
      mem[46867] = 6359415
      mem[52891] = 29571
      mem[37957] = 1880
      mem[38886] = 32297
      mem[47674] = 40454
      mask = 1010XX1011100011X01000X1X0111X011010
      mem[62632] = 184293833
      mem[43181] = 6802
      mem[29534] = 17997
      mem[55058] = 527832513
      mem[17560] = 876465884
      mem[33007] = 195444229
      mask = 111100X11X100X11X0100100X0X0X1000000
      mem[22371] = 620962
      mem[27171] = 13951
      mem[23941] = 285120
      mem[53371] = 2727
      mask = 101110011X111011X1000000X101X1X01X00
      mem[57437] = 4447669
      mem[62084] = 14810
      mem[20380] = 2
      mask = X01X100X10X11001X00001X00001X0001111
      mem[10604] = 29217
      mem[40103] = 20019
      mem[5593] = 1282
      mask = XX0100111X100011011X100XX11101X10111
      mem[29655] = 188308404
      mem[16108] = 987052
      mem[27684] = 1845758
      mem[8627] = 30072056
      mem[24904] = 28977792
      mem[24100] = 11388269
      mask = 11X1100XX01X001101100111110000100011
      mem[9003] = 63644817
      mem[23905] = 249772171
      mem[41608] = 566869
      mask = X001X0011110X011X0101X10001010000X1X
      mem[19837] = 1474559
      mem[6082] = 213612431
      mem[25394] = 746122
      mask = 101110X1111X101X01100000100101111X10
      mem[41906] = 2538917
      mem[43592] = 738
      mem[38721] = 69444
      mem[24036] = 6469
      mask = 0011100010X111X100001001XX000X000101
      mem[32354] = 709541
      mem[18383] = 3835385
      mem[1313] = 1178709
      mem[30236] = 6347401
      mask = 11011X001X10X0X1X0000100001010000000
      mem[64686] = 4029603
      mem[37187] = 269
      mem[41728] = 15291311
      mem[703] = 176262655
      mem[23732] = 473740
      mem[4238] = 2073061
      mask = 1101011111101X1X0010110X0XX110001X10
      mem[24443] = 487215
      mem[44567] = 898248
      mem[41636] = 11946
      mem[21254] = 1162908
      mask = X01110X11X1X101X01100X0000X01011X101
      mem[41555] = 13384162
      mem[51011] = 12307
      mem[54861] = 435
      mem[61876] = 10233
      mem[3849] = 14042
      mem[25473] = 273
      mem[10402] = 17072
      mask = 100100101110XX110X100110X010000XXXX1
      mem[13744] = 663718668
      mem[18696] = 361798
      mem[21207] = 851454
      mem[21194] = 21025
      mask = 1XX101XXX11X000X10111110111000X1X001
      mem[48482] = 107005
      mem[26156] = 15617
      mem[49747] = 65227757
      mem[52440] = 112190
      mem[36683] = 26159
      mem[19486] = 3181
      mask = X011X0X01XX1000101XX0010X11001110010
      mem[4510] = 7353062
      mem[10923] = 25067923
      mem[16714] = 173017
      mem[41162] = 12385087
      mem[25346] = 1388660
      mask = X10110011110001100101111001X000X1X01
      mem[27395] = 52406
      mem[63935] = 70016372
      mem[61748] = 1598
      mask = 1101101XX111011X001X00001111100X0111
      mem[2512] = 629921058
      mem[48026] = 247446319
      mem[48328] = 2018
      mem[9614] = 49871
      mem[25296] = 53536568
      mask = 10X110X0101X10011X00X010001XX0101011
      mem[62912] = 108898
      mem[22160] = 5738253
      mem[37655] = 5684656
      mem[292] = 103716917
      mem[40727] = 6480899
      mem[43121] = 2495962
      mem[8256] = 9752
      mask = 111X011111100000XX110110110X1X0X10X0
      mem[42306] = 3814
      mem[22173] = 104438687
      mem[60145] = 51698
      mem[44805] = 14155
      mem[23905] = 15867565
      mask = 1X01111X1X1001X110X000XX0000X0000010
      mem[7284] = 513301
      mem[14240] = 9298
      mem[9803] = 254768
      mem[36226] = 722
      mask = 10110111X1101000X011101001X10010101X
      mem[19105] = 1399632
      mem[40477] = 14030402
      mem[9180] = 258090132
      mem[2864] = 1722022
      mem[6880] = 31
      mask = 1011100X1X11X001X100X0100X100X1X0X10
      mem[3335] = 303249475
      mem[33869] = 89890
      mem[40724] = 3168
      mem[44588] = 3236692
      mem[43374] = 875137580
      mem[39986] = 327100054
      mem[16676] = 47668
      mask = 1011X00110111100011XX00X00X11111X001
      mem[46881] = 2251035
      mem[31188] = 860796
      mem[15313] = 88528
      mem[33666] = 645
      mem[9205] = 45363830
      mem[57565] = 604
      mem[13224] = 1342
      mask = 10X10X101X10011X101100X101X01X001000
      mem[25292] = 3155
      mem[18230] = 27276287
      mem[16051] = 270672
      mask = 1XX10001101X0011011011000101X11X0011
      mem[46144] = 8381238
      mem[22497] = 2356142
      mem[8433] = 70526
      mem[44424] = 8986
      mem[18442] = 508814
      mask = 101110011X1110X10XX000000X01X1X111XX
      mem[20373] = 1622495
      mem[44697] = 127381
      mem[20688] = 4500
      mem[16805] = 923777
      mem[32947] = 8080048
      mask = 0001X101XX10011110000X00000110X00000
      mem[31189] = 51277
      mem[1279] = 7010
      mem[40666] = 1361051
      mem[20337] = 6622
      mem[44409] = 245840
      mem[2587] = 231888105
      mask = 100X00111011XXXXX0X0100111X000010110
      mem[38644] = 122518
      mem[14690] = 853513
      mem[27280] = 499865495
      mask = 1XX1001011110110X011111011101XX0X100
      mem[5253] = 104635924
      mem[9220] = 469551991
      mem[54123] = 603
      mask = 1X01111111100X11101001X110X110100101
      mem[28857] = 4958
      mem[26793] = 43481
      mem[26110] = 2002
      mem[13670] = 737550271
      mem[48748] = 2175
      mem[15379] = 956329
      mask = X001111X101X01111010000100100000100X
      mem[5038] = 1896
      mem[1219] = 1590
      mem[2335] = 6839
      mem[44175] = 3246581
      mem[10128] = 861384
      mask = 10X100X1001X0X11011010100100X11000X1
      mem[25953] = 500230911
      mem[56860] = 3606219
      mem[3909] = 4193582
      mem[20077] = 13280564
      mem[42329] = 2622
      mem[19629] = 2920217
      mask = 1000X0111XX1XX11001000011X10110X0110
      mem[65286] = 10087505
      mem[39772] = 5587787
      mem[49314] = 25559
      mem[27912] = 135405924
      mem[46907] = 7910
      mem[36657] = 1549461
      mem[37088] = 74
      mask = 1101001XXX100X1100X0000010110010110X
      mem[26128] = 3098182
      mem[19238] = 6294862
      mem[35146] = 4761321
      mem[13782] = 407506
      mem[57435] = 1986
      mem[56396] = 1173362
      mask = 10X110011X11100X01101010X0X0X0010101
      mem[26478] = 5717
      mem[7980] = 895287
      mem[26719] = 254026
      mem[58541] = 444
      mem[43236] = 1222225
      mask = 10101010111100XX101000010X01X0101101
      mem[19246] = 1542
      mem[30987] = 1317750
      mem[15786] = 214473235
      mem[33835] = 989447
      mem[49063] = 63501903
      mem[50294] = 351534
      mem[4146] = 12924485
      mask = 1101100X11X00X1100X00X1X00011000X00X
      mem[49673] = 11520693
      mem[8936] = 422236363
      mem[22275] = 116474
      mask = 101110011X11X011000X0XX00X000X011X01
      mem[44344] = 518186197
      mem[20820] = 2429569
      mem[25541] = 30403114
      mask = 1X0X1011111X0111X0100111001111X00X10
      mem[47674] = 15040841
      mem[54205] = 9684
      mem[52058] = 38432
      mem[20458] = 522505115
      mem[16470] = 96407696
      mask = 101110001011X0010100X01011X0001000X0
      mem[2903] = 9215
      mem[24100] = 10412
      mem[65308] = 3449
      mask = 11110100011X00001011011X111101XX0001
      mem[1383] = 20524
      mem[43138] = 1853
      mem[53] = 13474143
      mem[53737] = 139742
      mem[36626] = 111978547
      mem[6473] = 296847
      mask = X00100X11010X01X011001XX101011001X00
      mem[14690] = 2176
      mem[49976] = 138885734
      mem[63369] = 58
      mem[24904] = 127774567
      mem[45788] = 935
      mem[15363] = 3333
      mask = 100100111X100011001X1011010111000XX0
      mem[44478] = 18487
      mem[25541] = 10395
      mem[2258] = 208737
      mem[33632] = 355267
      mask = 1X01001X1XX00111011000101XX10X01X100
      mem[46117] = 129267954
      mem[18953] = 496
      mem[3156] = 23246
      mem[22307] = 471295
      mem[56163] = 6733185
      mem[1324] = 2074915
      mask = 1X0100X1XX1000110X1001100001X10XX0X0
      mem[33913] = 191232
      mem[11074] = 586
      mem[6405] = 52666
      mask = X10X10X01110001X0110X011100100XX1001
      mem[2512] = 177970
      mem[58654] = 2392973
      mem[4166] = 3276
      mem[39753] = 1233
      mem[61980] = 474314617
      mem[37355] = 58269
      mask = X011100X1111X01100101000000X00110X1X
      mem[33118] = 5010
      mem[4760] = 111160
      mem[37689] = 823166
      mem[15678] = 1634
      mask = XX0101X111100111100000X0X0X0110X1XX1
      mem[17229] = 4665
      mem[22930] = 1040260
      mem[1324] = 260
      mem[31615] = 5434357
      mem[545] = 215622
      mask = 10110111X11X000110X10100110X0XX01110
      mem[44567] = 220705
      mem[53112] = 1089
      mask = X0010X101X1X11110X1X0110X0X1X0000000
      mem[7] = 1698581
      mem[53112] = 112679
      mem[63935] = 6201
      mask = 1010011011X1111X1011001000X010001X1X
      mem[27421] = 22548
      mem[20688] = 29314044
      mem[35943] = 66340108
      mem[17023] = 115252
      mem[47844] = 1957
      mem[20801] = 243380907
      mem[58453] = 1786
      mask = 10X11110X0XX0101X01010101101000X001X
      mem[56724] = 25322561
      mem[23838] = 128622093
      mem[14602] = 1871
      mem[9600] = 482868337
      mem[13919] = 427123809
      mask = 110100010110X011X11X11X001XX1X100010
      mem[26083] = 962249
      mem[48067] = 107
      mem[6405] = 979668
      mask = 10110X10111X0110101X010X110010X0000X
      mem[27181] = 581
      mem[1935] = 1700145
      mem[31816] = 7821892
      mem[62084] = 16529134
      mem[63391] = 3109
      mem[64545] = 30527
      mask = X101X11X11100111100XX0000X0X101X1001
      mem[8381] = 33407838
      mem[40566] = 15796331
      mem[18597] = 68546
      mem[25473] = 7358888
      mem[41562] = 4109
      mem[8797] = 4761
      mask = 1101001111100011X110X0010001X1X10001
      mem[48089] = 50505
      mem[8627] = 111289788
      mem[49063] = 7497
      mask = 10X1X011XX10X01X0110110X0X0101100011
      mem[6133] = 237
      mem[65302] = 776
      mem[24930] = 240012
      mem[49747] = 1455
      mem[25610] = 77345999
      mem[65463] = 830
      mem[18804] = 409066
      mask = 10X11001111X1001000X01XX11111101XX11
      mem[52439] = 54286
      mem[27158] = 334127093
      mem[50444] = 1667
      mem[1037] = 9375588
      mem[15887] = 95826
      mem[18383] = 4398280
      mask = XX01101111X101110X1X1X010101100001X0
      mem[38753] = 1944309
      mem[56903] = 103577
      mem[52840] = 85
      mem[47810] = 954770
      mem[31815] = 1660528
      mask = 101X011X1X11111110110000001010000X11
      mem[59096] = 180976584
      mem[64686] = 16261
      mem[31497] = 671389398
      mem[23216] = 4065503
      mem[55058] = 1248965
      mem[56396] = 3400500
      mask = 1X011011111001X1X01X000X10X1001X1010
      mem[20173] = 4586
      mem[61916] = 424482839
      mem[63391] = 6915265"""

}
