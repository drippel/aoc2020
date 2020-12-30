package com.pfs.advent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day21 {

  def main(args: Array[String]): Unit = {
    // day08 - last CPU
    // day12 - grid enhancements
    // day17 (and earlier) combine list of lists recursively with flatten
    Console.out.println("2020 21...")
    val ls = toLines(input)
    
    val fas = ls.map( parse(_))
    println(fas)
    solve(fas)
  }


  def solve( raw : List[(Set[String], Set[String])] ) = {
    
    // convert to a working list of stuffs
    val noOfFoods = raw.size
    val allIs = mutable.HashMap[Int,mutable.HashSet[String]]()
    val allAs = mutable.HashMap[Int,mutable.HashSet[String]]()
    
    for( r <- 0 until noOfFoods ) {
      val is = mutable.HashSet[String]()
      is ++= raw(r)._1
      allIs(r) = is 
      val as = mutable.HashSet[String]()
      as ++= raw(r)._2
      allAs(r) = as
    }
    
    // println(allIs)
    // println(allAs)
    
    // facts 
    val fToA = mutable.HashMap[String,String]()
    def addFact( f : (String, String) ) = { fToA += ( f._1 -> f._2 ) }
    
    def reduce() = {
      val fs = fToA.keys.toSet
      val as = fToA.values.toSet
      allIs.mapValuesInPlace( (k,v) => { v.diff(fs) } )
      allAs.mapValuesInPlace( (k,v) => { v.diff(as) } )
    }
    
    def findSingles() : List[(String,String)] = {
      val ss = ListBuffer[(String,String)]()
      for( r <- 0 until noOfFoods ) {
        if( allIs(r).size == 1 && allAs(r).size == 1 ) {
          val t = (allIs(r).head, allAs(r).head)
          ss += t
        }
      }
      ss.toList
    }
    
    val ss = findSingles()
    // add to facts
    // reduce
    println(ss)
    
    def findMatches() = {
      
      def innerFindMatches( idx : Int, accum : Option[(String,String)] ) : Option[(String,String)] = {
        if( accum.isDefined ) {
          accum
        }
        else if ( idx >= noOfFoods - 1 ) {
          None
        }
        else {
          val ts = (allIs(idx), allAs(idx) )
          val ids = ( idx + 1 until noOfFoods ).toList
          val ots = ids.map( i => (allIs(i), allAs(i) ) )
          val inters = ots.map( t => ( t._1.intersect(ts._1), t._2.intersect(ts._2) ) )
          val nextAccum = inters.find( t => t._1.size == 1 && t._2.size == 1 ) match {
            case Some(t) => { Some( (t._1.head, t._2.head)) }
            case None => { accum }
          }
          innerFindMatches( idx + 1, nextAccum )
        }
      }
      
      innerFindMatches(0, None)
      
    }
    
    val ms = findMatches()
    println(ms)
    
    // skip this for now
    var cont = false 
    while( cont ) {
      println("reduce")
      val ss = findSingles()
      if( ss.isEmpty ) {
        val ms = findMatches()
        if( ms.isEmpty ) {
          cont = false
        }
        else {
          // add to facts reduce
          addFact(ms.get)
          reduce()
        }
      }
      else {
        // add to facts and reduce
        ss.map(addFact(_))
        reduce()
      }
    }
    
    println(fToA)
    println(allIs)
    println(allAs)
    
    // get all the unique allergens
    val allAllergens = mutable.HashSet[String]()
    val as = allAs.values.toList 
    as.foreach( allAllergens ++= _ )
    println(allAllergens)
    
    val possMap = mutable.HashMap[String,mutable.HashSet[String]]()
    
    // get all the foods that contains shellfish
    for( a <- allAllergens ) {
      
      val ids = allAs.filter( t => t._2.contains( a ) ).map( t => t._1 ).toList.sorted

      // get all the ingredients in common for those foods
      val is = ids.map( allIs( _ ) )
      val (h, t) = (is.head, is.tail)
      val possible = t.foldLeft( h )( _.intersect( _ ) )
      possMap += ( a -> possible )
    }
    
    possMap.foreach( println(_))


    val aToF = mutable.HashMap[String,String]()
    
    while( !possMap.isEmpty ){
      
      // get any singles out
      val ss = possMap.toList.filter( _._2.size == 1 )
      
      // add these to the facts
      val ks = ss.map( t => t._1 -> t._2.head )
      aToF ++= ks
      
      // update the food mappings
      val as = ks.map( _._2 ).toSet
      possMap.mapValuesInPlace( (k, v) => { v.diff(as) })
      
      // remove the keys from possibles
      ss.foreach( t => possMap.remove( t._1 ))
      
    }
    
    println(aToF)
    val isSolved = aToF.values.toSet
    
    // solved
    val is = allIs.map( _._2 ).toList
    val isNoAllergens = is.map( _.diff(isSolved)) 
    val isNotAllergensCounts = isNoAllergens.map( _.size )
    val sum = isNotAllergensCounts.foldLeft(0)( _ + _ )
    println(sum)
    
    val asSorted = aToF.map( _._1 ).toList.sorted
    println(asSorted)
    val dangerSorted = asSorted.map( aToF(_) )
    val sol = dangerSorted.mkString(",")
    println(sol)
    
    
    
    
  }
  
  
  def parse( line : String ) : (Set[String], Set[String]) = {
    
    val s = line.replace("(", "").replace(")", "").replace(",", "").replace("contains", "|")
    
    val ps = s.split('|').toList.map(_.trim)
    
    val fs = ps(0).split(' ').toList
    val as = ps(1).split(' ').toList
    
    val s1 = fs.size
    val s2 = fs.toSet.size
    if( s1 != s2 ) {
      println("ooooooooooooops")
    }
    
    (fs.toSet,as.toSet)
  }
  
  def toLines(src : String ) = src.split("\n").toList.map(_.trim).filter( s => !s.isEmpty ) 
  
  val test =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      trh fvjkl sbzzf mxmxvkd (contains dairy)
      sqjhc fvjkl (contains soy)
      sqjhc mxmxvkd sbzzf (contains fish)"""
  
  val input =
    """rgt lmpxcr ssgkt klfp dfpqn scqc vzzxl hphcb pbmhl xrlmvz fvd cpj kxl gpplt jkrsn bhlbm dkxr vrzkz drpxzr bxhsq cvz nvhnsg jtzsqznl fjbjj slglnd qdbq gnrb zpbbvvm bqb mbdksj nlxh gtzp jrlvkz ctmzsr znjp zvmnv lhsgbq pdxhgx jrcczb vrpzss qdbj lsc jhrgz hqlm qlt kh grjf brhfcbt zjsh pkmbk rncjx nmszz fzpds qglps zjsz qfnq ftfx bbkth rkzqs vgp (contains nuts, shellfish)
      slmjchql qfnq lqdpl bgdq tjxp rkzqs zjsh znklc qdbj pbtns cpsbgsh qbtpnr nmszz vzzxl vrzkz vtrltkq znfrgvc dksfbk cvz qfvvf cvtj dmpl qnt hbbz zmhnj dfpqn rncjx scqc bccz hdnq zpbbvvm lnpbh pdxhgx nqstv fhpxz ftfx rgt ztrpkb lqtnn dqdn vlqtzzl msvgj dkxr fptpk bbkth jgkp hmshx xrlmvz trlmqn znjp vgp ztlbvp klfp slglnd rmthpj nfzczn hdkc nvhnsg cntfbt rmpnm bqfqcjb tkrr jgxbzks sks pchhkp qsrh kh mbdksj ctmzsr drpxzr xlqj bhlbm (contains soy, eggs, shellfish)
      zjdzx cpj bqfqcjb bqb ztlbvp hdkc hphcb zjsh pkgsf qllxhr grjf rgt vfnv gtzp sbsrk hdnq vzzxl httx vgp cntfbt ghfk hmshx rzjcs dbfk xfvtzq nvhnsg znfrgvc bbkth svrx rcv bgdq hqjgzmt rkzqs nmq lqdpl zcxxdp dxpdkp nqstv vrtfrj zhvsdc rsrgmkr nlxh lmpxcr rmpnm kxl qzbzd fzpds sltl cvtj zvmnv jgxbzks vrzkz cpsbgsh zdrlf cqbkj nmszz jrcczb qvhdl dlc sks lvjrbms vfhvp lhb knhvxm qlt dqdn ssgkt slmjchql nvbr cgrcmp pkmbk vllxj spzmvfl rvnfn ctmzsr cvz zqgzlr zmhnj fzgbq bhlbm (contains shellfish)
      pznl nmszz vtrltkq qpgj zqgzlr lnpbh gpplt rkzqs gmx bqb jgv hphcb dfpqn zcxxdp tkrr qbtpnr mbdksj hxfrh vfnv hmshx fmkbnr gnrb sltl kh drpxzr vzzxl fptpk ftfx ctmzsr jrnfp dksfbk qsrh rnxl czlpq dpkdp qzbzd dxpdkp fvpj tnjxcf zdrlf bxhsq bhlbm nlxh hqjgzmt qglps cf qllxhr cqbkj jmxpbtk nvdst slmjchql qnt dnzljk ckzdf vrzkz pbmhl ssgkt knhvxm gjvqffm xzhct xvmftv zmf qbnkj ltxqz mxk njcjm knqlqt bccz dbfk dlc vllxj rph frz zmhnj pkmbk (contains soy, sesame, fish)
      gbzcv znjp xvmftv pjgxpg bhlbm spzmvfl xfvtzq vrzkz bqb brhfcbt rmthpj hphcb vfnv knqlqt slmjchql vrtfrj fzgbq hxfrh rxzxs cgrcmp dg msvgj xcdb dksfbk zmhnj gjvqffm jsdcr rzjcs jgkp klfp flhzd dnzljk nmszz qbnkj bbkth hdkc vlqtzzl fptpk hmshx jrcczb lhsgbq tcp fvvjp cvz lhb rls rcv pspjgzx gtzp cntfbt ftfx qpgj vzzxl hbbz qfnq ztlbvp dxpdkp lmpxcr frnh qzbzd ctmzsr rkzqs svrx znklc zjsh knhvxm rnxl qbtpnr jmxpbtk zjsz vgp zpbbvvm pkgsf jrnfp rvnfn gpplt sks vtrltkq (contains sesame, fish)
      dmpl cpsbgsh msvgj qdbq tqtbd fptpk frnh zjdzx hbvqn rls bhlbm lhsgbq znklc rph bqfqcjb rmthpj dlc tkrr qfvvf xzhct dxpdkp qpgj rkzqs fvd ltxqz sltl zjsh ctmzsr kxl bbkth cvz znfrgvc jgkp zmhnj hphcb qvhdl vrzkz sbsrk jgxbzks qzbzd cgrcmp hqlm jmxpbtk mkpxx drpxzr kh rncjx qbnkj dbfk tjxp fvpj vpl nmszz jsdcr vzzxl dksfbk lqtnn pmhnmxr gnrb vkjggtx gmx cf (contains dairy, peanuts, sesame)
      qfvvf cntfbt cvz fvpj hxfrh vrzkz lsc zmf cgrcmp httx qvhdl xvmftv dksfbk zvmnv snxz lvjrbms zcxxdp gjvqffm pdxhgx czlpq spzmvfl knqlqt ctmzsr zhvsdc vzzxl rkzqs zjsh msvgj zmhnj nlxh dxpdkp hphcb dnzljk bqb lqdpl nvdst rcv tqtbd xfvtzq mkpxx (contains soy, eggs, peanuts)
      slmjchql zjsh qfnq lqdpl zmf vgp rkzqs cgrcmp snxz drpxzr zqgzlr cpsbgsh zvmnv vzzxl mxk knhvxm lsc dpkdp ltxqz xvmftv dnzljk ctmzsr spzmvfl pmhnmxr kh mbdksj gpplt znjp lvjrbms pkgsf hdkc vrzkz vfhvp znklc dg rcv jgxbzks fhpxz hbvqn qvhdl nqstv qglps dfpqn znfrgvc rncjx bqb gtzp xlqj zjsz pspjgzx zmhnj tjxp lmpxcr (contains nuts, sesame)
      zqgzlr hhth scqc jtfnm qdbq cf zcxxdp rxzxs rlxhc zdrlf qfnq slglnd dbfk fzgbq cpj rncjx zjsz pmhnmxr kxl qllxhr bqfqcjb dlc flhzd klfp dpkdp vrzkz zjdzx fptpk vzzxl rkzqs zhvsdc mbdksj snxz jsdcr xlqj qbnkj pznl tqtbd dqdn slmjchql xvmftv fjbjj vkjggtx qlt rnxl gpplt qpgj nvbr nlxh nqhk vfhvp hphcb zjsh qglps rph lhsgbq zvmnv rzjcs pchhkp frz dfpqn kh qbtpnr zmhnj drpxzr bccz cgrcmp msvgj cqbkj hdnq vttzh lsc dmpl pkgsf rmpnm pbtns xfvtzq (contains shellfish, dairy)
      hphcb hxfrh fptpk tsnhq ctmzsr qnt xvmftv nvbr zmf sks czlpq dlc cpsbgsh jtfnm znklc bhlbm bgdq qbtpnr nvhnsg slglnd jkrsn ghfk ztrpkb njcjm znjp jrnfp gpplt cpj tcp pdxhgx kxl mbdksj rnxl nvdst bxhsq zjdzx vzzxl rsrgmkr rvnfn rcv zmhnj xrlmvz dbfk tjxp ghtjbp rph zjsh rkzqs spzmvfl vlqtzzl (contains shellfish, soy)
      hphcb sbsrk qvhdl hbvqn cpsbgsh nvhnsg mkpxx ssgkt znklc cf jgv vzzxl dnzljk ttr pbtns cgrcmp rnxl cvz znjp fptpk kh zdrlf zmhnj zjsh svrx snxz slglnd lsc pdxhgx rmpnm cqbkj pznl frnh bxhsq bqfqcjb ghfk dxpdkp qbnkj hkzzg hbbz rkzqs grjf dbfk jkrsn zcxxdp ctmzsr nvbr hhth mbdksj lhb vrtfrj brhfcbt flhzd zjdzx hdkc nlxh jrlvkz dfpqn (contains nuts, peanuts, shellfish)
      hqjgzmt njcjm rlxhc brhfcbt fvvjp zjsh ghtjbp dnzljk qsrh zmhnj brhns pznl vzzxl jrnfp lnpbh rzjcs svrx pkmbk nfzczn pbtns nvhnsg pmhnmxr hdnq gjvqffm pjgxpg bxhsq vllxj lhb dksfbk tcp ssgkt vrzkz nvbr qzbzd dmpl pbmhl lmpxcr hdkc dpkdp rkzqs zdrlf sbsrk knqlqt pspjgzx qbnkj rph cqbkj drpxzr mbdksj ctmzsr dfpqn bccz kh vlqtzzl hbbz vfhvp (contains sesame, nuts)
      pchhkp jkrsn hbvqn vzzxl qsrh ctmzsr rzjcs cgrcmp xzhct zvmnv cntfbt dlc zqgzlr znfrgvc hqjgzmt dqdn pkmbk nmq zmhnj qdbj vrzkz qbnkj fzpds vlqtzzl qvhft bhlbm bccz rvnfn zjsh mbdksj lnpbh pbtns svrx rkzqs zhvsdc qbtpnr ltxqz dksfbk (contains dairy, shellfish)
      hkzzg vtrltkq nmszz hhth pjgxpg jgxbzks rcv cgrcmp tsnhq zmhnj dqdn fhpxz ltxqz pdxhgx ssgkt jrnfp gtzp tnjxcf qdbq hphcb dnzljk ghfk znklc nvdst tcp brhfcbt xzhct vzzxl nvhnsg ttr bbkth hdnq nmq rph jtzsqznl czlpq gnrb ctmzsr ftfx pkgsf msvgj zjsh sltl lsc qnt pbmhl cqbkj gztf vrzkz jrcczb zdrlf qfnq jtfnm qbnkj frnh jrlvkz hbbz klfp njcjm rvnfn pmhnmxr qglps sbsrk mbdksj nllks (contains nuts)
      spzmvfl fvvjp cvtj qsrh pspjgzx vzzxl svrx rkzqs vfnv xcdb jgv hdkc zmhnj ghtjbp fptpk frz lsc dmpl bhlbm qpgj sbsrk dnzljk vttzh hkzzg tsnhq ctmzsr dpkdp jgkp bxhsq lnpbh ztrpkb zjsh mxk jtzsqznl zpbbvvm vrtfrj rcv rph bgdq lhb bccz rls jhrgz nvdst hhth qnt vpl kh mbdksj zmf rncjx knqlqt flhzd jmxpbtk jrnfp lqtnn tnjxcf cpj fmkbnr tcp dfpqn hdnq vrzkz (contains shellfish, dairy)
      nqhk jrlvkz pjgxpg nvbr qvhdl rmthpj nqstv xcdb gpplt rgt vgp tjxp rlxhc rnxl tqtbd hqlm dkxr dbfk knqlqt msvgj scqc klfp httx hdnq hkzzg fmkbnr vllxj nmszz zjsh mkpxx ctmzsr dxpdkp fzpds xfvtzq nvhnsg kh qfvvf fzgbq dpkdp slmjchql knhvxm rcv mbdksj rzjcs vzzxl ghtjbp qzbzd jsdcr cpj pkmbk pdxhgx fptpk nlxh flhzd vrzkz qpgj spzmvfl jrnfp zmf ttr fvvjp qglps lhsgbq rkzqs snxz qfnq fvpj bbkth gztf lqtnn jgxbzks tkrr pbtns gjvqffm jgv nmq drpxzr czlpq kxl svrx hphcb (contains nuts)
      rph jkrsn dmpl tcp vrpzss jgkp cntfbt mkpxx spzmvfl dnzljk fvd cpj zmhnj cqbkj ltxqz tsnhq ctmzsr flhzd vtrltkq rkzqs pspjgzx hdnq vzzxl hbbz trlmqn cvtj fjbjj vrzkz gnrb hqjgzmt mbdksj lqdpl xrlmvz lnpbh jrlvkz qpgj kxl nvhnsg sbsrk svrx klfp vllxj slmjchql rncjx hkzzg bqfqcjb xvmftv qdbj dqdn nqhk tnjxcf vlqtzzl zcxxdp hxfrh vkjggtx pznl nmq qfvvf qdbq zjsh scqc (contains sesame, dairy, soy)
      rkzqs pchhkp jkrsn hqjgzmt qzbzd hbvqn zpbbvvm rph hphcb gmx njcjm nvbr qllxhr pbmhl ttr gpplt gbzcv vfhvp bccz hbbz nlxh mxk ltxqz vrzkz zjsz httx pmhnmxr fzgbq nvdst lqtnn vzzxl rvnfn pkmbk xzhct zqgzlr ctmzsr pznl bgdq znjp vllxj fvpj fvvjp mkpxx tkrr fhpxz tsnhq zjsh vttzh rmpnm gztf qbtpnr klfp cgrcmp fmkbnr jgv qglps mbdksj knhvxm sks kxl ckzdf cf (contains fish, sesame, nuts)
      fjbjj nqstv bqb dg vfnv gmx vzzxl qglps gpplt zjdzx zvmnv ctmzsr bqfqcjb xcdb tjxp gjvqffm hhth jrcczb bgdq cqbkj spzmvfl sltl qfnq vtrltkq dmpl qlt hphcb hdnq flhzd zmhnj vpl msvgj vrzkz dkxr jsdcr hbbz bbkth tsnhq svrx jrnfp lhsgbq slglnd gtzp cvtj ckzdf ztlbvp pspjgzx pbtns lmpxcr jmxpbtk mkpxx bxhsq ghfk rmpnm qllxhr dqdn fvpj ftfx vkjggtx qvhdl klfp qnt hdkc jtfnm zpbbvvm lqtnn nllks dfpqn vlqtzzl hbvqn jgkp hqlm grjf lsc pkgsf kxl qbtpnr qzbzd rkzqs nmszz zjsh (contains eggs, soy, dairy)
      knqlqt kh dlc vkjggtx jtfnm dnzljk svrx zmhnj dfpqn gpplt qvhft qsrh qzbzd hqlm kxl lqtnn qglps lvjrbms dmpl spzmvfl slmjchql knhvxm jmxpbtk ztlbvp vfhvp qnt rmthpj mbdksj xfvtzq lqdpl fjbjj vzzxl slglnd fhpxz snxz zhvsdc pbmhl nlxh ssgkt lhsgbq ckzdf tcp znfrgvc rkzqs lhb ctmzsr jrnfp frnh tjxp zjsh cgrcmp ftfx mxk rls vpl hphcb xcdb fptpk rvnfn fzgbq hkzzg vlqtzzl jkrsn ghfk vfnv (contains soy, fish)
      vfhvp rmpnm tsnhq sks cntfbt qdbj rzjcs vllxj pjgxpg gpplt mxk brhfcbt qsrh zqgzlr qdbq lhb fjbjj dlc ztlbvp rcv dg dxpdkp tqtbd zjsz pdxhgx zjsh znklc xvmftv rkzqs zcxxdp gmx cqbkj dqdn zmf vrtfrj ckzdf hdkc qpgj jsdcr bccz slglnd brhns bhlbm vrzkz jkrsn zvmnv nmq hbvqn rvnfn qvhft pbtns jrnfp knhvxm dfpqn spzmvfl rgt httx hphcb mbdksj bbkth jrlvkz zmhnj hxfrh rls fhpxz rlxhc jgv vlqtzzl fmkbnr tnjxcf tjxp jhrgz qbnkj svrx jmxpbtk tkrr nfzczn ctmzsr knqlqt qlt kh cvtj (contains dairy, fish)
      mbdksj dg hphcb jsdcr qvhdl bgdq pdxhgx vllxj jgxbzks ctmzsr hdkc hbvqn fhpxz znfrgvc cf svrx scqc rvnfn xlqj sks qvhft jrcczb zmf nqstv vzzxl jmxpbtk lvjrbms rcv tkrr vrtfrj nvbr bccz vlqtzzl qglps rkzqs zmhnj qzbzd pznl cpj cntfbt czlpq zjsz lqtnn zqgzlr slglnd jgkp vrzkz dpkdp sltl ztlbvp jrlvkz (contains shellfish, soy, peanuts)
      rncjx dg vfnv vzzxl pkgsf mbdksj jrcczb ltxqz zmhnj vtrltkq dfpqn gjvqffm dksfbk qbnkj slglnd nvdst hdnq ckzdf qlt hphcb mxk scqc sltl qfvvf jgxbzks cgrcmp bhlbm ttr njcjm cvz nvbr qglps vpl tcp trlmqn dnzljk pmhnmxr rlxhc zhvsdc lsc nvhnsg fhpxz gnrb brhns cf ctmzsr fzgbq fzpds fvd gztf drpxzr ssgkt pbtns rkzqs rmthpj fjbjj rmpnm bccz brhfcbt jmxpbtk zpbbvvm fvvjp zjsh rzjcs (contains eggs, shellfish)
      zmhnj hxfrh scqc tqtbd dqdn zjsh rcv sbsrk ctmzsr slglnd sltl zdrlf pspjgzx qbnkj gztf vrtfrj trlmqn rncjx bhlbm pchhkp qglps tsnhq fvvjp ztlbvp fptpk nvbr lmpxcr pkgsf xvmftv rvnfn qnt spzmvfl jrlvkz dpkdp flhzd ssgkt snxz rph rkzqs fjbjj gpplt vzzxl hphcb gbzcv nfzczn vrzkz jtzsqznl kxl (contains soy)
      zjsh scqc vlqtzzl fzgbq xlqj dxpdkp jgv tjxp qdbj jhrgz kh znjp mbdksj jgkp xfvtzq rzjcs hdkc dksfbk jmxpbtk ftfx lvjrbms nmq svrx ztlbvp xvmftv cvtj zhvsdc cpj cvz hmshx zmhnj rkzqs zjsz jtfnm slmjchql zmf vzzxl cf jkrsn lqdpl ctmzsr fmkbnr hdnq mxk dg hphcb gtzp qdbq vrtfrj vfhvp nllks znfrgvc vrpzss ghtjbp bqb fhpxz jtzsqznl (contains sesame, peanuts)
      pdxhgx rmthpj jrlvkz lqtnn pmhnmxr jmxpbtk vgp zjsh rncjx fvpj vrzkz rkzqs fptpk pbtns qzbzd bhlbm znfrgvc qbtpnr gtzp dbfk jsdcr pkmbk spzmvfl lhsgbq tqtbd gnrb vzzxl gpplt knqlqt frnh dksfbk qvhft lhb bqfqcjb hkzzg qfvvf scqc klfp cf mbdksj slmjchql dpkdp kh tsnhq ctmzsr rlxhc ssgkt hdnq rph rxzxs jgkp hbbz cqbkj xvmftv cpsbgsh pznl dnzljk zmhnj nvhnsg ztrpkb msvgj brhfcbt pchhkp rcv gmx rvnfn tcp zpbbvvm dlc cvz rzjcs (contains dairy, shellfish)
      qbnkj zcxxdp qglps dnzljk dkxr jgkp frnh knhvxm ctmzsr bgdq zhvsdc vrzkz dqdn tqtbd vzzxl xzhct slglnd rkzqs znfrgvc qfvvf snxz fvd cpsbgsh trlmqn gbzcv tsnhq qbtpnr rzjcs pmhnmxr nmq nvhnsg mbdksj knqlqt vgp njcjm brhns zjsh pkgsf zmhnj (contains sesame, nuts)
      spzmvfl zjsh fmkbnr qbtpnr pdxhgx fzpds qpgj vrzkz mkpxx jkrsn pchhkp tkrr zvmnv pspjgzx zmhnj hphcb rvnfn cf pmhnmxr nvdst frz qdbq fptpk fhpxz fzgbq nmszz qvhft gtzp vzzxl vlqtzzl dqdn pbmhl vkjggtx hqlm nmq trlmqn vtrltkq dpkdp lqdpl rkzqs ctmzsr qzbzd dksfbk fvvjp lvjrbms dkxr (contains soy, sesame, dairy)
      ctmzsr ssgkt msvgj xcdb tqtbd ghtjbp scqc nlxh nqhk vzzxl jrcczb tjxp vrtfrj ckzdf fvpj vfhvp jtzsqznl qpgj zqgzlr jmxpbtk ghfk sks gtzp zmhnj mkpxx vfnv jsdcr frnh zjsh rkzqs vrzkz httx rls qsrh xvmftv knqlqt jhrgz brhns frz qbtpnr njcjm fvd hbvqn rmthpj bxhsq lnpbh cpsbgsh bqb tsnhq rvnfn hqjgzmt slglnd lqdpl hphcb jgv cgrcmp slmjchql xrlmvz jgxbzks cvz qfvvf pbmhl cntfbt pznl rzjcs zvmnv cpj dksfbk gpplt znklc kh qfnq rmpnm tnjxcf dqdn ftfx qvhft ztlbvp jrnfp dfpqn hbbz pbtns bccz ttr (contains dairy)
      qbnkj bqb zmf lsc xfvtzq xrlmvz jgxbzks cvtj fvvjp gbzcv zqgzlr zmhnj vtrltkq jkrsn cvz kh lqdpl qzbzd rsrgmkr zjsz gpplt qlt cf fvd bbkth pkmbk pbmhl zhvsdc dmpl xcdb rmpnm rnxl lhb sks ssgkt vrzkz brhfcbt xzhct vzzxl pjgxpg ftfx fhpxz njcjm scqc hqjgzmt rkzqs lnpbh zjsh fzpds ctmzsr brhns trlmqn vlqtzzl lmpxcr jrcczb vllxj hphcb hxfrh rls qdbq znklc tsnhq dxpdkp cpj bhlbm zjdzx czlpq (contains eggs)
      hhth vkjggtx nmq zmhnj dmpl spzmvfl qfnq vzzxl pkgsf fjbjj rvnfn pkmbk frnh knhvxm vrzkz ctmzsr xcdb mbdksj nvdst cf fvd flhzd kxl cvz qbnkj hkzzg jtzsqznl rnxl sbsrk klfp cqbkj jsdcr rkzqs jhrgz nmszz slglnd nqstv qdbj zjsh tkrr zmf (contains nuts)
      nvbr ghtjbp nvdst fvd mkpxx xlqj dkxr znfrgvc mbdksj vfnv rph zpbbvvm gpplt ctmzsr rsrgmkr cvz ztlbvp rkzqs bbkth nfzczn hphcb httx zjsh zmhnj zhvsdc qsrh jsdcr gztf jrnfp trlmqn bccz pchhkp lhb qbnkj fjbjj dpkdp dxpdkp cgrcmp vlqtzzl zjsz rlxhc jgkp pkgsf jrlvkz nqhk tsnhq czlpq ckzdf vrzkz (contains nuts)
      tjxp xvmftv znklc ghfk svrx pbtns rgt fzgbq cgrcmp qvhdl lvjrbms rmpnm zmf cvz vzzxl bhlbm qllxhr pznl dfpqn ltxqz mbdksj bbkth jsdcr dkxr hdnq kxl nfzczn vpl jhrgz snxz trlmqn vrpzss njcjm gpplt zjsh dqdn ftfx zmhnj brhfcbt dmpl ztlbvp ctmzsr xcdb frnh nmszz jrcczb cpsbgsh vkjggtx knhvxm xfvtzq bxhsq xrlmvz rph jrnfp fhpxz cqbkj pdxhgx bqfqcjb lnpbh nmq frz gjvqffm klfp lqdpl rkzqs httx qdbj vrzkz pchhkp fzpds sks ttr gmx tqtbd (contains sesame, soy)
      lnpbh czlpq cpsbgsh zjsh rsrgmkr jmxpbtk vtrltkq cqbkj bxhsq vrzkz rnxl znfrgvc hqjgzmt pkgsf rlxhc dmpl lqdpl qbtpnr ghtjbp drpxzr scqc ztlbvp dxpdkp znklc kxl hphcb ctmzsr cgrcmp xlqj tkrr mkpxx rkzqs qvhdl qdbq gjvqffm cf gztf qglps gbzcv zpbbvvm frnh vfnv qbnkj vfhvp sks zvmnv mbdksj jrlvkz qlt rvnfn vkjggtx zmhnj (contains nuts, sesame)
      pkgsf rkzqs ckzdf fvvjp lhsgbq tsnhq tkrr qvhft qnt jtfnm vrzkz mkpxx lqtnn nllks qdbj pmhnmxr qdbq httx nmq pjgxpg bqfqcjb lmpxcr vlqtzzl rzjcs rncjx rvnfn cgrcmp ctmzsr rmpnm dmpl gnrb dfpqn jhrgz zjdzx nlxh zvmnv fvpj zjsh drpxzr mxk hbvqn vrtfrj jrnfp ghtjbp trlmqn dlc bccz hbbz nqstv vttzh pkmbk fzgbq bgdq ssgkt vrpzss vzzxl mbdksj hhth zmhnj dxpdkp rmthpj lqdpl (contains soy)
      ltxqz vttzh frnh hdkc rmthpj hxfrh bgdq jtfnm qbtpnr rlxhc tnjxcf lvjrbms ttr scqc dxpdkp rncjx hphcb vkjggtx qfvvf qzbzd jrnfp xlqj jrlvkz lsc rvnfn knqlqt zjdzx hqlm gbzcv qbnkj mbdksj qnt qpgj dkxr gjvqffm vllxj hmshx znfrgvc nvhnsg tqtbd hkzzg zpbbvvm cpsbgsh zdrlf sltl pkmbk rzjcs fzgbq trlmqn vzzxl czlpq rkzqs fptpk jgxbzks nqhk slmjchql vrtfrj ftfx ghfk fhpxz ctmzsr fmkbnr rls rcv snxz qlt zqgzlr zmhnj bqb nlxh vrpzss pchhkp vrzkz (contains soy)
      jrcczb xfvtzq nmszz sbsrk dpkdp pdxhgx cgrcmp qbnkj mbdksj rkzqs scqc rxzxs xzhct mkpxx ttr qglps vzzxl xlqj jsdcr pbtns qzbzd fvvjp vllxj zjsz frnh gnrb vpl zdrlf gztf vrpzss vrzkz dmpl hphcb gbzcv rgt kxl ctmzsr nlxh zjsh lvjrbms sltl brhfcbt lsc pmhnmxr fhpxz qsrh vkjggtx zcxxdp drpxzr jgxbzks (contains peanuts)
      tcp vlqtzzl zjsh rkzqs drpxzr cqbkj ftfx gbzcv xvmftv fptpk rncjx rnxl jtfnm nqstv znfrgvc jsdcr zmhnj jrnfp qfnq hphcb vzzxl bbkth flhzd jgxbzks ssgkt xcdb fvpj rlxhc ztrpkb qsrh qbtpnr bqfqcjb xzhct pznl hdnq qlt tjxp lvjrbms fhpxz cpj dpkdp mbdksj qpgj vrzkz xrlmvz rvnfn snxz hdkc ltxqz zjsz hxfrh pmhnmxr (contains nuts, fish, peanuts)
      vrtfrj ctmzsr zpbbvvm nvdst qvhdl rlxhc pbtns tcp lsc vrzkz dqdn hmshx fptpk rph rgt vllxj dbfk dkxr ttr nmszz qvhft znjp fhpxz jgv ftfx zjdzx zjsh znfrgvc qdbj vkjggtx znklc lhsgbq jsdcr frz pspjgzx pkmbk vpl pkgsf vfnv dnzljk hqlm klfp zmhnj njcjm pdxhgx dlc cntfbt ghtjbp qpgj rkzqs vtrltkq jrcczb fzgbq fjbjj vzzxl cgrcmp lvjrbms vlqtzzl hphcb svrx dmpl dxpdkp pjgxpg bbkth ztlbvp rncjx cqbkj (contains shellfish, eggs, soy)
      spzmvfl bccz xzhct qdbj qllxhr jgv rkzqs slglnd qlt tjxp rncjx rnxl fvpj fjbjj zhvsdc qnt nvbr mbdksj tkrr hphcb rcv znklc qdbq fzgbq nlxh rph rlxhc msvgj lhsgbq dfpqn cqbkj vrzkz znfrgvc tqtbd cntfbt zmhnj vrpzss xrlmvz flhzd zjsh httx znjp nmszz pbmhl pspjgzx ctmzsr kxl ftfx qzbzd zpbbvvm scqc jtfnm njcjm fvvjp (contains soy, shellfish, fish)
      brhns bbkth jtfnm znklc xzhct jrnfp tcp fptpk cqbkj mkpxx vpl ssgkt jgkp rmthpj hxfrh fvvjp zjsh pkgsf fzpds zmhnj tqtbd ltxqz rkzqs pmhnmxr rnxl gztf qvhft rxzxs qsrh ctmzsr fvd sks qbnkj xrlmvz rzjcs zmf hbvqn qbtpnr mbdksj nlxh nvdst ghtjbp slmjchql hphcb lhb zvmnv xcdb vllxj hmshx znjp vrpzss dbfk pchhkp klfp lqtnn bxhsq slglnd vlqtzzl cvz zdrlf vrzkz cpsbgsh nvhnsg pkmbk fzgbq dfpqn dlc fmkbnr cvtj (contains dairy)
      jtzsqznl bhlbm lsc lhb zpbbvvm ftfx znjp hphcb nqstv zjsh dksfbk pchhkp fzgbq pmhnmxr vrpzss knhvxm mbdksj pbmhl rkzqs httx bqfqcjb qvhft lqdpl jrlvkz zmhnj sks qbnkj nvdst czlpq pznl tnjxcf tkrr vrzkz hkzzg cf zdrlf nfzczn jrnfp gmx spzmvfl vzzxl rnxl znfrgvc dqdn jgxbzks jtfnm xcdb (contains nuts)"""

}