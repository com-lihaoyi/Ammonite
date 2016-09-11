package ammonite.kernel

import KernelTests._
import org.scalatest.FreeSpec

class EulerTests extends FreeSpec {

  val kernel = buildKernel()

  "p1" in {
    // Add all the natural numbers below one thousand that are multiples of 3 or 5.*
    checkSuccess(kernel, Vector(
      ("(1 until 1000).view.filter(n => n % 3 == 0 || n % 5 == 0).sum", checkInt(233168))
      ))
  }

  "p2" in {
    // Find the sum of all the even-valued terms in the
    // Fibonacci sequence which do not exceed four million.*
    checkSuccess(kernel, Vector(
      ("lazy val fs: Stream[Int] = 0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2)", checkUnit),
      ("fs.view.takeWhile(_ <= 4000000).filter(_ % 2 == 0).sum", checkInt(4613732))
      ))
  }

  "p3" in {
    // Find the largest prime factor of a composite number.
    checkSuccess(kernel, Vector(
      ("""
        def factors(n: Long): List[Long] = {
          (2 to math.sqrt(n).toInt)
          .find(n % _ == 0)
          .map(i => i.toLong :: factors(n / i)).getOrElse(List(n))
        }
        """, checkUnit),
      ("factors(600851475143L).last", checkLong(6857L))
      ))
  }

  "p4" in {
    // Find the largest palindrome made from the product of two 3-digit numbers.*
    checkSuccess(kernel, Vector(
      ("""
        {
    (100 to 999).view
                .flatMap(i => (i to 999).map(i *))
                .filter(n => n.toString == n.toString.reverse)
                .max
    }
        """, checkInt(906609))
      ))
  }

  "p5" in {
    // What is the smallest number divisible by each of the numbers 1 to 20?*
    checkSuccess(kernel, Vector(
      ("Range(20, Int.MaxValue).find(n => Range(2, 21).forall(n % _ == 0)).get", checkInt(232792560))
      ))
  }

  "p6" in {
    // What is the difference between the sum of the squares and the
    // square of the sums?*
    checkSuccess(kernel, Vector(
      ("val numbers = 1 to 100", checkUnit),
      ("def square(n: Int) = n * n", checkUnit),
      ("square(numbers.sum) - numbers.map(square).sum", checkInt(25164150))
      ))
  }

  "p7" in {
    // Find the 10001st prime.*
    checkSuccess(kernel, Vector(
      ("""
        lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
          ps.takeWhile(j => j * j <= i).forall(i % _ > 0))
        """, checkUnit),
      ("ps(10000)", checkInt(104743))
      ))
  }

  "p8" in {
    // Discover the largest product of five consecutive digits in the 1000-digit number.*
    val data =
      """73167176531330624919225119674426574742355349194934
          |96983520312774506326239578318016984801869478851843
          |85861560789112949495459501737958331952853208805511
          |12540698747158523863050715693290963295227443043557
          |66896648950445244523161731856403098711121722383113
          |62229893423380308135336276614282806444486645238749
          |30358907296290491560440772390713810515859307960866
          |70172427121883998797908792274921901699720888093776
          |65727333001053367881220235421809751254540594752243
          |52584907711670556013604839586446706324415722155397
          |53697817977846174064955149290862569321978468622482
          |83972241375657056057490261407972968652414535100474
          |82166370484403199890008895243450658541227588666881
          |16427171479924442928230863465674813919123162824586
          |17866458359124566529476545682848912883142607690042
          |24219022671055626321111109370544217506941658960408
          |07198403850962455444362981230987879927244284909188
          |84580156166097919133875499200524063689912560717606
          |05886116467109405077541002256983155200055935729725
          |71636269561882670428252483600823257530420752963450""".stripMargin.replace("\n", "")

    checkSuccess(kernel, Vector(
      (s"""val s = "$data" """, checkUnit),
      ("""
        {
        s.filter(_.isDigit)
         .map(_.asDigit)
         .sliding(5)
         .map(_.product).max
        }
        """, checkInt(40824))
      ))

  }

  "p9" in {
    // Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.*
    checkSuccess(kernel, Vector(
      ("val limit = (1 to 1000).find(n => n + math.sqrt(n) >= 1000).get", checkUnit),
      ("limit", checkInt(969)),
      ("""
        for{
          b <- 2 until limit
          a <- 1 until b
          c = 1000 - a - b
          if a * a + b * b  == c * c
        } yield a * b * c
        """, {
          case x: IndexedSeq[_] => x == Vector(31875000)
          case _ => false
          })
      ))
  }

  "p10" in {
    // Calculate the sum of all the primes below two million.*
    checkSuccess(kernel, Vector(
      ("""
        lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
           ps.takeWhile(j => j * j <= i).forall(i % _ > 0))
        """, checkUnit),
      ("ps.view.takeWhile(_ < 2000000).foldLeft(0L)(_ + _)", checkLong(142913828922L))
      ))
  }

  "p11" in {
    // What is the greatest product of four numbers
    // on the same straight line in the 20 by 20 grid?*
    val data =
      """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
          |49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
          |81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
          |52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
          |22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
          |24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
          |32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
          |67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
          |24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
          |21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
          |78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
          |16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
          |86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
          |19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
          |04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
          |88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
          |04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
          |20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
          |20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
          |01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
        """.stripMargin.replace("\n", " ")
    checkSuccess(kernel, Vector(
      (s"""val s = "$data" """, checkUnit),
      (s"""val ns = s.split("${"""\\s+"""}").map(_.toInt)""", checkUnit),
      ("def m(i: Int, p: Int, c: Int): Int = if(c > 0) ns(i) * m(i + p, p, c - 1) else 1", checkUnit),
      ("""
        def ms(xs: Seq[Int], ys: Seq[Int], p: Int) = {
          ys.flatMap(y => xs.map(x => m(20 * y + x, p, 4)))
        }
        """, checkUnit),
      ("""
        val ps = (
          ms(0 to 19, 0 to 15, 20) ++ ms(0 to 15, 0 to 19, 1) ++
          ms(0 to 15, 0 to 15, 21) ++ ms(3 to 19, 0 to 15, 19)
        )
        """, checkUnit),
      ("ps.max", checkInt(70600674))
      ))
  }

  "p12" in {
    // What is the value of the first triangle number to have over five hundred divisors?
    checkSuccess(kernel, Vector(
      ("lazy val ts: Stream[Int] = 0 #:: ts.zipWithIndex.map(p => p._1 + p._2 + 1)", checkUnit),
      ("""
        def p(t: Int) = {
          Range(1, Int.MaxValue)
            .takeWhile(n => n * n <= t)
            .foldLeft(0)((s, n) => if(t % n == 0) s + 2 else s)
        }
        """, checkUnit),
      ("ts.find(p(_) > 500).get", checkInt(76576500))
      ))
  }

  "p13" in {
    // Find the first ten digits of the sum of one-hundred 50-digit numbers.*
    val data = """37107287533902102798797998220837590246510135740250
                   |46376937677490009712648124896970078050417018260538
                   |74324986199524741059474233309513058123726617309629
                   |91942213363574161572522430563301811072406154908250
                   |23067588207539346171171980310421047513778063246676
                   |89261670696623633820136378418383684178734361726757
                   |28112879812849979408065481931592621691275889832738
                   |44274228917432520321923589422876796487670272189318
                   |47451445736001306439091167216856844588711603153276
                   |70386486105843025439939619828917593665686757934951
                   |62176457141856560629502157223196586755079324193331
                   |64906352462741904929101432445813822663347944758178
                   |92575867718337217661963751590579239728245598838407
                   |58203565325359399008402633568948830189458628227828
                   |80181199384826282014278194139940567587151170094390
                   |35398664372827112653829987240784473053190104293586
                   |86515506006295864861532075273371959191420517255829
                   |71693888707715466499115593487603532921714970056938
                   |54370070576826684624621495650076471787294438377604
                   |53282654108756828443191190634694037855217779295145
                   |36123272525000296071075082563815656710885258350721
                   |45876576172410976447339110607218265236877223636045
                   |17423706905851860660448207621209813287860733969412
                   |81142660418086830619328460811191061556940512689692
                   |51934325451728388641918047049293215058642563049483
                   |62467221648435076201727918039944693004732956340691
                   |15732444386908125794514089057706229429197107928209
                   |55037687525678773091862540744969844508330393682126
                   |18336384825330154686196124348767681297534375946515
                   |80386287592878490201521685554828717201219257766954
                   |78182833757993103614740356856449095527097864797581
                   |16726320100436897842553539920931837441497806860984
                   |48403098129077791799088218795327364475675590848030
                   |87086987551392711854517078544161852424320693150332
                   |59959406895756536782107074926966537676326235447210
                   |69793950679652694742597709739166693763042633987085
                   |41052684708299085211399427365734116182760315001271
                   |65378607361501080857009149939512557028198746004375
                   |35829035317434717326932123578154982629742552737307
                   |94953759765105305946966067683156574377167401875275
                   |88902802571733229619176668713819931811048770190271
                   |25267680276078003013678680992525463401061632866526
                   |36270218540497705585629946580636237993140746255962
                   |24074486908231174977792365466257246923322810917141
                   |91430288197103288597806669760892938638285025333403
                   |34413065578016127815921815005561868836468420090470
                   |23053081172816430487623791969842487255036638784583
                   |11487696932154902810424020138335124462181441773470
                   |63783299490636259666498587618221225225512486764533
                   |67720186971698544312419572409913959008952310058822
                   |95548255300263520781532296796249481641953868218774
                   |76085327132285723110424803456124867697064507995236
                   |37774242535411291684276865538926205024910326572967
                   |23701913275725675285653248258265463092207058596522
                   |29798860272258331913126375147341994889534765745501
                   |18495701454879288984856827726077713721403798879715
                   |38298203783031473527721580348144513491373226651381
                   |34829543829199918180278916522431027392251122869539
                   |40957953066405232632538044100059654939159879593635
                   |29746152185502371307642255121183693803580388584903
                   |41698116222072977186158236678424689157993532961922
                   |62467957194401269043877107275048102390895523597457
                   |23189706772547915061505504953922979530901129967519
                   |86188088225875314529584099251203829009407770775672
                   |11306739708304724483816533873502340845647058077308
                   |82959174767140363198008187129011875491310547126581
                   |97623331044818386269515456334926366572897563400500
                   |42846280183517070527831839425882145521227251250327
                   |55121603546981200581762165212827652751691296897789
                   |32238195734329339946437501907836945765883352399886
                   |75506164965184775180738168837861091527357929701337
                   |62177842752192623401942399639168044983993173312731
                   |32924185707147349566916674687634660915035914677504
                   |99518671430235219628894890102423325116913619626622
                   |73267460800591547471830798392868535206946944540724
                   |76841822524674417161514036427982273348055556214818
                   |97142617910342598647204516893989422179826088076852
                   |87783646182799346313767754307809363333018982642090
                   |10848802521674670883215120185883543223812876952786
                   |71329612474782464538636993009049310363619763878039
                   |62184073572399794223406235393808339651327408011116
                   |66627891981488087797941876876144230030984490851411
                   |60661826293682836764744779239180335110989069790714
                   |85786944089552990653640447425576083659976645795096
                   |66024396409905389607120198219976047599490197230297
                   |64913982680032973156037120041377903785566085089252
                   |16730939319872750275468906903707539413042652315011
                   |94809377245048795150954100921645863754710598436791
                   |78639167021187492431995700641917969777599028300699
                   |15368713711936614952811305876380278410754449733078
                   |40789923115535562561142322423255033685442488917353
                   |44889911501440648020369068063960672322193204149535
                   |41503128880339536053299340368006977710650566631954
                   |81234880673210146739058568557934581403627822703280
                   |82616570773948327592232845941706525094512325230608
                   |22918802058777319719839450180888072429661980811197
                   |77158542502016545090413245809786882778948721859617
                   |72107838435069186155435662884062257473692284509516
                   |20849603980134001723930671666823555245252804609722
                   |53503534226472524250874054075591789781264330331690"""
    checkSuccess(kernel, Vector(
      (s"""val s = "${data.stripMargin.replace("\n", " ")}" """, checkUnit),
      (s"""s.split("${"""\\s+"""}").map(_.take(11).toLong).sum.toString.take(10).toLong""", checkLong(5537376230L))
      ))
  }

  "p14" in {
    // Find the longest sequence using a starting number under one million.*
    checkSuccess(kernel, Vector(
      ("""
        def from(n: Long, c: Int = 0): Int =
          if(n == 1) c + 1 else
          from(if(n % 2 == 0) n / 2 else 3 * n + 1, c + 1)
        """, checkUnit),
      ("""
        (
        (1 until 1000000).view.map(n => (n, from(n)))
             .reduceLeft((a, b) => if(a._2 > b._2) a else b)._1
        )
        """, checkInt(837799))
      ))
  }

  "p15" in {
    // Starting in the top left corner in a 20 by 20 grid,
    // how many routes are there to the bottom right corner?*
    checkSuccess(kernel, Vector(
      ("""
        def f(row: Seq[Long], c: Int): Long =
             if (c == 0) row.last else f(row.scan(0L)(_ + _), c - 1)
        """, checkUnit),
      ("def r(n: Int) = f(Seq.fill(n + 1)(1L), n)", checkUnit),
      ("r(20)", checkLong(137846528820L))
      ))
  }

  "p16" in {
    // What is the sum of the digits of the number 2^1000?*
    checkSuccess(kernel, Vector(
      (" BigInt(2).pow(1000).toString.view.map(_.asDigit).sum", checkInt(1366))
      ))
  }

  "p17" in {
    // How many letters would be needed to write all
    // the numbers in words from 1 to 1000?*
    checkSuccess(kernel, Vector(
      ("val units = Array(0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8)", checkUnit),
      ("val tens = Array(0, 0, 6, 6, 5, 5, 5, 7, 6, 6)", checkUnit),
      ("""
        lazy val name: Int => Int = {
          case n if(n < 20) => units(n)
          case n if(n < 100) =>
          tens(n / 10) + (if(n % 10 > 0) units(n % 10) else 0)
          case n if(n < 1000) =>
          name(n / 100) + 7 + (if(n % 100 > 0) 3 + name(n % 100) else 0)
          case 1000 => 11
        }
        """, checkUnit),
      ("(1 to 1000).map(name).sum", checkInt(21124))
      ))
  }

  "p18" in {
    // Find the maximum sum travelling from the top of the triangle to the base.*
    val data = """75
                   |95 64
                   |17 47 82
                   |18 35 87 10
                   |20 04 82 47 65
                   |19 01 23 75 03 34
                   |88 02 77 73 07 63 67
                   |99 65 04 28 06 16 70 92
                   |41 41 26 56 83 40 80 70 33
                   |41 48 72 33 47 32 37 16 94 29
                   |53 71 44 65 25 43 91 52 97 51 14
                   |70 11 33 28 77 73 17 78 39 68 17 57
                   |91 71 52 38 17 14 91 43 58 50 27 29 48
                   |63 66 04 68 89 53 67 30 73 16 69 87 40 31
                   |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23""".stripMargin.replace("\n", "|")

    checkSuccess(kernel, Vector(
      (s"""val s =  "$data" """, checkUnit),
      (s"""val grid = s.trim.split("\\\\|").map(_.split(" ").map(_.toInt))""", checkUnit),
      ("""
        def f(rows: Array[Array[Int]], bottom: Seq[Int]): Int = {
         val ms = bottom.zip(bottom.tail).map(p => p._1 max p._2)
         val ss = rows.last.zip(ms).map(p => p._1 + p._2)
         if (ss.size == 1) ss.head else f(rows.init, ss)
        }
        """, checkUnit),
      ("f(grid.init, grid.last)", checkInt(1074))
      ))
  }

  "p19" in {
    //How many Sundays fell on the first of the month during the twentieth century?*
    checkSuccess(kernel, Vector(
      ("val lengths = Array(31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)", checkUnit),
      ("""
        val ls = for(y <- 1900 to 2000; m <- 1 to 12) yield {
         if(m == 2)
           if (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)) 29 else 28
         else
           lengths(m - 1)
        }
        """, checkUnit),
      ("val fs = ls.scanLeft(1)((ws, l) => (ws + l) % 7)", checkUnit),
      ("fs.drop(12).take(1200).count(_ == 0)", checkInt(171))
      ))
  }

  "p20" in {
    // Find the sum of digits in 100!*
    checkSuccess(kernel, Vector(
      ("def f(n: BigInt): BigInt = if(n < 2) 1 else n * f(n - 1)", checkUnit),
      ("f(100).toString.view.map(_.asDigit).sum", checkInt(648))
      ))
  }

  "p21" in {
    // Evaluate the sum of all amicable pairs under 10000.*
    checkSuccess(kernel, Vector(
      ("""
        val ds = (0 until 10000).view.map(
         n => (1 to (n / 2)).filter(n % _ == 0).sum
        )
        """, checkUnit),
      ("""
        val as = ds.zipWithIndex.collect {
          case (n, i) if n < 10000 && ds(n) != n && ds(n) == i => i
        }
        """, checkUnit),
      ("as.sum", checkInt(31626))
      ))
  }

  "p22" in {
    // Smaller than the real dataset, because the real dataset
    // is too big to copy & paste into the unit test code.
    val data = "\"\"\"" + """
        "MARY","PATRICIA","LINDA","BARBARA","ELIZABETH","JENNIFER","MARIA",
        "SUSAN","MARGARET","DOROTHY","LISA","NANCY","KAREN","BETTY","HELEN",
        "SANDRA","DONNA","CAROL","RUTH","SHARON","MICHELLE","LAURA","SARAH",
        "KIMBERLY","DEBORAH","JESSICA","SHIRLEY","CYNTHIA","ANGELA","MELISSA",
        "BRENDA","AMY","ANNA","REBECCA","VIRGINIA","KATHLEEN","PAMELA",
        "MARTHA","DEBRA","AMANDA","STEPHANIE","CAROLYN","CHRISTINE","MARIE",
        "JANET","CATHERINE","FRANCES","ANN","JOYCE","DIANE","ALICE","JULIE",
        "HEATHER","TERESA","DORIS","GLORIA","EVELYN","JEAN","CHERYL","MILDRED",
        "KATHERINE","JOAN","ASHLEY","JUDITH","ROSE","JANICE","KELLY","NICOLE",
        "JUDY","CHRISTINA","KATHY","THERESA","BEVERLY","DENISE","TAMMY",
        "IRENE","JANE","LORI","RACHEL","MARILYN","ANDREA","KATHRYN","LOUISE",
        "SARA","ANNE","JACQUELINE","WANDA","BONNIE","JULIA","RUBY","LOIS",
        "TINA","PHYLLIS","NORMA","PAULA","DIANA","ANNIE","LILLIAN","EMILY",
        "ROBIN","PEGGY","CRYSTAL","GLADYS","RITA","DAWN","CONNIE","FLORENCE",
        "TRACY","EDNA","TIFFANY","CARMEN","ROSA","CINDY","GRACE","WENDY",
        "VICTORIA","EDITH","KIM","SHERRY","SYLVIA","JOSEPHINE","THELMA",
        "SHANNON","SHEILA","ETHEL","ELLEN","ELAINE","MARJORIE","CARRIE",
        "CHARLOTTE","MONICA","ESTHER","PAULINE","EMMA","JUANITA","ANITA",
        "RHONDA","HAZEL","AMBER","EVA","DEBBIE","APRIL","LESLIE","CLARA",
        "LUCILLE","JAMIE","JOANNE","ELEANOR","VALERIE","DANIELLE","MEGAN",
        "ALICIA","SUZANNE","MICHELE","GAIL","BERTHA","DARLENE","VERONICA",
        "JILL","ERIN","GERALDINE","LAUREN","CATHY","JOANN","LORRAINE","LYNN",
        "SALLY","REGINA","ERICA","BEATRICE","DOLORES","BERNICE","AUDREY",
        "YVONNE","ANNETTE","JUNE","SAMANTHA","MARION","DANA","STACY","ANA",
        "RENEE","IDA","VIVIAN","ROBERTA","HOLLY","BRITTANY","MELANIE",
        "LORETTA","YOLANDA","JEANETTE","LAURIE","KATIE","KRISTEN","VANESSA",
        "ALMA","SUE","ELSIE","BETH","JEANNE","VICKI","CARLA","TARA","ROSEMARY",
        "EILEEN","TERRI","GERTRUDE","LUCY","TONYA","ELLA","STACEY","WILMA",
        "GINA","KRISTIN","JESSIE","NATALIE","AGNES","VERA","WILLIE","CHARLENE",
        "BESSIE","DELORES","MELINDA","PEARL","ARLENE","MAUREEN","COLLEEN",
        "ALLISON","TAMARA","JOY","GEORGIA","CONSTANCE","LILLIE","CLAUDIA",
        "JACKIE","MARCIA","TANYA","NELLIE","MINNIE","MARLENE","HEIDI","GLENDA",
        "LYDIA","VIOLA","COURTNEY","MARIAN","STELLA","CAROLINE","DORA","JO",
        "VICKIE","MATTIE","TERRY","MAXINE","IRMA","MABEL","MARSHA","MYRTLE",
        "LENA","CHRISTY","DEANNA","PATSY","HILDA","GWENDOLYN","JENNIE"
      """.replaceAll(" |\\n", "") + "\"\"\""
    // What is the total of all the name scores in the file of first names?*
    checkSuccess(kernel, Vector(
      (s"""
        {
         $data
           .mkString
           .split(",")
           .map(_.init.tail).sorted.map(_.map(_ - 64).sum)
           .zipWithIndex.map(p => p._1 * (p._2 + 1)).sum
        }
        """, checkInt(2260261))
      ))
  }

   "p23" in {
     // Find the sum of all the positive integers which cannot
     // be written as the sum of two abundant numbers.*
     checkSuccess(kernel, Vector(
      ("""
        val as = {
       (0 to 28123).map(n => (1 to (n / 2))
                   .filter(n % _ == 0).sum)
                   .zipWithIndex
                   .filter(p => p._1 > p._2)
                   .map(_._2)
       }
        """, checkUnit),
      ("""
        {
         val exc = as.flatMap { a =>
           as.takeWhile(_ <= (28123 - a)).map(a +)
         }
         (1 to 28123 diff exc).sum
       }
        """, checkInt(4179871))
      ))
   }

  "p24" in {
    // What is the millionth lexicographic permutation of the digits
    // 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?*
    checkSuccess(kernel, Vector(
      ("""
        def ps(s: String): Iterator[String] = {
          if (s.length == 1) Iterator(s)
          else s.toIterator.flatMap(c => ps(s.filter(c !=)).map(c +))
        }
        """, checkUnit),
      ("""ps("0123456789").drop(999999).next().toLong""", checkLong(2783915460L))
      ))
  }

  "p25" in {
    // What is the first term in the Fibonacci sequence to contain 1000 digits?*
    checkSuccess(kernel, Vector(
      ("""
        lazy val fs: Stream[BigInt] =
         0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2)
        """, checkUnit),
      ("fs.view.takeWhile(_.toString.length < 1000).size", checkInt(4782))
      ))
  }

  "p26" in {
    // Find the value of d < 1000 for which 1/d contains the longest recurring cycle.*
    checkSuccess(kernel, Vector(
      ("""
                val ps = {
          (2 until 1000).map(i => (1 to 2000)
                        .find(BigInt(10).modPow(_, i) == 1))
        }
        """, checkUnit),
      ("2 + ps.indexOf(Some(ps.flatten.max))", checkInt(983))
      ))
  }

  "p27" in {
    // Find a quadratic formula that produces the maximum number of
    // primes for consecutive values of n.*
    checkSuccess(kernel, Vector(
      ("""
        {
          lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
            ps.takeWhile(j => j * j <= i).forall(i % _ > 0))
          def isPrime(n: Int) = ps.view.takeWhile(_ <= n).contains(n)
          val ns = (-999 until 1000).flatMap { a =>
            (-999 until 1000).map(b => (a, b, (0 to 1000).view
              .takeWhile(n => isPrime(n * n + a * n + b)).size))
          }
          val t = ns.reduceLeft((a, b) => if(a._3 > b._3) a else b)
          t._1 * t._2
        }
        """, checkInt(-59231))
      ))
  }

  "p28" in {
    // What is the sum of both diagonals in a 1001 by 1001 spiral?*
    checkSuccess(kernel, Vector(
      ("def cs(n: Int, p: Int): Stream[Int] = (n * 4 + p * 10) #:: cs(n + p * 4, p + 2)", checkUnit),
      ("1 + cs(1, 2).take(500).sum", checkInt(669171001))
      ))
  }

  "p29" in {
    // How many distinct terms are in the sequence generated by ab
    // for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?*
    checkSuccess(kernel, Vector(
      ("""
        {
          (2 to 100).flatMap(a => (2 to 100)
                    .map(b => BigInt(a).pow(b)))
                    .distinct
                    .size
        }
        """, checkInt(9183))
      ))
  }

  "p30" in {
    // Find the sum of all the numbers that can be written as
    // the sum of fifth powers of their digits.*
    checkSuccess(kernel, Vector(
      ("def max(d: Int) = math.pow(10, d).toInt - 1", checkUnit),
      ("def sum(n: Int) = n.toString.map(_.asDigit).map(math.pow(_, 5).toInt).sum", checkUnit),
      ("val limit = Stream.from(1).find(d => max(d) > sum(max(d))).get", checkUnit),
      ("(2 to max(limit)).view.filter(n => n == sum(n)).sum", checkInt(443839))
      ))
  }

  "p31" in {
    // Investigating combinations of English currency denominations.*
    checkSuccess(kernel, Vector(
      ("""
        def f(ms: List[Int], n: Int): Int = ms match {
          case h :: t =>
            if (h > n) 0 else if (n == h) 1 else f(ms, n - h) + f(t, n)
          case _ => 0
        }
        """, checkUnit),
      ("f(List(1, 2, 5, 10, 20, 50, 100, 200), 200)", checkInt(73682))
      ))
  }

  "p32" in {
    // Find the sum of all numbers that can be written as pandigital products.*
    checkSuccess(kernel, Vector(
      ("""
        val ms = for {
          a <- 2 to 10000; b <- 2 to 10000 / a
          m = a * b; s = a.toString + b + m
          if s.length == 9 && (1 to 9).mkString.forall(s.contains(_))
        } yield m
        """, checkUnit),
      ("ms.distinct.sum", checkInt(45228))
      ))
  }

  "p33" in {
     // Discover all the fractions with an unorthodox cancelling method.*
     checkSuccess(kernel, Vector(
      ("""
        val rs = for(i <- 1 to 9; j <- (i + 1) to 9; k <- 1 to 9;
                    if k * (9 * i + j) == 10 * i * j) yield (10 * i + j, 10 * j + k)
        """, checkUnit),
      ("val p = rs.reduceLeft((n, d) => (n._1 * d._1, n._2 * d._2))", checkUnit),
      ("def gcd(n: Int, d: Int): Int = if (d == 0) n else gcd(d, n % d)", checkUnit),
      ("p._2 / gcd(p._1, p._2)", checkInt(100))
      ))
   }

}
