package hw3

import scala.collection.mutable

object Main {
  def standardDeviation(vector: List[Double]): Double = {
    if(vector.isEmpty)
      return 0.0
    Math.sqrt(vector.foldLeft(0.0)((x,y) => {x + Math.pow(y - vector.sum/vector.size, 2)})/vector.size)
  }

  def letterFrequencyRanking(corpus: String): String = {
    val freqMap = new mutable.HashMap[Char,Int]()
    for(c <- corpus)
      if(c.isLetterOrDigit) {
        val d = c.toLower
        if(freqMap.contains(d))
          freqMap.put(d, freqMap(d) + 1)
        else
          freqMap.put(d, 1)
      }
    val sortedSeq = freqMap.toSeq.sortWith((x,y) => x._2 > y._2 || x._2 == y._2 && x._1 < y._1)
    sortedSeq.foldLeft("")((x,y) => {x + y._1})
  }

  def romanji(katakana: String): String = {
    katakana.foldLeft("")((r, c) => {
      c match {
        case 'ン' =>
          if (r.length > 1 && ( r.endsWith("na") || r.endsWith("ni") || r.endsWith("nu") || r.endsWith("ne") || r.endsWith("no")))
            r + "escSeq"
          else
            r
        case 'ッ' => r + "escSeq"
        case 'ャ' | 'ュ' | 'ョ' =>
          if ((r.length > 0) && (r.last equals 'i'))
            r.substring(0, r.length - 1) + Katakana.symbols(c).foldLeft("")(_ + _)
          else
            r
        case 'ー' => r.substring(0, r.length - 1) + Katakana.longVowels(r.last).toString
        case _ =>
          if(r.length > 5 && r.endsWith("escSeq"))
            r.substring(0, r.length - 6) + Katakana.symbols(c).head + Katakana.symbols(c).foldLeft("")(_ + _)
          else
            r + Katakana.symbols(c).foldLeft("")(_ + _)
      }
    })
  }

  def gray(bits: Int): List[String] = {
    val r: mutable.ListBuffer[String] = mutable.ListBuffer[String]()
    if (bits <= 0) return r.toList
    r.addAll(mutable.ListBuffer("0", "1"))
    var i = 2
    while(i < (1 << bits)) {
      for(j <- (i - 1) to 0 by -1) {
        r.addOne(r(j))
      }
      for(j <- 0 until i) {
        r(j) = "0" + r(j)
      }
      for(j <- i until 2*i) {
        r(j) = "1" + r(j)
      }
      i = i << 1
    }
    r.toList
  }
}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'),  'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k','a'), 'キ' -> List('k','i'), 'ク' -> List('k','u'), 'ケ' -> List('k','e'), 'コ' -> List('k','o'),
    'ガ' -> List('g','a'), 'ギ' -> List('g','i'), 'グ' -> List('g','u'), 'ゲ' -> List('g','e'), 'ゴ' -> List('g','o'),
    'サ' -> List('s','a'), 'シ' -> List('s','i'), 'ス' -> List('s','u'), 'セ' -> List('s','e'), 'ソ' -> List('s','o'),
    'ザ' -> List('z','a'), 'ジ' -> List('z','i'), 'ズ' -> List('z','u'), 'ゼ' -> List('z','e'), 'ゾ' -> List('z','o'),
    'タ' -> List('t','a'), 'チ' -> List('t','i'), 'ツ' -> List('t','u'), 'テ' -> List('t','e'), 'ト' -> List('t','o'),
    'ダ' -> List('d','a'), 'ヂ' -> List('d','i'), 'ヅ' -> List('d','u'), 'デ' -> List('d','e'), 'ド' -> List('d','o'),
    'ナ' -> List('n','a'), 'ニ' -> List('n','i'), 'ヌ' -> List('n','u'), 'ネ' -> List('n','e'), 'ノ' -> List('n','o'),
    'ハ' -> List('h','a'), 'ヒ' -> List('h','i'), 'フ' -> List('h','u'), 'ヘ' -> List('h','e'), 'ホ' -> List('h','o'),
    'バ' -> List('b','a'), 'ビ' -> List('b','i'), 'ブ' -> List('b','u'), 'ベ' -> List('b','e'), 'ボ' -> List('b','o'),
    'パ' -> List('p','a'), 'ピ' -> List('p','i'), 'プ' -> List('p','u'), 'ペ' -> List('p','e'), 'ポ' -> List('p','o'),
    'マ' -> List('m','a'), 'ミ' -> List('m','i'), 'ム' -> List('m','u'), 'メ' -> List('m','e'), 'モ' -> List('m','o'),
    'ヤ' -> List('y','a'),                        'ユ' -> List('y','u'),                        'ヨ' -> List('y','o'),
    'ラ' -> List('r','a'), 'リ' -> List('r','i'), 'ル' -> List('r','u'), 'レ' -> List('r','e'), 'ロ' -> List('r','o'),
    'ワ' -> List('w','a'), 'ヰ' -> List('w','i'),                        'ヱ' -> List('w','e'), 'ヲ' -> List('w','o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
}