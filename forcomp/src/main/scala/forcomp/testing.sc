/*
type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]
def wordOccurrences(w: Word): Occurrences = w.groupBy(x => x.toLower).mapValues(_.size).toList.sorted
def sentenceOccurrences(s: Sentence): Occurrences = s.flatMap(x => wordOccurrences(x))


val lista = List(('a', 2), ('b', 2))
var items = lista.map(x => (for(i <- 1 until (x._2+1)) yield (x._1,i)).toList)
items.foldRight(List[Occurrences](Nil))((x,y) => y ++ (for(i <- x; j <- y) yield (i :: j)))

*/



import forcomp.Anagrams
var occurrences = Anagrams.wordOccurrences("Yesman")
Anagrams.combinations(occurrences)

var sentence = List("Linux", "rulez")
Anagrams.sentenceOccurrences(sentence)

var x = List(('l',2),('i',1),('n',1),('u',1),('x',1))
var y = List(('l',1),('i',1),('n',1))

def subtract(x: Anagrams.Occurrences, y: Anagrams.Occurrences): Anagrams.Occurrences = y match {
  case Nil => x
  case z :: zs => subtract(x.map(), zs)
}





