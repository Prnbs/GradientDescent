package nlp
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map}

/**
 * Created by psinha4 on 8/5/2015.
 */
object Ngram {

  def createSentences(book: ListBuffer[String]): ListBuffer[String] =
  {
    var sentences = ListBuffer[String]()
    var sentence = ""
    for(line <- book){
      sentence += " " + line
      if(sentence.contains('.')){
        val subSentences = sentence.split('.')
        if(subSentences.size == 1) { //. is at end of line
          sentences += subSentences(0)
          sentence = ""
        }
        else {
          for (i <- 0 to subSentences.size - 2) {
            sentences += subSentences(i)
          }
          sentence = subSentences(subSentences.size - 1)
        }
      }
    }
    sentences
  }

  def createTrigram(book: ListBuffer[String]): Map[String, Int] = {
    var trigram = Map[String, Int]()
    for(line <- book){
      val first = line.trim.split(" ")
      if(first.size > 3) {
        val second = first.tail
        val third = second.tail
        val triMap = (first zip second) zip third map {
          case ((x, y), z) => x + " " + y + " " + z
        }
        for (tri <- triMap) {
          trigram(tri) = trigram.getOrElse(tri, 0) + 1
        }
      }
    }
    for(item <- trigram)
      println(item._1 + " " + item._2)
    trigram
  }

  def main(args: Array[String]) {
    val path = "E:\\Workspace\\Scala\\ML\\Descent\\src\\nlp\\David_Copperfield.txt"
    var book = ListBuffer[String]()
    for(line <- Source.fromFile(path).getLines())
    {
      if(line.size > 0)
        book += line.replaceAll("[^a-zA-Z0-9 .]", "").trim().toLowerCase()
    }
    var sentences = createSentences(book)
   createTrigram(sentences)
  }

}
