import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.io._
import java.nio.file.Paths

object Task1 {
  // Global Variables

  val dir: String = Paths.get(".").toAbsolutePath.toString.dropRight(2)


  // Functions

  def read_books(): Seq[Array[String]] =
    {
      val listOfBooks = new java.io.File(dir+"\\books").listFiles.filter(_.getName.endsWith(".txt"))
      var books = Seq[Array[String]]()

      for(x <- listOfBooks)
      {
        val file = Source.fromFile(x, "UTF-8")
        val str = file.mkString.toLowerCase.replaceAll("['\"#_,“’`{.”!-:}?*;»…()«]", "").split("\\s+")
        file.close()
        val filtered = filter_stopwords(str)
        books = books :+ filtered
      }
      books
    }

  def filter_stopwords(arg: Array[String]): Array[String] =
   {
     val file = Source.fromFile("stopwords_en.txt")
     val stop = file.mkString.toLowerCase.replaceAll("['\"#_,“’`{.”!-:}?*;»…()«]", "").split("\\s+")
     val filtered = arg.filterNot(stop.contains(_))
     file.close()
     filtered
   }

  def count_distinct_words(arg: Seq[Array[String]]): ListBuffer[(String, Int)] =
    {
      var distinctWords = ListBuffer.empty[(String, Int)]
      val listOfBooks = new java.io.File(dir+"\\books").listFiles.filter(_.getName.endsWith(".txt"))
      var iter = 0
      val pw = new PrintWriter(new File("distinctWords.txt" ))
      for (x <- arg)
      {
        distinctWords += Tuple2(listOfBooks(iter).toString, x.distinct.length)
        pw.println(listOfBooks(iter).toString +" "+ x.distinct.length)
        iter += 1
      }
      pw.close()
      distinctWords
    }

  def log2(x: Double): Double = scala.math.log(x) / scala.math.log(2)

  def TFIDF_coefficient(arg: Seq[Array[String]]): ListBuffer[ListBuffer[(String, Double)]] =
    {
      val TFIDF = ListBuffer.empty[ListBuffer[(String, Double)]]
      var d = 0.0
      for (book <- arg)
      {
        var bookTFIDF = ListBuffer.empty[(String, Double)]
        for (word <- book.distinct)
        {
          d = 0.0
          val tf = book.count(_==word).toDouble / book.length.toDouble
          for (doc <- arg) if (doc.contains(word)) d += 1.0
          val idf = log2(arg.length / d)
          bookTFIDF += Tuple2(word, tf * idf)
        }
        TFIDF += bookTFIDF
      }
      TFIDF
    }

  def print_results(arg1: ListBuffer[(String, Int)], arg2: ListBuffer[ListBuffer[(String, Double)]]): Unit =
    {
      val pw = new PrintWriter(new File("BEST20TFIDFf&DistinctWords.txt" ))
      var toPrint = ListBuffer.empty[ListBuffer[(String, Double)]]
      for (book <- arg2) toPrint += book.sortBy(-_._2).take(20)
      for (x <- arg2.indices)
        {
          pw.println(arg1(x))
          for (y <- toPrint(x)) pw.println(y)
          pw.println(" ")
        }
      pw.close()
    }

  def find_word_in_book(arg1: List[String], arg2: ListBuffer[ListBuffer[(String, Double)]]): Unit =
    {
      val listOfBooks = new java.io.File(dir+"\\books").listFiles.filter(_.getName.endsWith(".txt"))
      for (word <- arg1)
        {
          var matches = ListBuffer.empty[(String, Double)]
          for (x <- arg2.indices)
              if (arg2(x).exists(_._1 == word))
                  matches += Tuple2(listOfBooks(x).toString, arg2(x).find(x => x._1 == word).get._2)
          println("The word \"" + word + "\" matches books:")
          matches = matches.sortBy(-_._2)
          for (x <- matches) println(x._1)
          println(" ")
        }
    }

  // Main function

  def main(args: Array[String]): Unit =
    {
      val books: Seq[Array[String]] = read_books()    // Sequence of 10 arrays, each array contains text of one book
      val distinctWords: ListBuffer[(String, Int)] = count_distinct_words(books)  // List containing book titles and #of distinct words in it
      val TFIDF: ListBuffer[ListBuffer[(String, Double)]] = TFIDF_coefficient(books)    // List of lists(10 books) containing all words and theirs TFIDF
      print_results(distinctWords, TFIDF)

      val inputWords = List("sword", "book", "nero", "pistol", "krakow", "oak")
      find_word_in_book(inputWords, TFIDF)

    }
}
