import scala.io.Source
import scala.util.Random

object Main {
  def main(args: Array[String]) = {
    println("Hello, Cambridge MA!")

    val bits = 2*1024*1024d
    val numHashes = 3d

    val filename = "wordlist.txt"
    val words = Source.fromFile(filename).getLines.toArray.map(_.toUpperCase())

    // This isn't parralel-safe
    val filter = new MutableBloomFilter(numHashes.toInt, bits.toInt)
    words.foreach(word => filter.add(word))
    filter.add("test")

    //val filter = words.par.aggregate(new ImmutableBloomFilter(numHashes.toInt, bits.toInt))((filter, word) => filter.add(word), _ ++ _)


    println(words.length)
    println("Test Result (test): " + filter.test("test"))
    println("Test Result (test1): " + filter.test("test1"))
    println("Caught words: " + words.count(word => filter.test(word)))
    println("Missed words: " + words.count(word => !filter.test(word)))

    val testRandomWords = 1000
    val generated = Range(1, testRandomWords).par.map(x => Random.alphanumeric.filter(_.isLetter).take(5).mkString("").toUpperCase())
    val estimatedPositives = generated.par.map(a => filter.test(a)).count(_ == true)
    val actualPositives = generated.filter(a => words.contains(a))

    println ("Estimated Pos: " + estimatedPositives)
    println ("Actual Pos: " + actualPositives.length)
    println ("Experimental FP Rate: " + 100*(estimatedPositives - actualPositives.length) / testRandomWords.toDouble + "%")

    // Cite: https://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives
    val expectedFP = Math.pow((1 - Math.pow(Math.E, -1 * numHashes * words.length / bits)), numHashes)
    println ("Theoretical FP Rate: " + 100*expectedFP)
  }
}