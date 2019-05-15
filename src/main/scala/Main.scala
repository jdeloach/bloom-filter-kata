import scala.io.Source
import scala.util.Random

object Main {
  def main(args: Array[String]) = {
    val bits = 2*1024*1024
    val numHashes = 3
    val filename = "wordlist.txt"
    val words = Source.fromFile(filename).getLines.toArray.map(_.toUpperCase())

    val filter = constructMutableFilter(words, numHashes, bits)

    println(s"Size of collection: ${words.length}")
    println(s"Caught words: ${words.count(word => filter.test(word))}")
    println(s"Missed words: ${words.count(word => !filter.test(word))}")

    val testRandomWords = 1000
    val generated = Range(1, testRandomWords).map(x => Random.alphanumeric.filter(_.isLetter).take(5).mkString("").toUpperCase())
    val estimatedPositives = generated.map(gen => filter.test(gen)).count(_ == true)
    val actualPositives = generated.filter(gen => words.contains(gen))

    println(s"Estimated Pos: $estimatedPositives")
    println(s"Actual Pos: ${actualPositives.length}")

    // Experimental should trend to theoreitcal rate as number of test random words increases
    println(s"Experimental FP Rate: ${100*(estimatedPositives - actualPositives.length) / testRandomWords.toDouble}%")
    println(s"Theoretical FP Rate: ${expectedFalsePositiveRate(words.length, numHashes, bits)}")
  }

  /**
   * Creates a mutable bloom filter. Fast performance locally, doesn't scale well for a map reduce paradigm and cannot
   * be run in parallel.
   */
  private def constructMutableFilter(words: Array[String], numHashes: Int, numBits: Int) : MutableBloomFilter = {
    val filter = new MutableBloomFilter(numHashes, numBits)
    words.foreach(word => filter.add(word))

    filter
  }

  /**
   * Constructs an immutable bloom filter and adds words through an aggregation. This does not make much sense
   * at the single machine level, but this data structure could be used in something like Spark, to distribute (map) the 
   * construction of the Bloom filter across N-machines, and then aggregate (reduce) the N-bloom filters as one.
   * 
   * To optimize that further, one would probably want to adapt an `addRange` method to the data structure such that
   * for each M words assigned to the machine, M filters wouldn't be created to locally reduce to 1 filter. That wastes
   * a lot of time on unnecessary local reducing.
   */
  private def constructImmutableFilter(words: Array[String], numHashes: Int, numBits: Int) : ImmutableBloomFilter = {
    words
      .par
      .aggregate(new ImmutableBloomFilter(numHashes, numBits))((filter, word) => filter.add(word), _ ++ _)
  }
  
  /*
   * Calculates the expected false positive rate, for the given conditions.
   * Cite: https://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives
   */
  private def expectedFalsePositiveRate(numWords: Int, numHashes: Int, numBits: Int) = 
    100*Math.pow((1 - Math.pow(Math.E, -1 * numHashes.toDouble * numWords / numBits)), numHashes.toDouble)
}