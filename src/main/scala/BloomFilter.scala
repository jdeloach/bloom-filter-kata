import scala.collection.immutable.BitSet
import scala.util.hashing.MurmurHash3
import scala.util.hashing.ByteswapHashing

class BloomFilter(numHashFunctions: Int = 2, numBits: Int = 1024*1024) {
    // probably should get lower than BitSet
    var internalSet = BitSet()

    // TODO Handle < 1 Hash Functions
    // TODO Docs
    // TODO Tests

    private def combinedHash(word: String): Array[Int] = {
        Range(0, numHashFunctions)
            .toArray
            .map(seed => 
                Math.abs(MurmurHash3.stringHash(word, seed)) % numBits)
    }

    def add(word: String) = {
        internalSet = internalSet ++ combinedHash(word)
    }

    def test(word: String) : Boolean = {
        combinedHash(word)
            .map(idx => internalSet(idx))
            .reduce(_ && _)
    }
}