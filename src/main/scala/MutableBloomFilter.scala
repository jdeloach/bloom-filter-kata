import scala.collection.mutable.BitSet
import scala.util.hashing.MurmurHash3
import scala.util.hashing.ByteswapHashing

/**
 * An implementation of a mutable bloom filter.
 */
class MutableBloomFilter(numHashFunctions: Int = 2, numBits: Int = 1024*1024) extends BaseBloomFilter(numHashFunctions, numBits) {
    var internalSet = BitSet()

    /**
     * Adds a new word to this Bloom filter in an mutable manner.
     */
    def add(word: String) = {
        super.combinedHash(word).foreach(hash => internalSet.add(hash))
    }

    /**
     * Tests if this word may be in the set. 
     * If False, the word is certainly not in the set.
     * If True, it may be in the set, or it may be a false positive.
     */
    def test(word: String) : Boolean = {
        super.combinedHash(word)
            .map(idx => internalSet(idx))
            .reduce(_ && _)
    }
}