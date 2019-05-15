import scala.collection.immutable.BitSet
import scala.util.hashing.ByteswapHashing

class ImmutableBloomFilter(numHashFunctions: Int, numBits: Int, val bitset: BitSet = BitSet()) extends BaseBloomFilter(numHashFunctions, numBits) {
    // TODO Tests

    /**
     * Adds a new word to this Bloom filter in an immutable manner.
     */
    def add(word: String) = {
        new ImmutableBloomFilter(numHashFunctions, numBits, bitset ++ super.combinedHash(word))
    }

    /**
     * Tests if this word may be in the set. 
     * If False, the word is certainly not in the set.
     * If True, it may be in the set, or it may be a false positive.
     */
    def test(word: String) : Boolean = {
        super.combinedHash(word)
            .map(idx => bitset(idx))
            .reduce(_ && _)
    }

    /**
     * Merges two immutable bloom filter and returns the resulting merged filter.
     */
    def ++(that: ImmutableBloomFilter) : ImmutableBloomFilter = {
        if (numHashFunctions != that.numHashFunctions || numBits != that.numBits) 
            throw new Error("The input params (numHashFunctions, numBits) must be the same to merge Bloom Filters.")

        new ImmutableBloomFilter(numHashFunctions, numBits, bitset ++ that.bitset)
    }
}