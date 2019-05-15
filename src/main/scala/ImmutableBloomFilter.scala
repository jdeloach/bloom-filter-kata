import scala.collection.immutable.BitSet
import scala.util.hashing.MurmurHash3
import scala.util.hashing.ByteswapHashing

class ImmutableBloomFilter(numHashFunctions: Int = 2, numBits: Int = 1024*1024, val bitset: BitSet = BitSet()) {
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
        new ImmutableBloomFilter(numHashFunctions, numBits, bitset ++ combinedHash(word))
    }

    def test(word: String) : Boolean = {
        combinedHash(word)
            .map(idx => bitset(idx))
            .reduce(_ && _)
    }

    def ++(that: ImmutableBloomFilter) : ImmutableBloomFilter = {
        // what if types are different
        new ImmutableBloomFilter(numHashFunctions, numBits, bitset ++ that.bitset)
    }
}