import scala.util.hashing.MurmurHash3

abstract class BaseBloomFilter(val numHashFunctions: Int, val numBits: Int) {        
    if (numHashFunctions < 1)
        throw new Error("Minimum hash functions is 1.")
    if (numBits < 1)
        throw new Error("Minimum number of bits is 1. 1024 or higher is recommended, however.")

    /**
     * Tests if this word may be in the set. 
     * If False, the word is certainly not in the set.
     * If True, it may be in the set, or it may be a false positive.
     */
    def test(word: String) : Boolean

    /**
     * Returns an array of the indexes of the bits to flip for this word.
     * Instead of having a fixed number of different hashes by using distinct
     * algorithms (say SHA-1, MD5, etc.), we use a single non-crypto hash with
     * different N-different seeds.
     */
    protected def combinedHash(word: String): Array[Int] = {
        Range(0, numHashFunctions)
            .toArray
            .map(seed => 
                Math.abs(MurmurHash3.stringHash(word, seed)) % numBits)
    }
}