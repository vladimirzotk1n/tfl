import scala.util.Random
import scala.util.control.Breaks._
import scala.collection.mutable

val ALPHABET: Vector[String] = Vector("a", "b")



object Main {

    def replaceRandomOccurrence(text: String, oldStr: String, newStr: String): String = {
        val occurrences = collection.mutable.ArrayBuffer[Int]()
        var start = 0

        while (start <= text.length - oldStr.length) {
            val idx = text.indexOf(oldStr, start)
            if (idx == -1) {
                start = text.length + 1
            } else {
                occurrences += idx
                start = idx + 1
            }
        }

        if (occurrences.isEmpty) text
        else {
            val randomIdx = occurrences(Random.nextInt(occurrences.length))
            text.substring(0, randomIdx) + newStr + text.substring(randomIdx + oldStr.length)
        }
    }

    def getWords(rules: Vector[(String, String)],
                 minWordLen: Int = 3,
                 maxWordLen: Int = 15,
                 maxOperations: Int = 10,
                 maxIter: Int = 1000): (String, String, Vector[String]) = {

        for (_ <- 1 to maxIter) {
            val wordLen = minWordLen + Random.nextInt(maxWordLen)
            val w = (1 to wordLen).map(_ => ALPHABET(Random.nextInt(ALPHABET.length))).mkString
            var wHat = w
            val chain = collection.mutable.ArrayBuffer[String](wHat)

            val numOperations = 1 + Random.nextInt(maxOperations)

            breakable {
                for (_ <- 1 to numOperations) {
                    val possibleRules = rules.filter { case (oldStr, _) => wHat.contains(oldStr) }
                    if (possibleRules.isEmpty) {
                        break
                    }
                    val (oldStr, newStr) = possibleRules(Random.nextInt(possibleRules.length))
                    wHat = replaceRandomOccurrence(wHat, oldStr, newStr)
                    chain += wHat
                }
            }

            if (w != wHat) {
                return (w, wHat, chain.toVector)
            }
        }

        // не получилось изменить слово
        val failWordLen = minWordLen + Random.nextInt(maxWordLen)
        val w = (1 to failWordLen).map(_ => ALPHABET(Random.nextInt(ALPHABET.length))).mkString
        (w, w, Vector(w))
    }

    def findNext(word: String, rules: Vector[(String, String)]): Vector[String] = {
        var nextWords = collection.mutable.ArrayBuffer[String]()

        for ((oldStr, newStr) <- rules) {
            var start = 0
            while (start <= word.length - oldStr.length) {
                val idx = word.indexOf(oldStr, start)
                if (idx == -1) {
                    start = word.length + 1
                } else {
                    val nextWord = word.substring(0, idx) + newStr + word.substring(idx + oldStr.length)
                    if (nextWord != word) {
                        nextWords += nextWord
                    }
                    start = idx + 1
                }
            }
        }

        nextWords.toVector
    }

    def reduce(word: String, rules: Vector[(String, String)], maxDepth: Int = 1000): Set[String] = {
        val visited = mutable.Set[String]()
        val queue = mutable.Queue[(String, Int)]()
        val normalForms = mutable.Set[String]()

        queue.enqueue((word, 0))

        while (queue.nonEmpty) {
            val (curWord, curDepth) = queue.dequeue()

            if (curDepth > maxDepth || visited.contains(curWord)) {}
            else {
                visited.add(curWord)
                val nextWords = findNext(curWord, rules)

                if (nextWords.isEmpty) {
                    normalForms.add(curWord)
                }

                nextWords.foreach { nextWord =>
                    if (!visited.contains(nextWord)) {
                        queue.enqueue((nextWord, curDepth + 1))
                    }
                }
            }
        }

        normalForms.toSet
    }

    def isReachable(word1: String, word2: String, rules: Vector[(String, String)]): Boolean = {
        val nf1 = reduce(word1, rules)
        val nf2 = reduce(word2, rules)
        nf1.intersect(nf2).nonEmpty
    }

    def fuzzTesting(rules1: Vector[(String, String)], rules2: Vector[(String, String)], numTest: Int = 100): Boolean = {
        val results = (1 to numTest).map { _ =>
            val (w, wHat, _) = getWords(rules1)
            isReachable(w, wHat, rules2)
        }

        results.forall(_ == true)
    }

    def checkInvariant(rules: Vector[(String, String)], invariants: Vector[String => Int]): Boolean = {
        val (_, _, chain) = getWords(rules)

        for (i <- 1 until chain.length) {
            val prev = chain(i - 1)
            val current = chain(i)

            for (invariant <- invariants) {
                if (invariant(prev) != invariant(current)) {
                    return false
                }
            }
        }

        true
    }

    def metaTesting(rules: Vector[(String, String)], invariants: Vector[String => Int], numTest: Int = 100): Boolean = {
        val results = (1 to numTest).map { _ =>
            checkInvariant(rules, invariants)
        }
        results.forall(_ == true)
    }


    val oldRules: Vector[(String, String)] = Vector(
        ("aaa", "bab"),
        ("bbb", "aaa")
    )

    val newRules: Vector[(String, String)] = Vector(
        ("aaa", "bab"),
        ("bbb", "aaa"),
        ("baba", "abab"),
        ("bbab", "babb"),
        ("babba", "ababb"),
        ("bbaabab", "abaabab"),
        ("baababb", "ababbab"),
        ("bbabbabbab", "ababbabbab")
    )

    val invariants: Vector[String => Int] = Vector(
        _.length,
        s => (s.count(_ == 'a') - s.count(_ == 'b')) & 1,
        s => ((s.count(_ == 'b') - 2 * s.count(_ == 'a')) % 3 + 3) % 3
    )


    def main(args: Array[String]): Unit = {

        if (fuzzTesting(oldRules, newRules))
            println("Fuzz-testing passed!")
        else
            println("Something wrong with fuzz-testing...")


        if (metaTesting(newRules, invariants))
            println("Meta-testing passed!")
        else
            println("Something wrong with meta-testing...")
    }
}
