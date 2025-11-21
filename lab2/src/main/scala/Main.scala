import scala.util.Random
import scala.util.matching.Regex

object Main {

    val ALPHABET = Array("a", "b")

    val DFA: Map[Int, List[(Int, String)]] = Map(
        1 -> List((2, "a"), (3, "b")),
        2 -> List((4, "a")),
        3 -> List((5, "a"), (6, "b")),
        4 -> List((7, "a"), (6, "b")),
        5 -> List((6, "a")),
        6 -> List((3, "b")),
        7 -> List((8, "a"), (3, "b")),
        8 -> List((9, "a"), (3, "b")),
        9 -> List((10, "a"), (11, "b")),
        10 -> List((12, "a"), (3, "b")),
        11 -> List((5, "a"), (11, "b")),
        12 -> List((13, "a"), (11, "b")),
        13 -> List((14, "a"), (11, "b")),
        14 -> List((15, "a"), (3, "b")),
        15 -> List((15, "a"), (11, "b"))
    )

    val NFA: Map[Int, List[(Int, String)]] = Map(
        0 -> List((1, "a"), (3, "a"), (5, "b")),
        1 -> List((2, "a"), (5, "a")),
        2 -> List((3, "a")),
        3 -> List((4, "a")),
        4 -> List((0, "a")),
        5 -> List((6, "a"), (7, "b")),
        6 -> List((7, "a")),
        7 -> List((5, "b"))
    )

    val AFA1: Map[Int, List[(Int, String)]] = Map(
        1 -> List((2, "a"), (3, "b")),
        2 -> List((4, "a")),
        3 -> List((5, "a"), (6, "b")),
        4 -> List((7, "a"), (6, "b")),
        5 -> List((6, "a")),
        6 -> List((3, "b")),
        7 -> List((8, "a"), (3, "b")),
        8 -> List((9, "a"), (3, "b")),
        9 -> List((10, "a"), (11, "b")),
        10 -> List((12, "a"), (3, "b")),
        11 -> List((5, "a"), (11, "b")),
        12 -> List((13, "a"), (11, "b")),
        13 -> List((14, "a"), (11, "b")),
        14 -> List((15, "a"), (3, "b")),
        15 -> List((15, "a"), (11, "b"))
    )

    val AFA2: Map[Int, List[(Int, String)]] = Map(
        1 -> List((1, "a"), (1, "b"))
    )

    def word_in_automata(word: String,
                         automaton: Map[Int, List[(Int, String)]],
                         final_states: Set[Int],
                         start_state: Int): Boolean = {
        var current_states = List(start_state)

        for (symbol <- word) {
            val s = symbol.toString
            var new_current_states = List[Int]()

            for (state <- current_states) {
                val transitions = automaton.getOrElse(state, Nil)
                for ((target, char) <- transitions) {
                    if (char == s) {
                        new_current_states = target :: new_current_states
                    }
                }
            }

            current_states = new_current_states
        }

        val ok = current_states.exists(final_states.contains)
        ok
    }


    def word_in_intersection(word: String,
                             automaton1: Map[Int, List[(Int, String)]],
                             final1: Set[Int],
                             start1: Int,
                             automaton2: Map[Int, List[(Int, String)]],
                             final2: Set[Int],
                             start2: Int): Boolean = {
        var state = (start1, start2)

        for (symbol <- word) {
            val (s1, s2) = state

            val targets1 = automaton1.getOrElse(s1, Nil).filter(_._2 == symbol.toString).map(_._1)
            val targets2 = automaton2.getOrElse(s2, Nil).filter(_._2 == symbol.toString).map(_._1)

            if (targets1.isEmpty || targets2.isEmpty) return false

            state = (targets1.head, targets2.head)
        }

        final1.contains(state._1) && final2.contains(state._2)
    }

    def random_word(word_len_range: (Int, Int) = (3, 20),
                    alphabet: Array[String] = ALPHABET): String = {
        val word_len = Random.between(word_len_range._1, word_len_range._2 + 1)
        var rand_word = ""

        for (_ <- 0 until word_len) {
            val idx = Random.nextInt(alphabet.length)
            rand_word += alphabet(idx)
        }

        rand_word
    }

    def random_regex_word(max_iter: Int = 10): String = {
        var word = ""

        var variants = Array("aaa", "aaaaa")
        var num_iters = Random.nextInt(max_iter + 1)
        for (_ <- 0 until num_iters) {
            word += variants(Random.nextInt(variants.length))
        }

        variants = Array("aa", "b")
        word += variants(Random.nextInt(variants.length))

        variants = Array("bb", "aab")
        num_iters = Random.nextInt(max_iter + 1)
        for (_ <- 0 until num_iters) {
            word += variants(Random.nextInt(variants.length))
        }

        word
    }

    def fuzz(regex: Regex,
             dfa: Map[Int, List[(Int, String)]],
             nfa: Map[Int, List[(Int, String)]],
             afa: (Map[Int, List[(Int, String)]], Map[Int, List[(Int, String)]]),
             num_iters: Int = 1000): Unit = {
        val dfa_finals = Set(3, 4, 9, 11, 12, 13, 15)
        val nfa_finals = Set(5)
        val afa1_finals = Set(3, 4, 9, 11, 12, 13, 15)
        val afa2_finals = Set(1)

        // Полный рандом
        for (_ <- 0 until num_iters) {
            val word = random_word()

            val in_dfa = word_in_automata(word, dfa, dfa_finals, 1)
            val in_nfa = word_in_automata(word, nfa, nfa_finals, 0)
            val in_afa = word_in_intersection(word, afa._1, afa1_finals, 1, afa._2, afa2_finals, 1)

            val in_regex = regex.matches(word)

            if (!(in_dfa == in_nfa && in_nfa == in_afa && in_afa == in_regex)) {
                println(s"Не работает на примере: $word")
                println(s"${List(in_dfa, in_nfa, in_afa, in_regex)}")
                return
            }
        }
        // Рандом соответствующий регулярке
        for (_ <- 0 until num_iters) {
            val word = random_regex_word()

            val in_dfa = word_in_automata(word, dfa, dfa_finals, 1)
            val in_nfa = word_in_automata(word, nfa, nfa_finals, 0)
            val in_afa = word_in_intersection(word, afa._1, afa1_finals, 1, afa._2, afa2_finals, 1)


            if (!(in_dfa == in_nfa && in_nfa == in_afa)) {
                val unit = println(s"Не работает на примере: $word")
                return
            }
        }
        println("Тестирование пройдено")
    }

    def main(args: Array[String]): Unit = {
        val regex = "^(aaa|aaaaa)*(b|aa)(bb|aab)*$".r
        fuzz(regex, DFA, NFA, (AFA1, AFA2))
    }
}
