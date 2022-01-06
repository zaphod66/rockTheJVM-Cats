package playground

import cats.Eval

object Playground {
  val meaningOfLife: Eval[Int] = Eval.later {
    println("Learning Cats: computing abstractions and the meaning of life...")
    42
  }

  def perm[A](list: List[A]): List[List[A]] = {
    def remove(elem: A): List[A] = {
      val index    = list.indexOf(elem)
      val (l1, l2) = list.splitAt(index)
      l1 ++ l2.tail
    }

    if (list.isEmpty) List(list)
    else {
      list.flatMap{ elem =>
        val remainList = remove(elem)
        val partPerms = perm(remainList)
        partPerms.map(l => elem :: l)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(meaningOfLife.value)

    val l1 = List(1, 2, 3, 2, 1)
    val p1 = l1.permutations.toList
    val p2 = perm(l1)

    println(s"p1: ${p1.size} - $p1")
    println(s"p2: ${p2.size} - ${p2.distinct} - ${p2.distinct.size}")
  }
}
