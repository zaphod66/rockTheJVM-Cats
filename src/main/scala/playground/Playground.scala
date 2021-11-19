package playground

import cats.Eval

object Playground:
  val meaningOfLife: Eval[Int] = Eval.later {
    println("Learning Cats: computing abstractions and the meaning of life...")
    42
  }

  @main
  def run: Unit =
    println(meaningOfLife.value)
