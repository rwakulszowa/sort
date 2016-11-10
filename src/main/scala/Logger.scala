package idk.yet

//TODO: use a macro / Dynamic instead
//TODO: derive each sort algorithm into OptimizedSort and LoggingSort
//      make methods final in Optimized, wrap them in `log` in Logging
object Logger {

  def log[T](name: String)
            (block: => T)
            (implicit id: String): T = {
    val res = block
    println(id, name, res)
    res
  }

}
