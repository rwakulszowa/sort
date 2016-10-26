package idk.yet

//TODO: use a macro / Dynamic instead
//TODO: derive each sort algorithm into OptimizedSort and LoggingSort
//      make methods final in Optimized, wrap them in `log` in Logging
object Logger {
 
  def log[T](block: => T): T = {
    val res = block
    println(res)
    res
  }

}


object NoopLogger {

  def log[T](block: T): T = block

}
