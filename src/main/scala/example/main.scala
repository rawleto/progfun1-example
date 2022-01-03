package example

@main def run(): Unit =
    val value = List(1, 2, 3)
    println("List: " + value)
    println("Max: " + Lists.max(value))
    println("Sum: " + Lists.sum(value))
