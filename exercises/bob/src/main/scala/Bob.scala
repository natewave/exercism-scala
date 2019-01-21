object Bob {
  def response(statement: String): String = {
    val trimmed = statement.trim 
    val isQuestion = trimmed.endsWith("?")
    val isShooting = trimmed.matches(".*[A-Z].*") && trimmed.toUpperCase == statement
    
    if (isQuestion && isShooting)
      "Calm down, I know what I'm doing!"
    else if (isQuestion)
      "Sure."
    else if (isShooting)
      "Whoa, chill out!"
    else if (trimmed.isEmpty)
      "Fine. Be that way!"
    else 
      "Whatever."
  }
}
