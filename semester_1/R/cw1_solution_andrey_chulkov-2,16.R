# Задача 2.
generate_words = function(n, alphabet) {
  if (n == 1) {
    return(unlist(strsplit(alphabet, "")))
  }
  prev_result = generate_words(n - 1, alphabet)
  result = character(length(prev_result) * nchar(alphabet))
  for (i in 1:length(prev_result)) {
    for (j in 1:nchar(alphabet)) {
      result[(i - 1) * nchar(alphabet) + j] =
        paste(prev_result[i], substr(alphabet, j, j), sep="")
    }
  }
  return(result)
}

result = generate_words(3, "asdf")
print(result)
length(result) == 4^3
# Как и ожидалось.

# Задача 16.
binom_as_string = function(n) {
  factorials = integer(n + 1)
  factorials[1] = 1
  # Приходится сдвигать и хранить факториал нуля по индексу 1.
  for (i in 2:(n + 1)) {
    factorials[i] = (i - 1) * factorials[i - 1]
  }
  
  result = ""
  for (i in 0:n) {
    c = factorials[n + 1] / (factorials[i + 1] * factorials[n - i + 1])
    strc = if (c == 1) "" else as.character(c)
    plus = if (i == 0) "" else "+"
    a = if (i == n) "" else "a"
    ea = if (i >= n - 1) "" else paste("^", toString(n - i), sep="")
    b = if (i == 0) "" else "b"
    eb = if (i <= 1) "" else paste("^", toString(i), sep="")
    result = paste(result, plus, strc, a, ea, b, eb, sep="")
  }
  return(result)
}

binom_as_string(3) == "a^3+3a^2b+3ab^2+b^3"
binom_as_string(6) == "a^6+6a^5b+15a^4b^2+20a^3b^3+15a^2b^4+6ab^5+b^6"
binom_as_string(1) == "a+b"
# Все примеры работают.
