# The canonical machine: a binary incrementer.  Reading the tape from the
# right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
# stops; if the carry runs off the tape a new leading 1 is added.
increment <- function(line) {
    chars <- strsplit(line, "")[[1]]
    if (length(chars) == 0 || is.na(chars[1]) || chars[1] == "") {
        return("1")
    }
    index <- length(chars)
    while (index >= 1) {
        if (chars[index] == "1") {
            chars[index] <- "0"
            index <- index - 1
        } else {
            chars[index] <- "1"
            return(paste(chars, collapse = ""))
        }
    }
    paste0("1", paste(chars, collapse = ""))
}

connection <- file("stdin", "r")
input <- readLines(connection, n = 1, warn = FALSE)
line <- if (length(input) == 0) "" else sub("[\r\n]+$", "", input[1])

if (grepl("[^01]", line)) {
    cat("Invalid input: expected a binary string.\n")
} else {
    cat("Initial tape: ", line, "\n", sep = "")
    cat("Final tape: ", increment(line), "\n", sep = "")
}
