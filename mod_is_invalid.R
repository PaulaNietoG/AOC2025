# Day 3 Advent of Code
# Paula Nieto García
# challenge #2

# update the is_valid function for second set of rules

# first define a function that splits a character vector into pices of size n
# and returns a vector of strings
split_into_chunks <- function(chars, n) {
  starts <- seq_len(length(chars) / n)
  idx <- rep(starts, each = n)
  vapply(split(chars, idx), paste0, collapse = "", FUN.VALUE = character(1))
}

# re-do the is-valid function
mod_is_invalid <- function(num){
  chars <- as.character(strsplit(as.character(num), "")[[1]])
  n <- length(chars)
  repeat_chars <- sum(table(chars) > 1)
  
  # check if there are repeated characters (else it cannot be invalid)
  if (repeat_chars > 0){
    # check if all characters are the same
    if (length(unique(chars)) == 1) {
      return(num)
    } else {
      # iterate from 2 to half the length
      for (i in c(2:round(length(chars)/2))) {
        # check if length is multiple of that number
        # otherwise don't split
        if (length(chars) %% i == 0){
          # print(i)
          # split in portions of size i
          chunks <- split_into_chunks(chars, i)
          if(length(unique(chunks)) == 1) {return(num)}
        }
      }
    }
  }
}

# test ranges
ranges <- c("11-22", "95-115", "998-1012", "1188511880-1188511890", "222220-222224", 
            "1698522-1698528", "446443-446449", "38593856-38593862", "565653-565659", 
            "824824821-824824827", "2121212118-2121212124")

ranges <- trimws(unlist(strsplit(readLines("tmp_files/day2_input.txt"), ",")))
head(ranges)
tail(ranges)

invalid_numbers <- c()
for (r in ranges) {
  parts <- strsplit(r, "-")[[1]]
  start <- as.numeric(parts[1])
  # print(start)
  end <- as.numeric(parts[2])
  # print(end)
  
  for (d in start:end) {
    invalid_numbers <- c(invalid_numbers, mod_is_invalid(d))
  }
}
# invalid_numbers
length(invalid_numbers)
sum(invalid_numbers)
