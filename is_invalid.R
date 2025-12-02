# create function to decide if a number is valid or not (first set of rules)

is_invalid <- function(num){
  chars <- as.character(strsplit(as.character(num), "")[[1]])
  n <- length(chars)
  repeat_chars <- sum(table(chars) > 1)
  
  # to save some time only check those numbers with repeated characters
  # and those whose lenth is even
  if (n %% 2 == 0 && repeat_chars > 0){
    first_half <- paste0(chars[1:(n/2)], collapse = "")
    # print(first_half)
    second_half <- paste0(chars[(n/2 + 1):n], collapse = "")
    # print(second_half)
    if (first_half == second_half){
      return(num)
    }
  }
}

# test ranges
ranges <- c("11-22", "95-115", "998-1012", "1188511880-1188511890", "222220-222224", 
            "1698522-1698528", "446443-446449", "38593856-38593862", "565653-565659", 
            "824824821-824824827", "2121212118-2121212124")

# actual input
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
  
  for (i in start:end) {
    invalid_numbers <- c(invalid_numbers, is_invalid(i))
  }
}

length(invalid_numbers)
sum(invalid_numbers)
