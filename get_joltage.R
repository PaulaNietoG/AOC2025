# Day 3 Advent of Code
# Paula Nieto García

# define function to obtain max joltage per battery

get_joltage <- function(battery, on = 2){
  # split in characters
  chars <- as.numeric(strsplit(as.character(battery), "")[[1]])
  # initialize joltage vector
  joltage <- c()
  # initialize first position
  position = 1
  # iterate to find as many batteries you need to turn on
  for (i in 1:on){
    # print(chars[position:(length(chars)-on+i)])
    # find the highest from the "first part" of the string
    highest_num <- max(chars[position:(length(chars)-on+i)])
    # update position so that next one starts from there
    position <- which.max(chars[position:(length(chars)-on+i)]) + position
    # print(position)
    joltage <- c(joltage, highest_num)
  }
  return(paste0(joltage, collapse = ""))
}

# test
batteries <- c(987654321111111,
               811111111111119,
               234234234234278,
               818181911112111)

# puzzle 1
batteries <- read.delim("tmp_files/day3_input.txt", header = FALSE, sep = "\n", colClasses = "character")[,1]
head(batteries)
tail(batteries)

max_jolt <- c()
for (bat in batteries) {
  max_jolt <- c(max_jolt, get_joltage(bat, on = 12))
}
# double check if length of original array is equal to length of result
length(batteries) == length(max_jolt)
as.character(sum(as.numeric(max_jolt)))
