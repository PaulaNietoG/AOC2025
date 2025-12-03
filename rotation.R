# Day 1 Advent of Code
# Paula Nieto García

# create function to rotate dial

rotate <- function(rotation, start = 50, clicks = 0){
  
  num <- as.numeric(stringr::str_extract(rotation, "[0-9]+"))
  
  if (grepl(rotation, pattern = "R")){result = start + num}
  if (grepl(rotation, pattern = "L")){result = start - num}

  # print(result)

  if ((result > 0 && result %% 100 == 0) ||
    (start == 0 && grepl("L", rotation))) {
    
    # print(rotation)
    clicks = clicks - 1
    
    while (result > 99){
      result = result - 100
      clicks = clicks + 1
      }
    while (result < 0){
      result = result + 100
      clicks = clicks + 1
      }
  } else {
    
      while (result > 99){
        result = result - 100
        clicks = clicks + 1
        }
      while (result < 0){
        result = result + 100
        clicks = clicks + 1
        }
  }
return(c(result, clicks))
}

# Test cases
# rotate("R48", start = 52)
# rotate("L5", start = 0)
# rotate("L55",start = 55)

rotation_list <- c("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

rotation_list <- read.delim("input/day1_input.txt", sep = "\n", header = FALSE)[,1]
head(rotation_list)
tail(rotation_list)

results = c()
extra_clics = 0

for (rot in rotation_list){
  # print(rot)
  
  if (length(results) == 0){
    tmp_rot <- rotate(rot) # no starting point always means start = 50
    }
  
  else {
    tmp_rot <- rotate(rot, start = tail(results,1)) # starting at last item
  }
  
  # print(tmp_rot)
  results <- c(results, tmp_rot[1])
  extra_clics = extra_clics + tmp_rot[2]
  
  # print(results)
  # print(extra_clics)
}

sum(results == 0) + extra_clics

# results
# extra_clics
