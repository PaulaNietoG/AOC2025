rotate <- function(rotation, start = 50, clicks = 0){
  num <- as.numeric(stringr::str_extract(rotation, "[0-9]+"))
  
  if (grepl(rotation, pattern = "R")){result = start + num}
  if (grepl(rotation, pattern = "L")){result = start - num}
  
  while (result > 99){
    result = result - 100
    clicks = clicks + 1
    }
  while (result < 0){
    result = result + 100
    clicks = clicks + 1
    }
  
  return(c(result, clicks))
  
}

rotation_list <- c("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

# rotation_list <- read.delim("/novo/users/pnfg/tmp_files/input.txt", sep = "\n", header = FALSE)[,1]
head(rotation_list)
tail(rotation_list)

results = c()
extra_clics = 0

for (rot in rotation_list){
  print(rot)
  
  if (length(results) == 0){
    tmp_rot <- rotate(rot) # no starting point always means start = 50
    }
  
  else {
    tmp_rot <- rotate(rot, start = tail(results,1)) # starting at last item
  }
  
  results <- c(results, tmp_rot[1])
  extra_clics = extra_clics + tmp_rot[2]
  
  print(results)
  print(extra_clics)
}

sum(results == 0) + extra_clics