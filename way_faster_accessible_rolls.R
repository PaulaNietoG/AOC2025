# IMPROVED VERSION, LESS TIME MORE FAST 
# Day 4 Advent of Code
# Paula Nieto García

# create function to shift matrix
# now using logicals and returning 0s
shift <- function(mat, dr, dc) {
  R <- nrow(mat)
  C <- ncol(mat)
  out <- matrix(0L, R, C)  # or FALSE for logical
  
  r_range <- max(1, 1-dr):min(R, R-dr)
  c_range <- max(1, 1-dc):min(C, C-dc)
  
  out[r_range + dr, c_range + dc] <- mat[r_range, c_range]
  out
}

# create function to get neighbors
# using the shift function above
# now vectorized and faster
# directly counts neighbors which are @
count_neighbors <- function(mat) {
  is_at <- (mat == "@")
  N  <- shift(is_at, -1, 0)
  S  <- shift(is_at, 1, 0)
  W  <- shift(is_at, 0, -1)
  E  <- shift(is_at, 0, 1)
  NW <- shift(is_at, -1, -1)
  NE <- shift(is_at, -1, 1)
  SW <- shift(is_at, 1, -1)
  SE <- shift(is_at, 1, 1)
  
  N + S + W + E + NW + NE + SW + SE
}

# test matrix
diagram <- c("..@@.@@@@.",
             "@@@.@.@.@@",
             "@@@@@.@.@@",
             "@.@@@@..@.",
             "@@.@@@@.@@",
             ".@@@@@@@.@",
             ".@.@.@.@@@",
             "@.@@@.@@@@",
             ".@@@@@@@@.",
             "@.@.@@@.@.")

diagram <- readLines("tmp_files/day4_input.txt")

# convert to matrix
m <- do.call(rbind, strsplit(diagram, split = ""))
total_flips <- 0
# adding repeat to keep iterating until no more changes
# now using vectorized approach MCH FASTER
system.time(
  repeat {
    old <- m
    neighbor_count <- count_neighbors(old)
    to_flip <- (old == "@") & (neighbor_count < 4)
    
    flips_this_round <- sum(to_flip)
    print(flips_this_round)
    total_flips <- total_flips + flips_this_round
    
    if (flips_this_round == 0) break
    m[to_flip] <- "."
  }
)
print(total_flips)
# takes 0.102 seconds