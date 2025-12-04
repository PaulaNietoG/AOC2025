# Day 4 Advent of Code
# Paula Nieto García

# create function to shift matrix
# will be useful to get neighbors
shift <- function(mat, dr, dc) {
  # input is mat: a matrix
  # dr: rows to shift (positive: down, negative: up)
  # dc: columns to shift (positive: right, negative: left)
  R <- nrow(mat)
  C <- ncol(mat)
  # matrix pre-filled with .  
  out <- matrix(".", R, C)
  r_src <- (1:R) - dr
  c_src <- (1:C) - dc
  valid_r <- r_src >= 1 & r_src <= R
  valid_c <- c_src >= 1 & c_src <= C
  out[valid_r, valid_c] <- mat[r_src[valid_r], c_src[valid_c], drop = FALSE]
  out
}

# create function to get neighbors
# using the shift function above
get_neighbors <- function(mat, r, c) {
  # these are the 8 adjacent positions
  N <- shift(mat, -1, 0)
  S <- shift(mat, 1, 0)
  W <- shift(mat, 0, -1)
  E <- shift(mat, 0, 1)
  NW <- shift(mat, -1, -1)
  NE <- shift(mat, -1, 1)
  SW <- shift(mat, 1, -1)
  SE <- shift(mat, 1, 1)
  return(c(N[r, c], NE[r, c], E[r, c], SE[r, c],
    S[r, c], SW[r, c], W[r, c], NW[r, c]))
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
system.time(
  repeat {
    old <- m # snapshot of start-of-round state
    to_flip <- matrix(FALSE, nrow(m), ncol(m))
    for (r in seq_len(nrow(m))) {
      for (c in seq_len(ncol(m))) {
        if (old[r, c] == "@") {
          neighbors <- get_neighbors(old, r, c)
          if (sum(neighbors == "@") < 4) {
            to_flip[r, c] <- TRUE
          }
        }
      }
    }
    print(sum(to_flip == TRUE))
    total_flips <- total_flips + sum(to_flip == TRUE)
    if (!any(to_flip)) break # no changes -> finished
    m[to_flip] <- "." # apply all flips at once
  }
)
print(total_flips)
# takes 537.814 seconds