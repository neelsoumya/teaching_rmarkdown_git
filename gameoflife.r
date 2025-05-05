# Conway's Game of Life in R
# This program simulates Conway's Game of Life on a grid.

# Function to initialize the grid with a random pattern
initialize_grid <- function(rows, cols) {
  grid <- matrix(sample(c(0, 1), rows * cols, replace = TRUE, prob = c(0.8, 0.2)), nrow = rows, ncol = cols)
  return(grid)
}

# Function to count live neighbors of a cell
count_live_neighbors <- function(grid, r, c) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  
  # Define the relative positions of the 8 neighbors
  neighbors <- expand.grid(c(-1, 0, 1), c(-1, 0, 1))
  neighbors <- neighbors[!(neighbors[, 1] == 0 & neighbors[, 2] == 0), ]
  
  # Count live neighbors
  live_neighbors <- 0
  for (i in 1:nrow(neighbors)) {
    nr <- r + neighbors[i, 1]
    nc <- c + neighbors[i, 2]
    
    if (nr > 0 && nr <= rows && nc > 0 && nc <= cols) {
      live_neighbors <- live_neighbors + grid[nr, nc]
    }
  }
  
  return(live_neighbors)
}

# Function to update the grid for the next generation
update_grid <- function(grid) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  new_grid <- grid
  
  for (r in 1:rows) {
    for (c in 1:cols) {
      live_neighbors <- count_live_neighbors(grid, r, c)
      
      # Apply the rules of the Game of Life
      if (grid[r, c] == 1) {
        # Rule 1: A live cell with fewer than 2 or more than 3 live neighbors dies
        if (live_neighbors < 2 || live_neighbors > 3) {
          new_grid[r, c] <- 0
        }
      } else {
        # Rule 3: A dead cell with exactly 3 live neighbors becomes alive
        if (live_neighbors == 3) {
          new_grid[r, c] <- 1
        }
      }
    }
  }
  
  return(new_grid)
}

# Function to display the grid
display_grid <- function(grid) {
  image(t(apply(grid, 2, rev)), col = c("white", "black"), axes = FALSE)
}

# Main simulation
simulate_game_of_life <- function(rows, cols, generations) {
  # Initialize the grid
  grid <- initialize_grid(rows, cols)
  
  # Run the simulation
  for (generation in 1:generations) {
    cat("Generation:", generation, "\n")
    display_grid(grid)
    Sys.sleep(0.5)  # Pause for a short time
    grid <- update_grid(grid)
  }
}

# Run the Game of Life
simulate_game_of_life(rows = 20, cols = 20, generations = 500)
