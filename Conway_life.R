# ---------------------------------------------------------------------- #
# Initialize ----------------------------------------------------------- #
# ---------------------------------------------------------------------- #

library("raster") # for the "raster()" function
library("rgdal")  # used to make the "raster()" function work with
setwd("C:/Users/Lowell Moore/Desktop/Game of Life")

# Generate game of life board
load_board <- function(board_source = "random", board_rows = 10, board_cols = 10
                       , sprinkle_k = 0.25){
  # generate board at random
  if(board_source == "random"){
    n_cells <- board_rows * board_cols
    board_import <- matrix(0, nrow = board_rows, ncol = board_cols)
    random_flag <- sample(size = n_cells * sprinkle_k, x = 1:n_cells, replace = TRUE)
    board_import[random_flag] <- 1
  }
  
  # Import board from .png image
  if(board_source != "random"){
    # Import board as .png image where live pixels are black and dead pixels are white
    #   Note: this can be generated easily in MS Paint for example
    image_path <- board_source
    board_import <- raster(image_path)
    board_import <- as.matrix(board_import)
    flag <- which(board_import < 10)
    board_import[flag] <- 1
    flag <- which(board_import > 250)
    board_import[flag] <- 0
  }
  
  return(board_import)
}

# initialize dataframe to store life sessions
init_life_history <- function(import_path = "none"){
  if(import_path == "none"){
    life_runs <- data.frame(board_rows = numeric(0)
                            , board_cols = numeric(0)
                            , n_cells = numeric(0)
                            , n_steps = numeric(0)
                            , sprinkle_k = numeric(0)
                            , pop_life = numeric(0)
                            , start_life = character(0)
    )
  }
  
  if(import_path != "none"){
    life_runs <- read.csv(import_path, stringsAsFactors = FALSE)
  }
  
  return(life_runs)
}

# Function to calculate the next frame from the previous frame
life <- function(input_frame, birth = c(3), sustain = c(2, 3)){
  board_rows <- nrow(input_frame)
  board_cols <- ncol(input_frame)
  
  # Modified input frame with cells looped around the outside
  frame_i <- rbind(input_frame[nrow(input_frame),]
                   , input_frame
                   , input_frame[1,])
  frame_i <- cbind(frame_i[,ncol(frame_i)]
                   , frame_i
                   , frame_i[,1])
  
  # Loop over cells to calculate next frame
  frame_f <- matrix(0, nrow = board_rows+2, ncol = board_cols+2)
  for(i in 2:(nrow(frame_i)-1)){
    for(j in 2:(ncol(frame_i)-1)){
      neighbors_ij <- sum(frame_i[(i-1):(i+1)
                                , (j-1):(j+1)]
      ) - frame_i[i, j]
      
      if(neighbors_ij %in% sustain){frame_f[i, j] <- frame_i[i, j]}
      if(neighbors_ij %in% birth){frame_f[i, j] <- 1}
    }
  }
  
  frame_f <- frame_f[2:(nrow(frame_f)-1), 2:(ncol(frame_f)-1)]
  
  return(frame_f)
}

# function to run the life() function for n iterations
run_life <- function(board_import, n_steps){
  # Debug: n_steps <- 5
  frame_i <- board_import
  step_i <- 1
  start_life <- as.vector(frame_i)
  save_life <- as.vector(frame_i)
  pop_life <- sum(frame_i)
  while(step_i <= n_steps){
    print(step_i)
    frame_f <- life(frame_i)
    pop_life <- paste(pop_life, sum(frame_f))
    save_life <- c(save_life, as.vector(frame_f))
    frame_i <- frame_f
    step_i <- step_i + 1
  }
  
  # Save game session as a long character string to be parsed later
  save_life <- paste(save_life, collapse = "")
  start_life <- paste(start_life, collapse = "")
  
  # Parse all information about this session as a row to add to the life_runs df
  life_run <- data.frame(board_rows = nrow(board_import)
                         , board_cols = ncol(board_import)
                         , n_cells = nrow(board_import) * ncol(board_import)
                         , n_steps = n_steps
                         , sprinkle_k = "not specified"
                         , pop_life = pop_life
                         , start_life = start_life
  )
  return(list(life_run, save_life))
}

# function to load a game of life session from a saved history
get_board <- function(save_in, n_frame = 1, board_rows, board_cols){
  n_cells <- board_rows * board_cols
  
  start_cell <- ((n_frame - 1) * n_cells) + 1
  end_cell <- n_frame * n_cells
  
  frame_out <- substr(save_in, start = start_cell, stop = end_cell)
  frame_out <- as.numeric(strsplit(frame_out, split = "")[[1]])
  frame_out <- matrix(frame_out, ncol = board_cols)
  return(frame_out)
}

# Plot population curve for a game session
pop_curve <- function(run_num, plot_ylim = c(0, 100), plot_xlim = c(0, 100)){
  plot_main <- paste("Runs:", paste(run_num, collapse = ", "))
  plot(0, 0, xlim = plot_xlim, ylim = plot_ylim, type = "n"
       , main = plot_main, xlab = "Step", ylab = "Population")
  for(i in run_num){
    pops <- strsplit(life_runs$pop_life[i], split = " ")[[1]]
    lines(pops, col = rgb(0, 0, 0, 0.5))
  }
}


# ---------------------------------------------------------------------- #
# RUN ------------------------------------------------------------------ #
# ---------------------------------------------------------------------- #
  
# Initialize log file
life_runs <- init_life_history()

# Load a life board
board_import <- load_board("methuselah.png")
plot(as.raster(board_import))

# run a game session and save the result
new_life <- run_life(board_import, 2000)
life_runs <- rbind(life_runs, new_life[[1]]) 

# Draw a population curve for a game run
pdf("methuselah population 2000 gens.pdf", width = 6, height = 4)
pop_curve(1, plot_xlim = c(0, 2000), plot_ylim = c(0, 400))
dev.off()

run_num <- nrow(life_runs)
frame_get <- round(locator(1)$x, 0)
frame_get <- get_board(new_life[[2]][run_num]
                      , board_rows = new_life[[1]]$board_rows[run_num]
                      , board_cols = new_life[[1]]$board_cols[run_num]
                      , n_frame = frame_get)

plot(as.raster(frame_get))
frame_get <- frame_get + 1


# ---------------------------------------------------------------------- #
# Save data ------------------------------------------------------------ #
# ---------------------------------------------------------------------- #

save_path <- "life_history.csv"
write.csv(x = life_runs, file = save_path, quote = FALSE, row.names = FALSE)

run_num <- 4
frame_get <- 1
save_board <- frame_get

save_path <- "life_board methuselah stable.png"
padding_factor <- 1
png(filename = save_path
    , width = padding_factor*ncol(save_board)
    , height = padding_factor*nrow(save_board)
    , type = "cairo", antialias = NULL)
par(mar = c(0, 0, 0, 0))
plot(as.raster((-1*save_board)+1))
dev.off()
par(mar = c(5, 4, 4, 2) + 0.1)

# Save a population curve
pdf(file = "popcurve.pdf", width = 6, height = 6, useDingbats = FALSE)
pop_curve(1:5, plot_xlim = c(0, 80), plot_ylim = c(0, 50))
dev.off()

