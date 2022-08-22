rm(list = ls())

library(data.table)

# settings

players <- 4 # max 4 at the moment
max_players <- 4
fields_per_players <- 10
# names must be unique
# TODO change colors to automate
players_names <- c("red", "blue", "yellow", "black")
meeples_per_player <- 4

# rule settings
max_dice <- 6
max_num_try <- 3

move_order <- list(
  "blocked" = 0,
  "free_on_field" = 1,
  "free_in_home" = 2,
  "kick" = 3,
  "free_start" = 4
)

# source helper functions
source("R/helper.R")

# play one turn ----------------------------------------------------------

init_game(players,
          max_players,
          players_names,
          fields_per_players,
          meeples_per_player)
turn <- 0
game_runs <- TRUE
set.seed(1234)
plot <- TRUE
start_time <- Sys.time()
while (game_runs) {
  # logs
  turn <- turn + 1
  if (plot) {
    cat("\rturn:", turn, "\n")
  } else {
    cat("\rturn:", turn)
  }
  flush.console()

  # play turn
  player_idx <- (turn %% players)
  if (player_idx == 0) {
    player_idx <- players
  }
  currentplayer <- players_names[player_idx]
  field <- play_turn(field = field, currentplayer = currentplayer, try = 1)
  if (plot) {
    cat("\n")
    tmp <- print_res(start_area, info = "START")
    tmp <- print_res(end_area, info = "HOME")
    # plot board
    board <- matrix(field, nrow = max_players, byrow = TRUE)
    board[is.na(board)] <- empty_field
    print(board)
    tmp <- plot_board(field)
    Sys.sleep(0.05)
  }


  # check for winner
  this_winner <- is_winner(currentplayer)
  game_runs <- this_winner == "FALSE"
}
cat("\n")
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "s")
cat(c("Game ended with '", this_winner, "' winning after ", turn, " turns (took ", ceiling(duration) ,"s)\n"),
    sep = "")
# DONE: add kick rule
# DONE: make pretty I
# DONE: fix house movement
# DONE: save dice rolls and statistics
# DONE: make pretty II
# TODO: no overjumping at home
# TODO: map field for kicking
# TODO: test strategies

if (FALSE) {
  for (i_game in 1:10) {
    source(file = "game.R")
  }
}
