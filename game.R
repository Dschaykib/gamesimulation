rm(list = ls())

library(helfRlein)
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



# functions ---------------------------------------------------------------
checkdir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path = path, showWarnings = FALSE, recursive = TRUE)
  }
}

init_game <- function(players, max_players, players_names, fields_per_players,
                      meeples_per_player) {
  empty_field <- paste0(rep(" ", max(sapply(players_names[seq_len(players)], nchar)) +
                              nchar(meeples_per_player)), collapse = "")
  field_size <- max_players * fields_per_players
  field <- rep(NA_character_, field_size)

  inital_mat <- matrix(c(players_names,
                         rep("", max_players * (fields_per_players - 1))),
                       ncol = max_players,
                       byrow = TRUE)
  inital_mat[nrow(inital_mat),] <- paste0(players_names, "_home")[c(2:max_players, 1)]
  names(field) <- c(inital_mat)

  # create start and end
  start_area <- rep(list(rep(seq_len(meeples_per_player))), players)
  names(start_area) <- players_names[seq_len(players)]

  end_area <- rep(list(rep(NA_character_, meeples_per_player)), players)
  names(end_area) <- players_names[seq_len(players)]

  # log files
  this_time <- gsub(" ", "_", gsub(":", "-", Sys.time()))
  log_roll_file <- paste0("logs/log_rols_", this_time, ".csv")
  #helfRlein::checkdir(dirname(log_roll_file))
  checkdir(dirname(log_roll_file))
  data.table::fwrite(x = list("turn;try;this_roll;currentplayer"),
                     file = log_roll_file, append = FALSE, row.names = FALSE)

  log_kick_file <- paste0("logs/log_kick_", this_time, ".csv")
  #helfRlein::checkdir(dirname(log_kick_file))
  checkdir(dirname(log_kick_file))
  data.table::fwrite(x = list("turn;try;field;kicked;from"),
                     file = log_kick_file, append = FALSE, row.names = FALSE)

  # bring to global
  empty_field <<- empty_field
  field <<- field
  start_area <<- start_area
  end_area <<- end_area
  log_roll_file <<- log_roll_file
  log_kick_file <<- log_kick_file

}

retry <- function(currentplayer, try) {
  # TODO: all meeple at home must be in order from the top, no free spaces between them
  if (try >= max_num_try) {
    out <- FALSE
  } else {
    out <-
      all(start_area[[currentplayer]] != 0) ||
      ( (sum(start_area[[currentplayer]] != 0) +
           sum(!is.na(end_area[[currentplayer]]))
      ) == meeples_per_player)
    # check that not all are already at home
    out <- out & any(!is.na(end_area[[currentplayer]]))
  }
  return(out)
}

play_turn <- function(field, currentplayer, try = 1) {
  # dice roll
  reroll <- FALSE
  this_roll <- sample(1:max_dice, size = 1)
  data.table::fwrite(x = list(turn, try, this_roll, currentplayer),
                     file = log_roll_file,
                     sep = ";",
                     append = TRUE,
                     row.names = FALSE)
  if (plot) {
    cat(currentplayer, "rolls:", this_roll, " ")
  }
  # check home
  # if (turn == 161) {
  #   browser()
  # }
  if ( this_roll == max_dice ) {
    reroll <- TRUE
    start_field <- which(names(field) == currentplayer)
    if ( any(start_area[[currentplayer]] != 0) ) {
      # are there any meepple in the house?

      idx <- which(start_area[[currentplayer]] != 0)[1]
      next_meeple <- paste0(currentplayer, start_area[[currentplayer]][idx])


      if ( is.na(field[start_field]) ) {
        # home plate is free
        start_area[[currentplayer]][idx] <- 0
        start_area <<- start_area
        field[start_field] <- next_meeple
      } else if ( grepl(currentplayer, field[start_field]) ) {
        # home plate is full with own meeple
        field <- move_meeple(index = start_field,
                             roll = this_roll,
                             field = field,
                             currentplayer = currentplayer)

      } else {
        # home plate is full with other meeple
        start_area[[currentplayer]][idx] <- 0
        start_area <<- start_area
        # logs
        data.table::fwrite(
          x = list(turn,try,start_field,field[start_field],next_meeple),
          file = log_kick_file,
          sep = ";",
          append = TRUE,
          row.names = FALSE)
        # send meeple home
        kick_meeple(field[start_field])
        # move kicker
        field[start_field] <- next_meeple



      }

    } else {
      # pick and move meeple
      field <- pick_and_move_meeple(field, currentplayer, roll = this_roll)


    }

  } else {
    # pick and move meeple
    field <- pick_and_move_meeple(field, currentplayer, roll = this_roll)

  }
  ## check for kick out
  ## move meeple depending on strategy
  ## - first, last, random
  if ( reroll ) {
    field <- play_turn(field = field,
                       currentplayer = currentplayer,
                       try = max_num_try)
  } else {
    # check if roll again is possible
    # all at start or at home
    if ( retry(currentplayer, try) ) {
      field <- play_turn(field = field,
                         currentplayer = currentplayer,
                         try = try + 1)
    }

  }
  return(field)
}

pick_meeple <- function(field,
                        currentplayer,
                        roll,
                        strategy = "first") {
  #field2 <- field;field <- c("red1", "red2", NA, NA); strategy<-"home"; roll<-2
  # check all meeples
  possible_meeple <- grep(currentplayer, field)
  meeple_check <- vector(mode = "numeric", length = length(possible_meeple))
  for (i_meeple in seq_along(possible_meeple)) {
    meeple_check[i_meeple] <-
      check_meeple(index = possible_meeple[i_meeple],
                   roll = roll,
                   field = field,
                   currentplayer = currentplayer)
  }


  possible_meeple_home <- grep(currentplayer, end_area[[currentplayer]])
  meeple_check_home <- vector(mode = "numeric", length = length(possible_meeple_home))
  for (i_meeple in seq_along(possible_meeple_home)) {
    meeple_check_home[i_meeple] <-
      check_meeple_home(index = possible_meeple_home[i_meeple],
                        roll = roll,
                        field = end_area[[currentplayer]],
                        currentplayer = currentplayer)

  }

  # check for max value as priority
  pick <- c()
  max_field <- max(c(meeple_check, -Inf))
  max_home <- max(c(meeple_check_home, -Inf))

  if (max_field <= max_home) {
    all_meeples_pos <- possible_meeple_home[
      meeple_check_home > 0 &
        meeple_check_home == suppressWarnings(max(meeple_check_home))]

    pick <- c("home" = all_meeples_pos[which.max(all_meeples_pos)])
  } else {
    all_meeples_pos <- possible_meeple[
      meeple_check > 0 & meeple_check == suppressWarnings(max(meeple_check))]
    # pick meeple
    shift_num <- which(names(field) == currentplayer)
    if ( strategy == "first" ) {
      idx <- which.max(ifelse(
        all_meeples_pos < shift_num,
        all_meeples_pos + length(field),
        all_meeples_pos))
      pick <- c("field" = all_meeples_pos[idx])
    } else if ( strategy == "last" ) {
      idx <- which.min(ifelse(
        all_meeples_pos < shift_num,
        all_meeples_pos + length(field),
        all_meeples_pos))
      pick <- c("field" = all_meeples_pos[idx])
    }
  }

  return(pick)
}

kick_meeple <- function(x) {
  x_color <- gsub("[0-9]", "", x)
  x_idx <- as.numeric(gsub("[[:alpha:]]", "", x))
  start_area[[x_color]][x_idx] <- x_idx
  # set to global, ugly but useful
  start_area <<- start_area
}

check_meeple <- function(index, roll, field, currentplayer) {
  # index <- 40; roll <- 4
  # set target
  target <- (index + roll) %% length(field)
  if (target == 0) {
    target <- 40
  }

  # check if end is available
  this_end_idx <- which(names(field) == paste0(currentplayer, "_home"))
  if ( index <= this_end_idx && index > this_end_idx - roll ) {
    home_target <- this_end_idx - index + roll
    # check if home was over shot
    if (home_target > meeples_per_player) {
      check_result <- move_order[["blocked"]]
    } else if (is.na(end_area[[currentplayer]][home_target])) {
      # home target is free
      check_result <- move_order[["free_in_home"]]
    } else {
      # home targe is not free
      check_result <- move_order[["blocked"]]
    }

  } else {
    # check if target is free
    if (is.na(field[target])) {
      check_result <- move_order[["free_on_field"]]
    } else {
      if ( grepl(currentplayer, field[target]) ) {
        # target is blocked by own meeple
        check_result <- move_order[["blocked"]]
      } else {
        # target is blocked by other meeple
        check_result <- move_order[["kick"]]
      }
    }
  }
  # check start field
  if (index == which(names(field) == currentplayer) & check_result != 0) {
    check_result <- move_order[["free_start"]]
  }
  return(check_result)
}

check_meeple_home <- function(index, roll, field, currentplayer) {
  # index <- 40; roll <- 4
  # set target
  target <- (index + roll)
  if (target > length(field)) {
    # target overshot
    check_result <- move_order[["blocked"]]
  } else {
    # check if target is free
    if (is.na(field[target])) {
      check_result <- move_order[["free_in_home"]]
    } else {
      # target is blocked
      check_result <- move_order[["blocked"]]

    }
  }
  return(check_result)
}


move_meeple <- function(index, roll, field, currentplayer) {
  # index <- 40; roll <- 4
  # set target
  target <- (index + roll) %% length(field)
  if (target == 0) {
    target <- 40
  }
  # check if end is available
  this_end_idx <- which(names(field) == paste0(currentplayer, "_home"))
  if ( index <= this_end_idx && index > this_end_idx - roll ) {
    home_target <- this_end_idx - index + roll
    # check if home was over shot
    if (home_target > meeples_per_player) {
      if (plot) stop("rolled to high!")
      return(field)
    }
    # check if home target is free
    if (is.na(end_area[[currentplayer]][home_target])) {
      end_area[[currentplayer]][home_target] <- field[index]
      end_area <<- end_area
      field[index] <- NA_character_
    } else {
      if (plot) stop("target home is full")
      return(field)
    }
  } else {
    # check if target is free
    if (is.na(field[target])) {
      field[target] <- field[index]
      field[index] <- NA_character_
    } else {
      if ( grepl(currentplayer, field[target]) ) {
        # target is blocked by own meeple
        field <-move_meeple(
          index = target,
          roll = roll,
          field = field,
          currentplayer = currentplayer)
      } else {
        # target is blocked by other meeple
        prev_meeple <- field[target]
        field[target] <- field[index]
        field[index] <- NA_character_

        # logs
        data.table::fwrite(
          x = list(turn,0,target,prev_meeple,field[target]),
          file = log_kick_file,
          sep = ";",
          append = TRUE,
          row.names = FALSE)
        # send meeple home
        kick_meeple(prev_meeple)

      }
    }
  }
  return(field)
}

move_meeple_home <- function(index, roll, field) {
  # set target
  target <- (index + roll)

  if (is.na(field[target])) {
    field[target] <- field[index]
    field[index] <- NA_character_
  }
  return(field)
}


pick_and_move_meeple <- function(field, currentplayer, roll) {

  # check home
  # this_meeple_home <- pick_meeple(field = end_area[[currentplayer]],
  #                            currentplayer = currentplayer,
  #                            roll = roll,
  #                            strategy = "home")
  # check field
  this_meeple <- pick_meeple(field, currentplayer, roll = roll)


  if (length(this_meeple) != 0) {
    if (names(this_meeple) == "home") {
      end_area[[currentplayer]] <- move_meeple_home(index = this_meeple,
                                                    roll = roll,
                                                    field = end_area[[currentplayer]])
      end_area <<- end_area
    } else {
      # move meeple
      field <- move_meeple(index = this_meeple,
                           roll = roll,
                           field = field,
                           currentplayer = currentplayer)
    }
  }
  return(field)
}
# check winner
is_winner <- function(currentplayer) {
  #out <- sample(x = c("red", "blue",F), prob = c(0.05, 0.05,  0.9), size = 1)
  check <- all(!is.na(end_area[[currentplayer]]))
  if (check) {
    out <- currentplayer
  } else {
    out <- FALSE
  }
  return(out)
}


print_res <- function(list, info = "", maxspace = 20) {
  #list <- start_area;maxspace <- 10; info<-"START"

  # max chars per meeple name times number of meeples plus spaces in between
  maxspace <- max(
    maxspace,
    nchar(empty_field) * meeples_per_player + (meeples_per_player - 1))

  if (info != "") {
    titlespace <- floor((maxspace * players - nchar(info)) / 2)
    print(sprintf(paste0("%", titlespace, "s"), info))
  }

  title <- paste0(sprintf(paste0("%", maxspace, "s"), names(list)), collapse = "")
  tmp <- lapply(list, paste0, collapse = " ")
  main <- paste0(sprintf(paste0("%", maxspace, "s"), tmp), collapse = "")
  print(title)
  print(main)

  return()
}

plot_board <- function(field) {
  if (max_players == 4) {

    # position meeples
    pos <-c(5,5,5,5,5,4,3,2,1,1,1,2,3,4,5,5,5,5,5,6,7,7,7,7,7,8,9,10,11,11,11,10,9,8,7,7,7,7,7,6)
    df_meeple <- data.frame(
      x = pos,
      y = pos[c(11:40, 1:10)])

    df_meeple$color <- "grey90"
    start_idx <- 0:(players-1) * fields_per_players + 1
    df_meeple$color[start_idx] <- "grey60"

    df_meeple$color[!is.na(field)] <- gsub("[[:digit:]]", "", field[!is.na(field)])
    # homes
    df_home <- data.frame(
      x = c(1,1,2,2,1,1,2,2,10,10,11,11,10,10,11,11)[1:(players*meeples_per_player)],
      y = c(1,2,2,1,10,11,11,10,10,11,11,10,1,2,2,1)[1:(players*meeples_per_player)])

    df_home$color <- ifelse(unlist(start_area) == 0, "grey40", gsub("[[:digit:]]", "", names(unlist(start_area))))

    # houes
    df_house <- data.frame(
      x = c(6,6,6,6,2,3,4,5,6,6,6,6,10,9,8,7)[1:(players*meeples_per_player)],
      y = c(2,3,4,5,6,6,6,6,10,9,8,7,6,6,6,6)[1:(players*meeples_per_player)])

    df_house$color <- ifelse(is.na(unlist(end_area)), "grey70", gsub("[[:digit:]]", "", names(unlist(end_area))))

    df <- rbind(df_meeple, df_home, df_house)

    plot(x = df$x,
         y = df$y,
         type = "p",
         col = df$color,
         lwd = 8,
         main = paste0("Turn: ", turn))

  }
}
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
    Sys.sleep(0.2)
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
