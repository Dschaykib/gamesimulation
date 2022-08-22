library(data.table)
library(helfRlein)

# read in the logs
logfiles_rolls <- list.files(path = "logs/", pattern = "rolls", full.names = TRUE)
logfiles_kicks <- list.files("logs/", pattern = "kick", full.names = TRUE)

rolls_dt <- helfRlein::read_files(files = logfiles_rolls, fun = fread)
kicks_dt <- helfRlein::read_files(files = logfiles_kicks, fun = fread)

minmaxscale <- function(x) {
  return((x - min(x) )/ (max(x) - min(x)))
}
percentage <- function(x) {
  return(x / sum(x))
}

# analyse rolls -----------------------------------------------------------
rolls_dt[, .N, by = this_roll]
rolls_dt[, mean(this_roll)]


# analyse kicks -----------------------------------------------------------

tmp_dt <- data.table(field = 1:40, N = 0)

kicks_norm <- kicks_dt[, .N, by = field]
kicks_norm <- kicks_dt[, .N, by = field][, N := percentage(N)]
kicks_norm <- kicks_dt[, .N, by = field][, N := minmaxscale(N)]

kicks_norm <- rbindlist(list(kicks_norm, tmp_dt), use.names = TRUE, fill = TRUE)
kicks_norm <- kicks_norm[, list(N = sum(N)), by = field][order(field)]
kicks_norm$color <- paste0("grey", round(kicks_norm$N*100))

plot(kicks_norm$field, kicks_norm$N)

pos <-c(5,5,5,5,5,4,3,2,1,1,1,2,3,4,5,5,5,5,5,6,7,7,7,7,7,8,9,10,11,11,11,10,9,8,7,7,7,7,7,6)
df_meeple <- data.frame(
  x = pos,
  y = pos[c(11:40, 1:10)])

df_meeple$color <- kicks_norm$color
#start_idx <- 0:(players-1) * fields_per_players + 1
#df_meeple$color[start_idx] <- "grey60"

#df_meeple$color[!is.na(field)] <- gsub("[[:digit:]]", "", field[!is.na(field)])
# homes
df_home <- data.frame(
  x = c(1,1,2,2,1,1,2,2,10,10,11,11,10,10,11,11)[1:(players*meeples_per_player)],
  y = c(1,2,2,1,10,11,11,10,10,11,11,10,1,2,2,1)[1:(players*meeples_per_player)])

df_home$color <- gsub("[[:digit:]]", "", names(unlist(start_area)))

# houes
df_house <- data.frame(
  x = c(6,6,6,6,2,3,4,5,6,6,6,6,10,9,8,7)[1:(players*meeples_per_player)],
  y = c(2,3,4,5,6,6,6,6,10,9,8,7,6,6,6,6)[1:(players*meeples_per_player)])

df_house$color <- gsub("[[:digit:]]", "", names(unlist(end_area)))

df <- rbind(df_meeple, df_home, df_house)

plot(x = df$x,
     y = df$y,
     type = "p",
     col = df$color,
     lwd = 8,
     main = "Kick potential",
     sub =paste0("Simulated Games: ", kicks_dt[, uniqueN(game)])
     )


