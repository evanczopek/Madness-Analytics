# Evan Czopek - 1/29/2017 6:00pm

# Input for desired analyzed categories and weights.
cat_choose = c('wlp', 'sos', 'points_against', 'trb', 'tov')
cat_weight = c(3, 5, 2, 1, 1)

# Query of Sports Reference to get relevant table on all stats for NCAA teams. 
library(XML)
url_one <- "http://www.sports-reference.com/cbb/seasons/2017-school-stats.html"
cc = c(rep('character',2), rep('integer',3), rep('numeric',3), rep('integer',8), 'character', rep('integer',3),
       'numeric', rep('integer',2), 'numeric', rep('integer',2), 'numeric', rep('integer',7))
all_data_df <- readHTMLTable(url_one, which = 1, colClasses = cc )

# Clears all rows that hold column headers. 
del_rows = c(which(all_data_df$Rk == ''),which(all_data_df$Rk == 'Rk'))
all_data_df = all_data_df[-del_rows,]

# Clear redundant first column 'Rk'
all_data_df$Rk = NULL
all_data_df$`Â ` = NULL

# Renaming columns to more clear entries for manipulation. See README for definitions.
colnames(all_data_df) = c('school', 'gp', 'wins', 'loss', 'wlp', 'srs', 'sos', 'conf_wins',
                          'conf_loss', 'home_wins', 'home_loss', 'away_wins', 'away_loss',
                          'points_for', 'points_against', 'min_played', 'fg', 'fga', 'fgp', 
                          'thrp', 'thrpa', 'thrpp', 'ft', 'fta', 'ftp', 'orb', 'trb', 'ast', 'stl', 
                          'blk', 'tov', 'pf')

# Adding new columns. Two pointers made, attempted and percentage.
library(dplyr)
all_data_df = mutate(all_data_df, twop = fg - thrp, twopa = fga - thrpa, twopp = twop / twopa)

all_data_df[109,13] = 1

# Generate the names of the teams in each bracket. In order 1-16 seed.
midwest = c('Kansas', 'Louisville', 'Oregon', 'Purdue', 'Iowa State', "Creighton", 
            'Michigan', 'Miami (FL)', 'Michigan State', 'Oklahoma State', 
            'Rhode Island', 'Nevada', 'Vermont', 'Iona', 'Jacksonville State', 
            'UC-Davis')
east = c('Villanova', 'Duke', 'Baylor', 'Florida', 'Virginia', 'Southern Methodist', 
         'South Carolina', 'Wisconsin', 'Virginia Tech', 'Marquette', 'Southern California', 
         'North Carolina-Wilmington', 'East Tennessee State', 'New Mexico State', 'Troy', "Mount St. Mary's")
west = c('Gonzaga', 'Arizona', 'Florida State', 'West Virginia', 'Notre Dame', 'Maryland', "Saint Mary's (CA)", 
         'Northwestern', 'Vanderbilt', 'Virginia Commonwealth', 'Xavier', 'Princeton', 
         'Bucknell', 'Florida Gulf Coast', 'North Dakota', 'South Dakota State')
south = c('North Carolina', 'Kentucky', 'UCLA', 'Butler', 'Minnesota', 'Cincinnati', 
          'Dayton', 'Arkansas', 'Seton Hall', 'Wichita State', 'Kansas State', 'Middle Tennessee', 
          'Winthrop', 'Kent State', 'Northern Kentucky', 'Texas Southern')

# Combine bracket teams in order, 1-4 seeds as 1 seeds.
teams = c(east, midwest, west, south)

# Choose which rows to save from large, original data set.
save_row = matrix(NA, nrow = 1, ncol = 64)
for (x in 1:length(teams)) {
  save_row[x] = which(all_data_df$school == paste(teams[x],'*'))
}

# Keep data fromonly desired teams/rows.
t_data = all_data_df[save_row,]

# Generate scoring criteria for categories where larger numbers are better. All
# values are generated as ratios compared to the best scoring team in that
# category i.e. the team with most wins will score a 1 in the wins category, a
# team with half as many wins will score a .5.
p_data = matrix(NA, nrow = 64, ncol = length(t_data))
p_data[, 1] = as.character(t_data$school)
big_good = c(2, 3, 5, 6, 7, 8, 10, 12, 14, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
             33, 34, 35)
for (x in 1:length(big_good)) {
  j = big_good[x]
  max = max(t_data[, j])
  for (i in 1:64) {
    p_data[i, j] = t_data[i, j]/max
  }
}

# Does the same as above for remaining rows where smaller numbers are better. 
small_good = c(4, 9, 11, 13, 15, 31, 32)
for (x in 1:length(small_good)) {
  j = small_good[x]
  min = min(t_data[, j])
  for (i in 1:64) {
    p_data[i, j] = min/(t_data[i, j] + 1)
  }
}

# Converts points data to data frame. For some reason the conversion wants to
# turn all numbers into strings, hence the loop to convert them back to numeric.
p_data = as.data.frame(p_data, stringsAsFactors = FALSE)
for (x in 2:length(p_data)) {
  p_data[, x] = as.numeric(p_data[, x])
}

# Gives appropriate column names to data frame.
colnames(p_data) = c('school', 'gp', 'wins', 'loss', 'wlp', 'srs', 'sos', 'conf_wins',
                     'conf_loss', 'home_wins', 'home_loss', 'away_wins', 'away_loss',
                     'points_for', 'points_against', 'min_played', 'fg', 'fga', 'fgp', 
                     'thrp', 'thrpa', 'thrpp', 'ft', 'fta', 'ftp', 'orb', 'trb', 'ast', 'stl', 
                     'blk', 'tov', 'pf', 'twop', 'twopa', 'twopp')

# Find the index of the categories to be saved in category choices. 
cnames = c('school', 'gp', 'wins', 'loss', 'wlp', 'srs', 'sos', 'conf_wins',
           'conf_loss', 'home_wins', 'home_loss', 'away_wins', 'away_loss',
           'points_for', 'points_against', 'min_played', 'fg', 'fga', 'fgp', 
           'thrp', 'thrpa', 'thrpp', 'ft', 'fta', 'ftp', 'orb', 'trb', 'ast', 'stl', 
           'blk', 'tov', 'pf', 'twop', 'twopa', 'twopp')
save_cols = matrix(NA, nrow = 1, ncol = length(cat_choose) + 1)
save_cols[1] = which(cnames == 'school')
for (x in 1:length(cat_choose)) {
  name = cat_choose[x]
  save_cols[x + 1] = which(cnames == name)
}

# Creates final matrix with only the chosen columns.
fin_data = matrix(NA, nrow = 64, ncol = length(save_cols))
for (x in 1:length(save_cols)) {
  fin_data[, x] = p_data[, save_cols[x]]
}

# Converts the matrix to a data frame and applies the desired weights as
# specified in the beginning.
fin_data = as.data.frame(fin_data, stringsAsFactors = FALSE)
for (x in 2:length(fin_data)) {
  fin_data[, x] = as.numeric(fin_data[, x])
  fin_data[, x] = fin_data[, x]*cat_weight[x - 1]
}

# Sums desired columns for total score and gives names to the data frame
# columns.
fin_data = transform(fin_data, totalscore = rowSums(fin_data[, 2:length(fin_data)]))
colnames(fin_data) = c('school', cat_choose, 'totalscore')

bracket = matrix(NA, nrow = 64, ncol = 7)
game_sim = function(team1, team2) {
  if (fin_data[which(fin_data$school == team1),length(fin_data)] > fin_data[which(fin_data$school == team2), length(fin_data)]) {
    winner = team1
  }
  else {
    winner = team2 
  }
  return(winner)
}

bracket[,1] = fin_data[,1]

# Round 1 simulation.
nums = c(0, 16, 32, 48)
for (y in 1:4) {
  z = nums[y]
  for (x in 1:8) {
    bracket[x + (z / 2), 2] = game_sim(bracket[x + z,1], bracket[(17 + z) - x, 1])
  }
}

# Round 2 simulation.
nums = c(0, 8, 16, 24)
for (y in 1:4) {
  z = nums[y]
  for (x in 1:4) {
    bracket[x + (z / 2), 3] = game_sim(bracket[x + z, 2], bracket[(9 + z) - x, 2])
  }
}


# Sweet 16
nums = c(0, 4, 8, 12)
for (y in 1:4) {
  z = nums[y]
  for (x in 1:2) {
    bracket[x + (z / 2), 4] = game_sim(bracket[x + z, 3], bracket[(5 + z) - x, 3])
  }
}

# Elite 8
bracket[1, 5] = game_sim(bracket[1, 4], bracket[2, 4])
bracket[2, 5] = game_sim(bracket[3, 4], bracket[4, 4])
bracket[3, 5] = game_sim(bracket[5, 4], bracket[6, 4])
bracket[4, 5] = game_sim(bracket[7, 4], bracket[8, 4])

# Final 4
bracket[1, 6] = game_sim(bracket[1, 5], bracket[3, 5])
bracket[2, 6] = game_sim(bracket[2, 5], bracket[4, 5])

# Championship
bracket[1, 7] = game_sim(bracket[1, 6], bracket[2, 6])
