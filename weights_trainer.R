# Load in data set generated by Data_Masterfile.R
load("~/R/Madness-Analytics/2016_cleaned.RData")

# Redundancy to turn off in code RStudio warnings.
all_data_df = all_data_df

# Generate the names of the teams in each bracket. In order 1-16 seed.
midwest = c('Virginia', 'Michigan State', 'Utah', 'Iowa State', 'Purdue', "Seton Hall", 
            'Dayton', 'Texas Tech', 'Butler', 'Syracuse', 
            'Gonzaga', 'Arkansas-Little Rock', 'Iona', 'Fresno State', 'Middle Tennessee', 
            'Hampton')
east = c('North Carolina', 'Xavier', 'West Virginia', 'Kentucky', 'Indiana', 'Notre Dame', 
         'Wisconsin', 'Southern California', 'Providence', 'Pittsburgh', 'Michigan', 
         'Chattanooga', 'Stony Brook', 'Stephen F. Austin', 'Weber State', 'Florida Gulf Coast')
west = c('Oregon', 'Oklahoma', 'Texas A&M', 'Duke', 'Baylor', 'Texas', 'Oregon State', 
         "Saint Joseph's", 'Cincinnati', 'Virginia Commonwealth', 'Northern Iowa', 'Yale', 
         'North Carolina-Wilmington', 'Green Bay', 'Cal State Bakersfield', 'Holy Cross')
south = c('Kansas', 'Villanova', 'Miami (FL)', 'University of California', 'Maryland', 'Arizona', 
          'Iowa', 'Colorado', 'Connecticut', 'Temple', 'Wichita State', 'South Dakota State', 'Hawaii', 
          'Buffalo', 'North Carolina-Asheville', 'Austin Peay')

# Combine bracket teams in order, 1-4 seeds as 1 seeds.
teams = c(south, east, midwest, west)

# Choose which rows to save from large, original data set.
save_row = matrix(NA, nrow = 1, ncol = 64)
for (x in 1:length(teams)) {
  save_row[x] = which(all_data_df$school == paste(teams[x],'*'))
}

# Keep data from only desired teams/rows.
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
    p_data[i, j] = min/t_data[i, j]
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
                     'blk', 'tov', 'pf','twop', 'twopa', 'twopp')

# Find the index of the categories to be saved in category choices. 
cnames = c('school', 'gp', 'wins', 'loss', 'wlp', 'srs', 'sos', 'conf_wins',
           'conf_loss', 'home_wins', 'home_loss', 'away_wins', 'away_loss',
           'points_for', 'points_against', 'min_played', 'fg', 'fga', 'fgp', 
           'thrp', 'thrpa', 'thrpp', 'ft', 'fta', 'ftp', 'orb', 'trb', 'ast', 'stl', 
           'blk', 'tov', 'pf', 'twop', 'twopa', 'twopp')

# Create bracket of last year's actual results 
actual_bracket = matrix(NA, nrow = 64, ncol = 7)
actual_bracket[,1] = p_data[,1]
r1winners = c('Kansas *', 'Villanova *','Miami (FL) *', 'Hawaii *', 'Maryland *', 'Wichita State *','Iowa *', 'Connecticut *',
               'North Carolina *', 'Xavier *', 'Stephen F. Austin *', 'Kentucky *', 'Indiana *', 
              'Notre Dame *', 'Wisconsin *', 'Providence *', 'Virginia *', 'Middle Tennessee *', 
              'Utah *', 'Iowa State *', 'Arkansas-Little Rock *', 'Gonzaga *', 'Syracuse *', 'Butler *', 
              'Oregon *', 'Oklahoma *', 'Texas A&M *', 'Duke *', 'Yale *', 'Northern Iowa *',
              'Virginia Commonwealth *', "Saint Joseph's *")
actual_bracket[,2] = r1winners
r2winners = c('Kansas *', 'Villanova *', 'Miami (FL) *', 'Maryland *', 'North Carolina *', 'Wisconsin *', 
              'Notre Dame *', 'Indiana *', 'Virginia *', 'Syracuse *', 'Gonzaga *', 'Iowa State *', 
              'Oregon *', 'Oklahoma *', 'Texas A&M *', 'Duke *')
actual_bracket[,3] = r2winners
r3winners = c('Kansas *', 'Villanova *', 'North Carolina *', 'Notre Dame *', 'Virginia *', 'Syracuse *', 
              'Oregon *', 'Oklahoma *')
actual_bracket[,4] = r3winners
r4winners = c('Villanova *', 'North Carolina *', 'Syracuse *', 'Oklahoma *')
actual_bracket[,5] = r4winners
r5winners = c('Villanova *', 'North Carolina *')
actual_bracket[,6] = r5winners
r6winners = c('Villanova *')
actual_bracket[,7] = r6winners


# Input for desired analyzed categories and weights.
cat_choose = c('loss', 'wlp', 'srs', 'sos',
               'away_wins', 'away_loss',
               'points_against','fgp',
               'thrp', 'thrpp', 'ft','ftp','trb', 'ast',
               'tov', 'pf', 'twopp')
cat_weight = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
final_cat_weight = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
score_best = 1550
scores = matrix(NA, nrow = 2, ncol = 10000)

for (loop in 1:10000) {


    change_weights = sample(1:17, 3)
    weights_vals = runif(3, min = -1, max = 1)
    permutation = matrix(0, nrow = 1, ncol = 17)
    permutation[change_weights] = weights_vals
    
    cat_weight = cat_weight + permutation
    
    
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
    
    
    r1 = sum(bracket[,2] == actual_bracket[,2], na.rm = TRUE)
    r2 = sum(bracket[,3] == actual_bracket[,3], na.rm = TRUE)
    r3 = sum(bracket[,4] == actual_bracket[,4], na.rm = TRUE)
    r4 = sum(bracket[,5] == actual_bracket[,5], na.rm = TRUE)
    r5 = sum(bracket[,6] == actual_bracket[,6], na.rm = TRUE)
    r6 = sum(bracket[,7] == actual_bracket[,7], na.rm = TRUE)
    
    final_score = 10*r1 + 20*r2 + 40*r3 + 80*r4 + 160*r5 + 320*r6
    
    if (final_score > score_best) {
      final_cat_weight = cat_weight
      score_best = final_score
    } else {
      cat_weight = cat_weight - permutation
    }

    scores[loop] = score_best

}

print(final_cat_weight)
print(scores[loop])


