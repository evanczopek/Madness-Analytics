# Evan Czopek - 1/29/2017 6:00pm

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



