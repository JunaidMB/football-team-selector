library(googlesheets)
library(dplyr)

# Read most recent prospective player sheet

# player.list <- read_csv('prospective_players_sheet.csv')

ppl <- gs_url('https://docs.google.com/spreadsheets/d/1RI5k3lTVfHCUmegUT9YvdDnH14HHRw82vivqp5egU5k/edit#gid=0', verbose = TRUE)                     

player.list <- data.frame(gs_read(ss = ppl, ws = "Sheet1", range = 'A1:D150'))

df <- player.list %>% filter(Playing == 'Y') %>% dplyr::select(-Playing)

# Load historical logs of teams

## Read historical list CSV

# historical.teams <- read_csv('historical_teams.csv')

ht <- gs_url('https://docs.google.com/spreadsheets/d/19HIPklKbaYv9i4g2bol0tiHlS0CTkHCfKY601H-Y3mw/edit#gid=316725927', verbose = TRUE)

historical.teams <- data.frame(gs_read(ss = ht, ws = "historical_teams"))

## Extract last week's teams

last.week.team.a <- filter(historical.teams, team_type == "a" & date == max(date))
last.week.team.b <- filter(historical.teams, team_type == "b" & date == max(date))

# Assign latest week teams

repeat {
  
  team.a <- df %>% dplyr::sample_frac(0.5)
  team.b <- dplyr::anti_join(df, team.a, by = 'player.ids')

  if ((abs(mean(team.a$scores) - mean(team.b$scores)) <= 2 & length(dplyr::setdiff(team.a$player.ids, last.week.team.a$player.ids)) <= 3) == TRUE) break
}

# Take current week teams to upload into historical teams

team.a.append <- data.frame(team.a, team_type = 'a', date = Sys.Date())
team.b.append <- data.frame(team.b, team_type = 'b', date = Sys.Date())

current.week.append <- dplyr::union_all(team.b.append, team.a.append) %>% arrange(team_type)


# Append onto historical log

if (max(historical.teams$date) == max(current.week.append$date)) {
  historical.teams.new <- dplyr::union(current.week.append, filter(historical.teams, date != max(historical.teams$date)) ) %>% arrange(date, team_type)
} else {
  historical.teams.new <- dplyr::union(current.week.append, historical.teams) %>% arrange(date, team_type)
}

# Write CSV of historical team

write.csv(historical.teams.new, file = "/Users/junaidbutt/developer/football_team_selector/csv_files/historical_teams.csv", row.names = FALSE)

# Upload historical team to GS
gs_upload(file = "/Users/junaidbutt/developer/football_team_selector/csv_files/historical_teams.csv", sheet_title = "historical_teams", verbose = TRUE, overwrite = TRUE)



