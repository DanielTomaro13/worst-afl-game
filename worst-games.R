library(dplyr)
library(fitzRoy)
library(zoo)
# Data Extends back to 1897

fetch_all_years_footywire <- function(start_year = 1897, end_year = 2024) {
  all_data <- list()
  
  for (year in start_year:end_year) {
    cat("Fetching data for year:", year, "\n")
      tryCatch({
      year_data <- fetch_player_stats_footywire(year)
      year_data$Season <- year
      all_data[[as.character(year)]] <- year_data
      Sys.sleep(2)
    }, error = function(e) {
      cat("Error fetching data for year", year, ": ", e$message, "\n")
    })
  }
  if (length(all_data) > 0) {
    all_years_data <- do.call(rbind, all_data)
    return(all_years_data)
  } else {
    stop("No data was successfully fetched for any year.")
  }
}
footywire <- fetch_all_years_footywire()


# First collect all player stats as we discussed earlier
footywire <- fetch_all_years_footywire(1897)

# Lowest fantasy/SuperCoach points (modern era)
worst_fantasy <- footywire %>%
  filter(!is.na(SC) %>% 
  arrange(SC) %>%
  head(20)  
  
# Least possessions (disposals)
worst_disposals <- footywire %>%
  filter(!is.na(D) & D >= 0) %>%
  arrange(D) %>%
  head(20)

# Worst efficiency

# Composite measure (low stats + high errors)
worst_composite <- footywire %>%
  mutate(
    badness_score = (D * -1) + (TO * 2) + (CL * 2)
  ) %>%
  arrange(desc(badness_score)) %>%
  head(20)

# Players who played most of the game but had minimal impact
worst_invisible <- footywire %>%
  filter(TOG > 75) %>%  
  arrange(D) %>% 
  head(20)


player_seasons <- footywire %>%
  arrange(Player, Season, Round) %>%
  group_by(Player, Season) %>%
  mutate(
    game_number = row_number(),
    days_since_last = as.numeric(difftime(Date, lag(Date), units = "days"))
  ) %>%
  filter(n() >= 5)

worst_stretches <- player_seasons %>%
  mutate(
    disposals_5game = rollsum(D, 5, align = "right", fill = NA),
    kicks_5game = rollsum(K, 5, align = "right", fill = NA),
    handballs_5game = rollsum(H, 5, align = "right", fill = NA),
    goals_5game = rollsum(G, 5, align = "right", fill = NA),
    behinds_5game = rollsum(B, 5, align = "right", fill = NA),
    tackles_5game = rollsum(T, 5, align = "right", fill = NA),
    
# Fewest total disposals over 5 games
worst_disposal_stretches <- worst_stretches %>%
  ungroup() %>%
  arrange(disposals_5game) %>%
  head(20)

# Lowest fantasy/SuperCoach points 
worst_fantasy_stretches <- worst_stretches %>%
  mutate(
    fantasy_5game = rollsum(SC, 5, align = "right", fill = NA)
  ) %>%
  ungroup() %>%
  arrange(fantasy_5game) %>%
  filter(!is.na(fantasy_5game)) %>%
  head(20)

