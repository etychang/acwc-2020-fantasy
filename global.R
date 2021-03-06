library(dplyr)
library(tidyr)
library(readr)

# Fictional selections
# df_selections <- tibble(Name = c("Tom", "Amy"),
#                         Player1 = c("Alice 2500", "Alice 2500"),
#                         Player2 = c("Bob 100", "Alice 2500"),
#                         TeamName = c("TeamA", "TeamB")
#                         )
# 
# players <- tibble(Player = c("Alice", "Bob"),
#                   Cost = c(2500, 100)) %>% 
#   mutate(Key = paste(Player, Cost))

# Load players
players <- read_csv("2020acwc_participant_names.csv") %>% 
  rename(Key = Name_Cost, Player = Name)


#Load selections
# df_selections <- read_csv("2020 AC World Championship - Fantasy Competition (Responses) - Form responses 1.csv") %>% 
df_selections <- readxl::read_xlsx("2020 AC World Championship - Fantasy Competition (Responses).xlsx") %>% 
  select(-Timestamp) %>% 
  rename(Player1 = `Select player 1`,
         Player2 = `Select player 2`,
         Player3 = `Select player 3`,
         Player4 = `Select player 4`,
         Player5 = `Select player 5`,
         Player6 = `Select player 6`,
         TeamName = `Team Name`,
         Affiliation = `Affiliation (how did you find out about this)`
         )

# Load full list from Chris Clarke db    
df_selections1 <- read_csv("AC_Fantasy_2020_Scoring.csv") %>% 
  gather(key, val, -Name) %>% 
  left_join(players,
            by = c("val" = "ShortName")) %>% 
  select(Name, key, Key) %>% 
  spread(key, Key) %>% 
  mutate(TeamName = Name,
         Affiliation = "Croquet community")


# results <- tibble(ID = 1:4,
#                   Winner = c("Reg Bamford", "Ben Rothman", "Mark Avery", "Reg Bamford"),
#                   Loser = c("Jose Riva", "Paddy Chapman", "Simon Hockey", "Simon Hockey"),
#                   Event = c("Block", "Z", "KO", "Bowl"),
#                   Peeling = c(NA, "qp", "tp", "sxp"),
#                   Comment = c("Bowl final", "Final", NA, NA))

results <- read_csv("results_2020_02_22.csv")

results <- results %>% 
  filter(!((str_detect(Winner, "Aiken Hakes") | str_detect(Loser, "Aiken Hakes")) & Event == "Block"))

calc_player_special <- function(){
  out <- results %>% 
    select(Winner, Loser, Comment) %>% 
    gather(key, Player, -Comment) %>% 
    filter(!is.na(Comment)) %>% 
    mutate(Points = case_when(
      Comment %in% c("Bowl final", "Plate final") & key == "Winner" ~ 25L,
      Comment %in% c("Bowl final", "Plate final") & key == "Loser" ~ 15L,
      Comment == "Shield final" & key == "Winner" ~ 15L,
      Comment == "QF" & key == "Loser" ~ 20L,
      Comment == "SF" & key == "Loser" ~ 30L,
      Comment == "Final" & key == "Loser" ~ 40L,
      Comment == "Final" & key == "Winner" ~ 50L,
      TRUE ~ 0L
    )
    )
  return(out)
}

calc_player_wins <- function(){
  out <- results %>% 
    rename(Player = Winner) %>% 
    count(Player, Event) %>% 
    mutate(Points = ifelse(Event %in% c("Block", "KO"),
                           10L * n,
                           5L * n))
  return(out)
}
calc_player_peels <- function(){
  out <- results %>% 
    rename(Player = Winner) %>% 
    count(Player, Peeling) %>% 
    mutate(Points = case_when(
      Peeling == "sxp" ~ 10L * n,
      Peeling %in% c("tp", "qp", "qnp", "tpo", "qpo", "qnpo") ~ 3L * n,
      TRUE ~ 0L)
    ) %>% 
      rbind(
        results %>% 
          rename(Player = Loser) %>% 
          count(Player, Peeling) %>% 
          mutate(Points = case_when(
            Peeling == "osxp" ~ 10L * n,
            Peeling %in% c("otp", "oqp", "oqnp") ~ 3L * n,
            TRUE ~ 0L)
          )
      )
  return(out)
} 
calc_player_points <- function(){
  pts_peeling <- calc_player_peels() %>% 
    group_by(Player) %>% 
    summarise(Type = "Peels",
              n = n(),
              Points = sum(Points))
  pts_wins <- calc_player_wins() %>% 
    group_by(Player) %>% 
    summarise(Type = "Wins",
              n = n(),
              Points = sum(Points))
  pts_special <- calc_player_special() %>% 
    group_by(Player) %>% 
    summarise(Type = "Special",
              n = n(),
              Points = sum(Points))
  out <- pts_wins %>% 
    rbind(pts_peeling) %>% 
    rbind(pts_special) %>% 
    group_by(Player) %>% 
    summarise(Points = sum(Points))
  return(out)
}

df_player_points <- players %>% 
  left_join(calc_player_points(),
            by = "Player") %>% 
  # mutate(Points = ifelse(Player == "Aiken Hakes", 0, Points)) %>% # options if just setting Aiken's points to 0, not used.
  arrange(desc(Cost))

df_teamvalues <- df_selections %>% 
  rbind(df_selections1) %>% 
  gather(Selected, Key, -Name, -TeamName) %>% 
  left_join(players, by = "Key") %>% 
  group_by(Name) %>% 
  summarise(TeamCost = sum(Cost, na.rm = T))

# df_teamvalues1 <- df_selections1 %>% 
#   gather(Selected, Key, -Name) %>% 
#   left_join(players, by = c("Key")) %>% 
#   group_by(Name) %>% 
#   summarise(TeamCost = sum(Cost, na.rm = T))
# 
tbl_selections <- df_selections %>% 
  rbind(df_selections1) %>%
  left_join(df_teamvalues) %>%
  select(Name, everything()) %>% 
  mutate(TeamCost = as.integer(TeamCost))

tbl_playerscores <- df_player_points %>% 
  left_join(results %>% 
              group_by(Winner) %>% 
              summarise(Wins = n(), 
                        Peels = sum(!is.na(Peeling))
                        ),
            by = c("Player" = "Winner")) %>% 
  select(-Key)
  

tbl_leaderboard <- df_selections %>% 
  rbind(df_selections1) %>% 
  # gather(Player, Key, -Name) %>% 
  gather(Player, Key, -Name, -TeamName) %>%
  left_join(df_player_points,
            by = "Key") %>% 
  # group_by(Name) %>% 
  group_by(Name, TeamName) %>%
  summarise(TeamCost = sum(Cost, na.rm = T),
            Points = sum(Points, na.rm = T)
            ) %>%
  ungroup() %>% 
  left_join(df_selections %>% 
              rbind(df_selections1) %>% 
              select(Name, Affiliation)
            ) %>% 
  arrange(desc(Points)) %>% 
  mutate(Rank = row_number(desc(Points))) %>% 
  select(Rank, everything())

tbl_scoring <- tribble(
  ~Type, ~Points,
  "Block/main KO win", 10,
  "Other game win", 5,
  "Peels - tp/qp/qnp", 3,
  "Peels - sxp", 10,
  "Bowl runner-up", 15,
  "Bowl winner", 25,
  "Plate runner-up", 15,
  "Plate winner", 25,
  "Shield winner", 15,
  "Quarter-finalist", 20,
  "Semi-finalist", 30,
  "Runner-up", 40,
  "Winner", 50
) %>% 
  mutate(Points = as.integer(Points))

