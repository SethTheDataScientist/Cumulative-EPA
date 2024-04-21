

PlayoffEPAQB <- Playoff_Tracker %>% 
  mutate(season = as.double(
                      case_when(Season == 99 ~ paste0(19,Season),
                                Season < 10 ~ paste0(200,Season),
                                T ~ paste0(20,Season))
                      ),
         Result = case_when(Result == "SuperBowl Winner" ~ "Superbowl Winner",
                            Result == "SuperBowl Loser" ~ "Superbowl Loser",
                             T ~ Result)) %>% 
  mutate(team = case_when(team == "HST" ~ "HOU",
                               team == "CLV" ~ "CLE",
                               team == "SD" ~ "LAC",
                               team == "BLT" ~ "BAL",
                               team == "OAK" ~ "LV",
                               team == "ARZ" ~ "ARI",
                               team == "SL" ~ "LA",
                                team == "LAR" ~ "LA",
                          team == "LAX" ~ "JAX",
                               team == "WSH" ~ "WAS",
                          team == "JET" ~ "NYJ",
                               T ~ team))  %>% 
  left_join(QBCumulativeEPA, by = c("season", "team" = "posteam")) %>% 
  group_by(season, team) %>% 
  arrange(desc(Count)) %>% 
  slice_head(n = 1) %>% 
  filter(!is.na(LastEPA), season > 2006) %>% 
  mutate(alpha = case_when(rank <= 10 & raterank <= 10 ~ 1,
                           T ~ 0.25))


ggplot(PlayoffEPAQB, aes(x = season, y = LastEPA))+
  geom_point(aes(color = Color, alpha = alpha)) +
  scale_color_identity(aesthetics = c("color")) +
  labs(
    y= "",
    x= "",
    title= "",
    caption = "@SethDataScience"
  ) +
  theme_reach() +
  facet_wrap(~Result)


QBEPA <- QBCumulativeEPA %>% 
  filter(!is.na(LastEPA),
         QBname == "P.Manning" & season != 2015 | QBname != "P.Manning",
         QBname == "T.Brady" & season != 2002 | QBname != "T.Brady",
         QBname == "D.Carr" & posteam != "HOU" | QBname != "D.Carr") %>% 
  group_by(QBname) %>% 
  summarise(Count = n(),
            LastSeason = max(season),
            TotalEPA = sum(LastEPA),
            MeanEPAT = mean(LastEPA),
            Rank = round(mean(rank),2),
            MedianRank = median(rank),
            BestRank = min(rank),
            WorstRank = max(rank),
            Value = (1/((Rank + MedianRank + BestRank *2 + WorstRank * 2)/6))) %>% 
  filter(Count > 1, BestRank <= 25, LastSeason >= 2012) %>% 
  group_by() %>% 
  mutate(MeanEPATPR = percent_rank(MeanEPAT),
         ValuePR = percent_rank(Value),
         Value2 = ValuePR * MeanEPATPR)
