percent_rank(EarlyPass) * percent_rank(EarlyPassEPA) *
  percent_rank(EarlyDown)
WR
OL
QB
Playmaker Vs structure

Rolling_GamesData <- Rolling_Games %>% 
  group_by(season, player) %>% 
  mutate(SeasonSnaps = sum(Snaps)) %>% 
  slice_head(n = 1)

WARDataSideOfBall1 <- WARDataSideOfBall %>% 
  group_by(season, team_name) %>% 
  summarise(DBWAR = sum(WAR[SideofBall == "DB"]),
            DLWAR = sum(WAR[SideofBall == "DL"]),
            LBWAR = sum(WAR[SideofBall == "LB"]),
            OLWAR = sum(WAR[SideofBall == "OL"]),
            SkillWAR = sum(WAR[SideofBall == "Skill"]),
            QBWAR = sum(WAR[SideofBall == "QB"]))

CumulativeEPAFill <- CumulativeEPA %>% 
  group_by(season, posteam) %>% 
  summarise(LastEPA = head(LastEPA, 1))

 Derive Cumulative EPA
 
 CumulativeEPAData <- EarlyDown %>% 
   full_join(WARDataSideOfBall1, by = c("season", "posteam" = "team_name")) %>% 
   full_join(Rolling_GamesData, by = c("season", "posteam" = "team_name")) %>% 
   filter(!is.na(player)) %>% 
   group_by(season, posteam) %>% 
   arrange(desc(SeasonSnaps)) %>% 
   slice_head(n = 1) %>% 
   left_join(CumulativeEPAFill, by = c("season", "posteam")) %>% 
   mutate(ExpectedLastEPA = -237.4 - 41.75*EarlyPass + 299.79 *EarlyPassEPA +
            425.14 * EarlyDown + 11.04 * SkillWAR + 16.17 * OLWAR +
            30.03 * AvgFloor + 21.28 * AvgCeiling,
          Diff = LastEPA - ExpectedLastEPA)%>% 
   select(season, posteam, player, OC, HC, LastEPA, ExpectedLastEPA, Diff,
          EarlyPass, EarlyPassEPA, EarlyDown, SkillWAR, OLWAR,
          AvgFloor, AvgCeiling)
 
 model <- lm(formula = LastEPA ~ EarlyPass + EarlyPassEPA + EarlyDown +
               SkillWAR + OLWAR + AvgFloor + AvgCeiling,
             data = CumulativeEPAData)
 
 
 CumulativeEPAProjection <- EarlyDown %>% 
   full_join(WARDataSideOfBall1, by = c("season", "posteam" = "team_name")) %>% 
   full_join(Rolling_GamesData, by = c("season", "posteam" = "team_name")) %>% 
   filter(!is.na(player)) %>% 
   group_by(season, posteam) %>% 
   arrange(desc(SeasonSnaps)) %>% 
   slice_head(n = 1) %>% 
   left_join(CumulativeEPAFill, by = c("season", "posteam")) %>% 
   filter(season >= 2020)  %>% 
   mutate(SkillWAR = case_when(posteam == "KC" ~ SkillWAR - 0.61,
                               posteam == "MIA" ~ SkillWAR + 0.61,
                               posteam == "GB" ~ SkillWAR - 0.81,
                               posteam == "LV" ~ SkillWAR + 0.81,
                               T ~ SkillWAR),
          OLWAR = case_when(posteam == "CIN" ~ OLWAR + 0.33,
                            T ~ OLWAR),
          AvgFloor = case_when(posteam == "CLE" ~ 0.60,
                               posteam == "SEA" ~ 0.25,
                               posteam == "DEN" ~ 0.88,
                               posteam == "IND" ~ 0.81,
                               posteam == "WAS" ~  0.29,
                               posteam == "ATL" ~ 0.53,
                               posteam == "PIT" ~ 0.17,
                               T ~ AvgFloor),
          AvgCeiling = case_when(posteam == "CLE" ~ 0.92,
                                 posteam == "SEA" ~ 0.52,
                                 posteam == "DEN" ~ 0.97,
                                 posteam == "IND" ~ 0.53,
                                 posteam == "WAS" ~ 0.74,
                                 posteam == "ATL" ~ 0.78,
                                 posteam == "PIT" ~ 0.76,
                                 T ~ AvgCeiling)) %>% 
   group_by(posteam) %>% 
   mutate(EarlyPass = mean(EarlyPass, na.rm = T),
          EarlyPassEPA = mean(EarlyPassEPA, na.rm = T),
          EarlyDown = mean(EarlyDown, na.rm = T),
          SkillWAR = mean(SkillWAR, na.rm = T),
          OLWAR = mean(OLWAR, na.rm = T),
          AvgFloor = mean(AvgFloor, na.rm = T),
          AvgCeiling = mean(AvgCeiling, na.rm = T)) %>% 
   mutate(ExpectedLastEPA = -237.4 - 41.75*EarlyPass + 299.79 *EarlyPassEPA +
            425.14 * EarlyDown + 11.04 * SkillWAR + 16.17 * OLWAR +
            30.03 * AvgFloor + 21.28 * AvgCeiling,
          Diff = LastEPA - ExpectedLastEPA)%>% 
   select(season, posteam, player, OC, HC, LastEPA, ExpectedLastEPA, Diff,
          EarlyPass, EarlyPassEPA, EarlyDown, SkillWAR, OLWAR,
          AvgFloor, AvgCeiling) %>% 
   group_by(posteam) %>% 
   slice_head(n = 1)
 