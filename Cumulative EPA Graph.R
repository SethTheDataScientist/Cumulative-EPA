
# Season Cumulative EPA ---------------------------------------------------


nfl_colorsUnique <- nfl_colors %>% 
  filter(team_code != "CLE*") %>% 
  arrange(primary) %>% 
  mutate(Test = case_when(is.na(lag(primary, 1)) == 1 ~ "#000001",
                          T ~ lag(primary, 1))) %>% 
  arrange(secondary) %>% 
  mutate(Test2 = case_when(is.na(lag(secondary, 1)) == 1 ~ "#000001",
                          T ~ lag(secondary, 1)),
         Color = case_when(primary != Test ~ primary,
                           secondary != Test2 ~ secondary,
                           T ~ "#002245"))



oppadjtest <- pbpCurrent %>% 
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down)) %>%
  mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                       epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1))) %>% 
  group_by(season, defteam, week) %>% 
  summarise(Pos = posteam,
            Def = defteam,
            Count = NROW(defteam),
            PassEPA = mean(if_else(pass == 1, epa, 0), na.rm = T),
            RushEPA = mean(if_else(rush == 1, epa, 0), na.rm = T),
            EPA = mean(epa, na.rm = T)) %>% 
  distinct() %>% 
  group_by(season, defteam) %>% 
  summarise(Week = week,
            Pos = Pos,
            Def = Def,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            PosteamPassEPA = mean(PassEPA),
            PosteamRushEPA = mean(RushEPA),
            PosteamEPA = mean(EPA)) %>% 
  group_by(season,Def) %>% 
  summarise(Week = Week,
            Pos = Pos,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            PosteamPassEPA = PosteamPassEPA,
            PosteamRushEPA = PosteamRushEPA,
            PosteamEPA = PosteamEPA,
            DefteamPassEPA = mean(PassEPA),
            DefteamRushEPA = mean(RushEPA),
            DefteamEPA = mean(EPA)) %>% 
  group_by(season) %>% 
  summarise(Week = Week,
            Pos = Pos,
            Def = Def,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            PosteamPassEPA = PosteamPassEPA,
            PosteamRushEPA = PosteamRushEPA,
            PosteamEPA = PosteamEPA,
            DefteamPassEPA = DefteamPassEPA,
            DefteamRushEPA = DefteamRushEPA,
            DefteamEPA = DefteamEPA,
            LGPassEPA = mean(PassEPA),
            LGRushEPA = mean(RushEPA),
            LGEPA = mean(EPA),
            PassADJ = DefteamPassEPA - LGPassEPA,
            RushADJ = DefteamRushEPA - LGRushEPA,
            EPAADJ = DefteamEPA - LGEPA,
            FinPassEPA = PassEPA - PassADJ,
            FinRushEPA = RushEPA - RushADJ,
            FinEPA = EPA - EPAADJ) %>% 
  distinct() %>% 
  select(season, Week, Pos, Def, PassADJ, RushADJ)



CumulativeEPA <- pbpCurrent %>%
  filter(play == 1,
         !is.na(epa), !is.na(down),
         season >= 2011) %>% 
  arrange(game_id) %>% 
  #left_join(oppadjtest, by = c("season", "week" = "Week", "defteam" = "Def")) %>% 
  left_join(nfl_colorsUnique, by = c("posteam" = "team_code")) %>% 
  mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                       epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)),
         epa = case_when(is.na(epa) == 1 ~ 0,
                         T ~ epa),
         # EPA = case_when(pass == 1 ~ epa - PassADJ, 
         #                   rush == 1 ~ epa - RushADJ,
         #                 T ~ epa)
         ) %>%
  group_by(season, posteam) %>%
  filter(!is.na(EPA)) %>% 
  summarise(Week = week,
            game_id = game_id,
            playNumber = seq(1, n(), 1),
            MaxPlayNumber = n(),
            EPA = EPA,
         CumEPA = cumsum(EPA),
         LastEPA = tail(CumEPA, 1),
         Color = head(Color, 1),
         url = head(url, 1)) %>% 
  group_by(season, posteam) %>% 
  slice_tail(n = 1)





CumulativeEPA1 <- pbpCurrent %>%
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down)) %>% 
  arrange(game_id) %>% 
  #left_join(oppadjtest, by = c("season", "week" = "Week", "defteam" = "Def")) %>% 
  left_join(nfl_colorsUnique, by = c("posteam" = "team_code")) %>% 
  # mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
  #                      epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)),
  #        EPA = case_when(pass == 1 ~ epa - PassADJ,
  #                        rush == 1 ~ epa - RushADJ,
  #                        T ~ epa)) %>%
  mutate(EPA = epa) %>% 
  group_by(posteam) %>%
  summarise(season = season,
            Week = week,
            EPA = EPA,
            game_id = game_id,
            Color = head(Color, 1),
            url = head(url, 1)) 


  mutate(playNumber = seq(1, n(), 1),
          Season_Change = case_when(lag(season, n= 1) != season ~ playNumber + 0.5),
          MaxPlayNumber = n(),
          EPA = cumsum(EPA),
          LastEPA = tail(EPA, 1)) %>% 
  group_by(posteam) %>% 
  slice_head(n = 1)


ggplot(CumulativeEPA1 %>% 
         filter(posteam == "BUF" | posteam == "KC" | posteam == "MIA" | posteam == "CLE" | posteam == "PHI" ,
                season >= 2022) %>% 
         group_by(posteam) %>% 
         mutate(
           playNumber = seq(1, n(), 1),
           Season_Change = case_when(lag(season, n= 1) != season ~ playNumber + 0.5),
           MaxPlayNumber = n(),
           EPA = cumsum(EPA),
           LastEPA = tail(EPA, 1)),
       aes(x = playNumber, y = EPA))+
  geom_line(aes(color = Color))+
  geom_image(aes(image = url, x = MaxPlayNumber, y = LastEPA))+
  geom_hline(yintercept = 0)+
  geom_vline(aes(xintercept = Season_Change, color = Color), linetype = "dashed")+
  theme_reach()+
  scale_color_identity(aesthetics = "color")







QBCumulativeEPA <- pbpCurrent %>%
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down), posteam != "CLE*", defteam != "CLE*") %>% 
  arrange(game_id) %>% 
  left_join(oppadjtest, by = c("season", "week" = "Week", "defteam" = "Def")) %>% 
  left_join(nfl_colorsUnique, by = c("posteam" = "team_code")) %>% 
  mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                       epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)),
         epa = case_when(is.na(epa) == 1 ~ 0,
                         T ~ epa),
         EPA = case_when(pass == 1 ~ epa - PassADJ, 
                         rush == 1 ~ epa - RushADJ,
                         is.na(epa) == 1 ~ 0,
                         T ~ epa)) %>%
  group_by(season, QBname) %>%
  mutate(EPA = cumsum(EPA),
         EPA = case_when(is.na(EPA) == 1 ~ lag(EPA, 1),
                         T ~ EPA)) %>% 
  summarise(posteam = head(posteam, 1),
            Count = n(),
            LastEPA = tail(EPA, 1),
            EPArate = LastEPA/Count,
            Color = head(Color, 1),
            url = head(url, 1)) %>% 
  filter(Count >= 200) %>%
  group_by(season) %>% 
  arrange(desc(LastEPA)) %>% 
  mutate(rank = seq(1,n(),1)) %>%
  group_by(season) %>% 
  arrange(desc(EPArate)) %>% 
  mutate(raterank = seq(1,n(),1))



QBList <- QBCumulativeEPA %>% 
  group_by(QBname) %>% 
  summarise(Count = n(),
            lastSeason = max(season),
            MinRank = min(rank),
            MaxRank = max(rank),
            Mean = mean(rank),
            Median = median(rank),
            Avg = (Mean*2 + Median*3)/5) %>% 
  filter(Count >= 2, lastSeason >= 2015)

# QB EPA Graph ------------------------------------------------------------
choice <- c("T.Romo", "P.Rivers", "E.Manning", "B.Roethlisberger")



ggplot(QBCumulativeEPA %>%
         group_by() %>% 
         filter(between(season, 
                min(QBCumulativeEPA$season[QBCumulativeEPA$QBname %in% choice]),
                max(QBCumulativeEPA$season[QBCumulativeEPA$QBname %in% choice]))) %>% 
         mutate(Alpha = case_when(QBname %in% choice ~ 1,
                                  T ~ 0.6),
                Size = case_when(QBname %in% choice ~ 0.035,
                                 T ~ 0.025),
                Label = case_when(QBname %in% choice ~ rank),
                Count = percent_rank(Count)),
       aes(x = season, y = LastEPA, group = QBname))+
  geom_line(aes(color = Color, alpha = Alpha, size = Size))+
  geom_point(aes(color = Color, fill = Color, alpha = Alpha, size = Count))+
  geom_text(aes(label = Label, color = "white"))+
  geom_smooth(aes(x = season, y = LastEPA, group = ""), method = "lm")+
  scale_color_identity(aesthetics = c("color", "fill"))+
  scale_x_continuous(breaks = seq(1999,2030,1))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_reach()+
  labs(
    y= "Final Cumulative EPA",
    x= "Season",
    title= "QB Cumulative Opponent and WP Adjusted EPA",
    subtitle = "Regular and Postseason included, Number listed is Season Rank, Size is Proportional to # of Plays,
     (Total Offense Rushes, Passes, Penalties only for games with QB as Starter)",
    caption = "@SethDataScience"
  ) 


output$OffensePlot <- renderPlot({
  
  OEPA %>%
    filter(season == input$seasons,
           posteam %in% input$teams,
           Week %in% input$weeks) %>%  
    group_by(season, posteam) %>% 
    mutate(playNumber = seq(1, n(), 1),
           MaxPlayNumber = max(playNumber, na.rm = T),
           MaxEPA = tail(EPA, 1)) %>%
    ggplot(aes(x = playNumber, y = EPA))+
    geom_line(aes(color = Color))+
    geom_image(aes(image = url, x = MaxPlayNumber, y = MaxEPA))+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = max(OEPA$LastEPA[OEPA$season == input$seasons], na.rm = T),
               linetype = "dashed")+
    geom_hline(yintercept = min(OEPA$LastEPA[OEPA$season == input$seasons], na.rm = T),
               linetype = "dashed")+
    theme_reach()+
    scale_color_identity(aesthetics = "color")+
    labs(
      y= "Cumulative Opponent & WP Adjusted Offensive EPA",
      x= "Total Number of Plays",
      title= paste0(
        input$seasons,
        " Cumulative Opp/WP Adj Offensive EPA"),
      subtitle = "Dashed Lines Represent Min/Max Final EPA from Selected Season",
      caption = "Regular and Postseason (If applicable), @SethDataScience"
    ) 
})

output$DefensePlot <- renderPlot({
  
  
  DEPA %>%
    filter(season == input$seasons,
           defteam %in% input$teams,
           Week %in% input$weeks) %>% 
    group_by(season, defteam) %>% 
    mutate(playNumber = seq(1, n(), 1),
           MaxPlayNumber = max(playNumber, na.rm = T),
           MaxEPA = tail(EPA, 1)) %>% 
    ggplot(aes(x = playNumber, y = EPA))+
    geom_line(aes(color = Color))+
    geom_image(aes(image = url,  x = MaxPlayNumber, y = MaxEPA))+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = max(DEPA$LastEPA[DEPA$season == input$seasons], na.rm = T),
               linetype = "dashed")+
    geom_hline(yintercept = min(DEPA$LastEPA[DEPA$season == input$seasons], na.rm = T),
               linetype = "dashed")+
    theme_reach()+
    scale_color_identity(aesthetics = "color")+
    scale_y_reverse()+
    labs(
      y= "Cumulative Opponent & WP Adjusted Defensive EPA",
      x= "Total Number of Plays",
      title= paste0(
        input$seasons,
        " Cumulative Opp/WP Adj Defensive EPA"),
      subtitle = "Y-axis Flipped (Higher = Better), Dashed Lines Represent Min/Max Final EPA from Selected Season",
      caption = "Regular and Postseason (If applicable), @SethDataScience"
    ) 
})

# Defense Season Cumulative EPA ---------------------------------------------------



oppadjtest2 <- pbpCurrent %>% 
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down), posteam != "CLE*", defteam != "CLE*")  %>% 
  select(season, posteam, defteam, week, epa, wp, down, rush, pass, 
         shotgun, yards_gained, ydstogo, air_yards) %>% 
  mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                       epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1))) %>% 
  group_by(season, defteam, week) %>% 
  summarise(Pos = posteam,
            Count = NROW(defteam),
            PassEPA = mean(if_else(pass == 1, epa, 0), na.rm = T),
            RushEPA = mean(if_else(rush == 1, epa, 0), na.rm = T),
            EPA = mean(epa, na.rm = T)) %>% 
  distinct() %>% 
  group_by(season, defteam) %>% 
  summarise(Week = week,
            Pos = Pos,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            DefteamPassEPA = mean(PassEPA),
            DefteamRushEPA = mean(RushEPA),
            DefteamEPA = mean(EPA)) %>% 
  group_by(season, Pos) %>% 
  summarise(Week = Week,
            Def = defteam,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            DefteamPassEPA = DefteamPassEPA,
            DefteamRushEPA = DefteamRushEPA,
            DefteamEPA = DefteamEPA,
            PosteamPassEPA = mean(PassEPA),
            PosteamRushEPA = mean(RushEPA),
            PosteamEPA = mean(EPA)) %>% 
  group_by(season) %>% 
  summarise(Week = Week,
            Pos = Pos,
            Def = Def,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            PosteamPassEPA = PosteamPassEPA,
            PosteamRushEPA = PosteamRushEPA,
            PosteamEPA = PosteamEPA,
            DefteamPassEPA = DefteamPassEPA,
            DefteamRushEPA = DefteamRushEPA,
            DefteamEPA = DefteamEPA,
            LGPassEPA = mean(PassEPA),
            LGRushEPA = mean(RushEPA),
            LGEPA = mean(EPA),
            PassADJ = PosteamPassEPA - LGPassEPA,
            RushADJ = PosteamRushEPA - LGRushEPA,
            EPAADJ = PosteamEPA - LGEPA,
            FinPassEPA = PassEPA - PassADJ,
            FinRushEPA = RushEPA - RushADJ,
            FinEPA = EPA - EPAADJ)  %>%  
  distinct() %>% 
  select(season, Week, Pos, Def, PassADJ, RushADJ)


DefCumulativeEPA <- pbpCurrent %>%
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down), posteam != "CLE*", defteam != "CLE*",
         season >= 2011) %>% 
  arrange(game_id) %>% 
  left_join(oppadjtest2, by = c("season", "week" = "Week", "posteam" = "Pos")) %>% 
  left_join(nfl_colorsUnique, by = c("defteam" = "team_code")) %>% 
  left_join(nfl_logos, by = c("defteam" = "team_code")) %>% 
  mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                       epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1)),
         epa = case_when(is.na(epa) == 1 ~ 0,
                         T ~ epa),
         EPA = case_when(pass == 1 ~ epa - PassADJ, 
                         rush == 1 ~ epa - RushADJ,
                         T ~ epa)) %>% 
  group_by(season, defteam) %>% 
  summarise(Week = week,
            game_id = game_id,
            MaxPlayNumber = n(),
            EPA = cumsum(EPA),
            LastEPA = tail(EPA, 1),
            Color = head(Color, 1),
            url = head(url.y, 1)) 

ggplot(DefCumulativeEPA, aes(x = playNumber, y = EPA))+
  geom_line(aes(color = Color))+
  geom_image(aes(image = url, x = MaxPlayNumber, y = LastEPA))+
  geom_hline(yintercept = 0)+
  theme_reach()+
  scale_color_identity(aesthetics = "color")+
  scale_y_reverse()


# EPA by Quarter ----------------------------------------------------------



offQTRoppadjWP <- pbpCurrent %>% 
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down)) %>%
  mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                       epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1))) %>% 
  group_by(season, defteam, week, qtr) %>% 
  summarise(Pos = posteam,
            Def = defteam,
            Count = NROW(defteam),
            PassEPA = mean(if_else(pass == 1, epa, 0), na.rm = T),
            RushEPA = mean(if_else(rush == 1, epa, 0), na.rm = T),
            EPA = mean(epa, na.rm = T)) %>% 
  distinct() %>% 
  group_by(season, defteam) %>% 
  summarise(Week = week,
            qtr = qtr,
            Pos = Pos,
            Def = Def,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            PosteamPassEPA = mean(PassEPA),
            PosteamRushEPA = mean(RushEPA),
            PosteamEPA = mean(EPA)) %>% 
  group_by(season,Def) %>% 
  summarise(Week = Week,
            qtr = qtr,
            Pos = Pos,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            PosteamPassEPA = PosteamPassEPA,
            PosteamRushEPA = PosteamRushEPA,
            PosteamEPA = PosteamEPA,
            DefteamPassEPA = mean(PassEPA),
            DefteamRushEPA = mean(RushEPA),
            DefteamEPA = mean(EPA)) %>% 
  group_by(season) %>% 
  summarise(Week = Week,
            qtr = qtr,
            Pos = Pos,
            Def = Def,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            PosteamPassEPA = PosteamPassEPA,
            PosteamRushEPA = PosteamRushEPA,
            PosteamEPA = PosteamEPA,
            DefteamPassEPA = DefteamPassEPA,
            DefteamRushEPA = DefteamRushEPA,
            DefteamEPA = DefteamEPA,
            LGPassEPA = mean(PassEPA),
            LGRushEPA = mean(RushEPA),
            LGEPA = mean(EPA),
            PassADJ = DefteamPassEPA - LGPassEPA,
            RushADJ = DefteamRushEPA - LGRushEPA,
            EPAADJ = DefteamEPA - LGEPA,
            FinPassEPA = PassEPA - PassADJ,
            FinRushEPA = RushEPA - RushADJ,
            FinEPA = EPA - EPAADJ) %>% 
  distinct() %>% 
  group_by(season, Week, qtr, Pos) %>% 
  summarise(FinEPA = mean(FinEPA),
            FinPassEPA = mean(FinPassEPA),
            FinRushEPA = mean(FinRushEPA))


defQTRoppadjWP <- pbpCurrent %>% 
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down))  %>% 
  select(season, posteam, defteam, week, epa, wp, down, rush, pass, qtr,
         shotgun, yards_gained, ydstogo, air_yards) %>% 
  mutate(epa = if_else(pass == 1 , epa*(((wp-0.5)^2*-3)+1),
                       epa * ((wp - 0.5)*if_else(wp > 0.5, -1.5, 1.5)+1))) %>% 
  group_by(season, defteam, week, qtr) %>% 
  summarise(Pos = posteam,
            Count = NROW(defteam),
            PassEPA = mean(if_else(pass == 1, epa, 0), na.rm = T),
            RushEPA = mean(if_else(rush == 1, epa, 0), na.rm = T),
            EPA = mean(epa, na.rm = T)) %>% 
  distinct() %>% 
  group_by(season, defteam) %>% 
  summarise(Week = week,
            qtr = qtr,
            Pos = Pos,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            DefteamPassEPA = mean(PassEPA),
            DefteamRushEPA = mean(RushEPA),
            DefteamEPA = mean(EPA)) %>% 
  group_by(season, Pos) %>% 
  summarise(Week = Week,
            qtr = qtr,
            Def = defteam,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            DefteamPassEPA = DefteamPassEPA,
            DefteamRushEPA = DefteamRushEPA,
            DefteamEPA = DefteamEPA,
            PosteamPassEPA = mean(PassEPA),
            PosteamRushEPA = mean(RushEPA),
            PosteamEPA = mean(EPA)) %>% 
  group_by(season) %>% 
  summarise(Week = Week,
            qtr = qtr,
            Pos = Pos,
            Def = Def,
            PassEPA = PassEPA,
            RushEPA = RushEPA,
            EPA = EPA,
            PosteamPassEPA = PosteamPassEPA,
            PosteamRushEPA = PosteamRushEPA,
            PosteamEPA = PosteamEPA,
            DefteamPassEPA = DefteamPassEPA,
            DefteamRushEPA = DefteamRushEPA,
            DefteamEPA = DefteamEPA,
            LGPassEPA = mean(PassEPA),
            LGRushEPA = mean(RushEPA),
            LGEPA = mean(EPA),
            PassADJ = PosteamPassEPA - LGPassEPA,
            RushADJ = PosteamRushEPA - LGRushEPA,
            EPAADJ = PosteamEPA - LGEPA,
            FinPassEPA = PassEPA - PassADJ,
            FinRushEPA = RushEPA - RushADJ,
            FinEPA = EPA - EPAADJ)  %>%  
  distinct() %>% 
  group_by(season, Week, qtr, Def) %>% 
  summarise(FinEPA = mean(FinEPA),
            FinPassEPA = mean(FinPassEPA),
            FinRushEPA = mean(FinRushEPA))


offdefQTRoppadjWP <- offQTRoppadjWP %>% 
  left_join(defQTRoppadjWP, by =c ("Pos" = "Def", "Week", "season", "qtr")) %>% 
  group_by(season, Week, qtr, Pos) %>% 
  summarise(FinPassEPA.x = mean(FinPassEPA.x, na.rm = T),
            FinRushEPA.x = mean(FinRushEPA.x, na.rm = T),
            FinEPA.x = mean(FinEPA.x, na.rm = T),
            FinPassEPA.y = mean(FinPassEPA.y, na.rm = T),
            FinRushEPA.y = mean(FinRushEPA.y, na.rm = T),
            FinEPA.y = mean(FinEPA.y, na.rm = T),
            TotalEPA = FinEPA.x - FinEPA.y) %>% 
  arrange(desc(TotalEPA))


seasonQTRoffdefoppadjWP <- offoppadjWP %>% 
  left_join(defoppadjWP, by =c ("Pos" = "Def", "Week", "season")) %>% 
  group_by(season, Pos) %>% 
  summarise(FinPassEPA.x = mean(FinPassEPA.x, na.rm = T),
            FinRushEPA.x = mean(FinRushEPA.x, na.rm = T),
            FinEPA.x = mean(FinEPA.x, na.rm = T),
            FinPassEPA.y = mean(FinPassEPA.y, na.rm = T),
            FinRushEPA.y = mean(FinRushEPA.y, na.rm = T),
            FinEPA.y = mean(FinEPA.y, na.rm = T),
            TotalEPA = FinEPA.x - FinEPA.y) %>% 
  arrange(desc(TotalEPA))


TurnoverBattleQTR <-  pbpCurrent %>% 
  filter(epa != 0, season >= 2011) %>%  
  group_by(game_id, drive) %>% 
  arrange(play_id) %>% 
  arrange(game_id) %>% 
  mutate(TurnoverDrive = case_when(interception == 1 &
                                     (fumble_recovery_1_team == defteam |
                                        is.na(fumble_recovery_2_team) == 1 ) ~ 1,
                                   fumble_recovery_1_team == defteam | 
                                     (touchback == 1 & fumble_lost == 1) ~ 1,
                                   fumble_recovery_1_team == defteam &
                                     is.na(fumble_recovery_2_team) == 1 ~ 1,
                                   fumble_recovery_1_team == defteam &
                                     fumble_recovery_1_team == fumble_recovery_2_team ~ 1,
                                   fumble_recovery_1_team != defteam &
                                     defteam == fumble_recovery_2_team ~ 1,
                                   field_goal_result == "blocked" |
                                     field_goal_result == "missed" ~ 1,
                                   fourth_down_failed == 1 ~ 1,
                                   T ~ 0),
         RealTurnoverDrive = case_when(interception == 1 &
                                         (fumble_recovery_1_team == defteam |
                                            is.na(fumble_recovery_2_team) == 1 ) ~ 1,
                                       fumble_recovery_1_team == defteam | 
                                         (touchback == 1 & fumble_lost == 1) ~ 1,
                                       fumble_recovery_1_team == defteam &
                                         is.na(fumble_recovery_2_team) == 1 ~ 1,
                                       fumble_recovery_1_team == defteam &
                                         fumble_recovery_1_team == fumble_recovery_2_team ~ 1,
                                       fumble_recovery_1_team != defteam &
                                         defteam == fumble_recovery_2_team ~ 1,
                                       T ~ 0),
         DefensiveScore = case_when(interception == 1 & touchdown == 1 ~ 7,
                                    fumble_lost == 1 & return_touchdown == 1 ~ 7,
                                    safety == 1 ~ 2,
                                    return_touchdown == 1 & punt_attempt == 1 ~ 7,
                                    defensive_two_point_conv == "success" ~ 2)) %>% 
  group_by(game_id) %>% 
  mutate(DefScoreDrive = lag(DefensiveScore, n = 1),
         TurnoverDrive1 = case_when((posteam != lag(posteam, n = 1)) &
                                      (drive != lag(drive, n = 1)) ~ 
                                      lag(TurnoverDrive, n = 1),
                                    DefScoreDrive == 7 ~ 0,
                                    T ~ 0),
         RealTurnoverDrive1 = case_when((posteam != lag(posteam, n = 1)) &
                                          (drive != lag(drive, n = 1)) ~ 
                                          lag(RealTurnoverDrive, n = 1),
                                        DefScoreDrive == 7 ~ 0,
                                        T ~ 0)) %>%
  group_by(game_id, drive) %>% 
  mutate(TODriveLabel = case_when(sum(TurnoverDrive1) >= 1 ~ 1,
                                  T ~ 0),
         RealTODriveLabel = case_when(sum(RealTurnoverDrive1) >= 1 ~ 1,
                                      T ~ 0)) %>% 
  group_by() %>% 
  group_by(season, posteam, game_id, qtr)  %>% 
  summarise(QBname = head(QBname, 1),
            week = head(week, 1),
            TurnoverDrives = sum(TurnoverDrive1, na.rm = T),
            TODrive = sum(TurnoverDrive, na.rm = T),
            TD = sum(touchdown[TODriveLabel >= 1], na.rm = T)*7,
            FG = sum(if_else(field_goal_result == "made" & TODriveLabel >= 1,
                             1, 0), na.rm = T)*3,
            OTD = sum(touchdown[TODriveLabel < 1], na.rm = T)*7,
            OFG = sum(if_else(field_goal_result == "made" & TODriveLabel < 1,
                             1, 0), na.rm = T)*3,
            KickReturnTD = sum(return_touchdown == 1 & kickoff_attempt == 1, na.rm = T)*7,
            FGBlockTD = sum(touchdown == 1 & field_goal_attempt == 1, na.rm =T),
            DefScore = sum(DefScoreDrive, na.rm = T),
            NonOScore = (FG + TD + DefScore + KickReturnTD + FGBlockTD),
            OScore = OTD + OFG,
            FinalScore = tail(posteam_score_post, n = 1),
            OppScore = case_when(posteam == home_team ~ tail(away_score, 1),
                                 posteam == away_team ~ tail(home_score, 1)),
            PtsWithout = FinalScore - NonOScore,
            RealTurnoverDrives = sum(RealTurnoverDrive1, na.rm = T),
            RealTODrive = sum(RealTurnoverDrive, na.rm = T),
            RealTD = sum(touchdown[RealTODriveLabel >= 1], na.rm = T)*7,
            RealFG = sum(if_else(field_goal_result == "made" & RealTODriveLabel >= 1,
                                 1, 0), na.rm = T)*3,
            RealOTD = sum(touchdown[RealTODriveLabel < 1], na.rm = T)*7,
            RealOFG = sum(if_else(field_goal_result == "made" & RealTODriveLabel < 1,
                                 1, 0), na.rm = T)*3,
            RealNonOScore = (RealFG + RealTD + DefScore + KickReturnTD + FGBlockTD),
            RealOScore = RealOFG + RealOTD,
            RealPtsWithout = FinalScore - RealNonOScore,
            PosteamWin = head(PosteamWin, 1)) %>% 
  distinct() 


EPAbyQuarter <- pbpCurrent %>%
  filter(play == 1, epa != 0,
         !is.na(epa), !is.na(down), posteam != "CLE*", defteam != "CLE*",
         season >= 2011) %>% 
  group_by(season, week, qtr, posteam) %>% 
  summarise(epa = mean(epa, na.rm = T)) %>% 
  left_join(TurnoverBattleQTR, by = c("season", "posteam", "qtr", "week")) %>% 
  left_join(offdefQTRoppadjWP, by = c("season", "posteam" = "Pos", "qtr", "week" = "Week")) %>% 
  select(season, game_id, week, qtr, posteam, epa, FinPassEPA.x, FinRushEPA.x, FinEPA.x,
         TotalEPA, OScore, NonOScore, RealOScore, RealNonOScore, PosteamWin) %>% 
  group_by(season, qtr, posteam) %>% 
  summarise(epa = mean(epa),
         FinEPA.x = mean(FinEPA.x),
         OScore = mean(OScore)) %>% 
  group_by() %>% 
  mutate(epaPR = percent_rank(epa),
         FinEPAPR = percent_rank(FinEPA.x),
         ScorePR = percent_rank(OScore)) %>% 
  left_join(nfl_colors, by = c("posteam" = "team_code")) %>% 
  left_join(nfl_logos, by = c("posteam" = "team_code"))
  

ggplot(EPAbyQuarter %>% 
         filter(season == 2020, qtr != 5),
       aes(x = qtr, y = FinEPA.x))+
  geom_col(aes(color = secondary, fill = primary))+
  geom_image(aes(image = url))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0.1, color = "green")+
  scale_colour_identity(aesthetics = c("color", "fill")) +
  facet_wrap(~posteam)


