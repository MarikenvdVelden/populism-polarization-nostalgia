pd1 <- d %>% 
  dplyr::select(id, H19_1:H19_8, H19_10:H19_11, A2b, E2_2) |> 
  filter(A2b != "Blanco") |> 
  pivot_longer(cols = H19_1:H19_11) |> 
  mutate(A2b = recode(A2b,
                      `Lid Omtzigt` = "New Social Contract",
                      `PvdA` = "GroenLinks/PvdA",
                      `GL` = "GroenLinks/PvdA",
                      `BBB` = "Farmers' Party",
                      `PvdD` = "Animal Rights Party",
                      `CU` = "Christian Union",
                      `BVNL` = "Corporate NL",
                      `FvD` = "Forum for Democracy"),
         E2_2 = recode(E2_2,
                       `very conservative` = 1,
                       `consersative` = 2,
                       `center` = 3,
                       `progressive` = 4,
                       `very progressive` = 5),
         name = recode(name,
                       `H19_1` = "Multinationals",
                       `H19_2` = "Neo-Liberal Politicians",
                       `H19_3` = "Right-Wing Voters",
                       `H19_4` = "Left-Wing Voters",
                       `H19_5` = "Materialistic People",
                       `H19_6` = "Climate Activists",
                       `H19_7` = "Immigrants",
                       `H19_8` = "Living on Benefits",
                      # `H19_9` = "Government",
                       `H19_10` = "Muslims",
                       `H19_11` = "Christians"),
         group = recode(name,
                           `Multinationals` = "In-group: Conservative Voters",
                           `Neo-Liberal Politicians` = "In-group: Conservative Voters",
                           `Right-Wing Voters` = "In-group: Conservative Voters",
                           `Left-Wing Voters` = "In-group: Progressive Voters",
                           `Materialistic People` = "In-group: Conservative Voters",
                           `Climate Activists` = "In-group: Progressive Voters",
                           `Immigrants` = "In-group: Progressive Voters",
                           `Living on Benefits` = "In-group: Progressive Voters",
                       #    `Government` = "In-group: Progressive Voters",
                           `Muslims` = "In-group: Progressive Voters",
                           `Christians` = "In-group: Conservative Voters"),
         party = factor(A2b,
                        levels = c("Bij1","Animal Rights Party", "Volt",
                                   "GroenLinks/PvdA", "D66", "Denk",
                                   "SP", "Christian Union", "50Plus",
                                   "New Social Contract", "CDA",
                                   "VVD", "SGP", "Farmers' Party",
                                   "PVV", "JA21", "Corporate NL",
                                   "Forum for Democracy"),
                        labels = c("Bij1","Animal Rights Party", "Volt",
                                   "GroenLinks/PvdA", "D66", "Denk",
                                   "SP", "Christian Union", "50Plus",
                                   "New Social Contract", "CDA",
                                   "VVD", "SGP", "Farmers' Party",
                                   "PVV", "JA21", "Corporate NL",
                                   "Forum for Democracy"))) 

pd1a <- pd1 |> 
  filter(group == "In-group: Conservative Voters") |> 
  ggplot(aes(x = party, y = value, colour = name)) +
  stat_summary(aes(colour = name), fun.data = "mean_cl_boot", 
               geom = "pointrange", position = position_dodge(.5)) +
  labs(x = "", y = "") +
  facet_grid(.~group) +
  geom_hline(yintercept = 5, linetype = "dashed",
             color = "lightgray", size = .5) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(col = guide_legend(nrow=2))

pd1b <- pd1 |> 
  filter(group != "In-group: Conservative Voters") |> 
  ggplot(aes(x = party, y = value, colour = name)) +
  stat_summary(aes(colour = name), fun.data = "mean_cl_boot", 
               geom = "pointrange", position = position_dodge(.5)) +
  labs(x = "", y = "Affective Sentiment for Ideological Social Groups \n (1 = very negative, 10 = very positive)") +
  facet_grid(.~group) +
  geom_hline(yintercept = 5, linetype = "dashed",
             color = "lightgray", size = .5) +  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(col = guide_legend(nrow=2))

pd2b <- pd1 |> 
  ggplot(aes(x = party, y = E2_2, colour = name)) +
 stat_summary(fun.data = "mean_cl_boot", geom = "pointrange",
               colour = fig_cols[9]) +
  labs(x = "", y = "Ideological Selfplacement \n (1 = very conservative, 5 = very progressive)") +
  geom_hline(yintercept = 3, linetype = "dashed",
             color = "lightgray", size = .5) +  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "none",
        legend.title = element_blank()) +
  guides(col = guide_legend(nrow=2))
  
tmp <- d1 |> 
  mutate(pop_att = round((F1_1 + F1_2 + F1_3 + F1_4 + F1_5 + F1_6 + F1_7 + F1_8)/8,0),
         pop_att2 = recode(pop_att,
                           `1` = "No populist attitudes",
                           `2` = "No populist attitudes",
                           `3` = "Neutral",
                           `4` = "Populist attitudes",
                           `5` = "Populist attitudes"),
         pop_att2 = factor(pop_att2,
                           levels = c("No populist attitudes",
                                      "Neutral", "Populist attitudes"))) |> 
  dplyr::select(id, pop_att, pop_att2) |> 
  drop_na(pop_att)

pd2 <- left_join(pd1, tmp)

pd3a <- pd2 |> 
  filter(group == "In-group: Conservative Voters",
         pop_att2 != "<NA>") |> 
  ggplot(aes(x = pop_att2, y = value, colour = name)) +
  stat_summary(aes(colour = name), fun.data = "mean_cl_boot", 
               geom = "pointrange", position = position_dodge(.5)) +
  labs(x = "", y = "") +
  facet_grid(.~group) +
  geom_hline(yintercept = 5, linetype = "dashed",
             color = "lightgray", size = .5) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(col = guide_legend(nrow=2))

pd3b <- pd2 |> 
  filter(group != "In-group: Conservative Voters",
         pop_att2 != "<NA>") |> 
  ggplot(aes(x = pop_att2, y = value, colour = name)) +
  stat_summary(aes(colour = name), fun.data = "mean_cl_boot", 
               geom = "pointrange", position = position_dodge(.5)) +
  labs(x = "", y = "Affective Sentiment for Ideological Social Groups \n (1 = very negative, 10 = very positive)") +
  facet_grid(.~group) +
  geom_hline(yintercept = 5, linetype = "dashed",
             color = "lightgray", size = .5) +  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(col = guide_legend(nrow=2))

pd4a <- pd1 |> 
  filter(group == "In-group: Conservative Voters") |> 
  mutate(E2_2 = recode(E2_2,
                       `1` = "Very Conservative",
                       `2` = "Conservative",
                       `3` = "Center",
                       `4` = "Progressive",
                       `5` = "Very Progressive"),
         E2_2 = factor(E2_2,
                       levels = c("Very Conservative", "Conservative",
                                  "Center", "Progressive",
                                  "Very Progressive"))) |> 
  ggplot(aes(x = E2_2, y = value, colour = name)) +
  stat_summary(aes(colour = name), fun.data = "mean_cl_boot", 
               geom = "pointrange", position = position_dodge(.5)) +
  labs(x = "", y = "") +
  facet_grid(.~group) +
  geom_hline(yintercept = 5, linetype = "dashed",
             color = "lightgray", size = .5) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(col = guide_legend(nrow=2))

pd4b <- pd1 |> 
  filter(group != "In-group: Conservative Voters") |> 
  mutate(E2_2 = recode(E2_2,
                       `1` = "Very Conservative",
                       `2` = "Conservative",
                       `3` = "Center",
                       `4` = "Progressive",
                       `5` = "Very Progressive"),
         E2_2 = factor(E2_2,
                       levels = c("Very Conservative", "Conservative",
                                  "Center", "Progressive",
                                  "Very Progressive"))) |> 
  ggplot(aes(x = E2_2, y = value, colour = name)) +
  stat_summary(aes(colour = name), fun.data = "mean_cl_boot", 
               geom = "pointrange", position = position_dodge(.5)) +
  labs(x = "", y = "Affective Sentiment for Ideological Social Groups \n (1 = very negative, 10 = very positive)") +
  facet_grid(.~group) +
  geom_hline(yintercept = 5, linetype = "dashed",
             color = "lightgray", size = .5) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(col = guide_legend(nrow=2))
