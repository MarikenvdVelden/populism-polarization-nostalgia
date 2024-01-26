## NL Data
covs <- d |> 
  mutate(treatment = recode(treatment,
                            `No Nostalgia, No Scapegoat` = 1,
                            `No Nostalgia, Scapegoat` = 2,
                            `Nostalgia, No Scapegoat` = 3,
                            `Nostalgia, Scapegoat` = 4)) |> 
  dplyr::select(treatment, age:urbanisation, E2_2, A1, A2b) |> 
    drop_na()

balanced <-cobalt::bal.tab(treatment ~ age + factor(sex) + factor(ethnicity) + 
                     factor(education) + factor(region) + factor(urbanisation) + E2_2 + 
                     factor(A1) + factor(A2b),
                   data = covs,
                   thresholds = c(m = 0.05))[[1]] 

bp <- balanced %>%
  mutate(variable = c("Age", "Male",
                       "Ethnicity: Dutch", 
                       "Ethnicity: Non-Western Migrant", 
                       "Ethnicity: Western Migrant", 
                       "High Levels of Education",
                       "Low Levels of Education", "Medium Levels of Education",
                       "Region: East", "Region: North",
                       "Region: South", 
                       "Region: Big Cities","Region: West",  
                       "Urbanization: Low", "Urbanization: Medium",
                       "Urbanization: No",  "Urbanization: Strong", 
                       "Urbanization: Very Strong",
                       "Ideology: Center", "Ideology: Conservative",
                       "Ideology: Pogressive", "Ideology: Very Conservative",
                       "Ideology: Very Progressive",
                       "Voted: Don't Know", "Voted: No",
                       "Voted: Not eligle", "Voted: Yes",
                       "50+","BBB","Bij1", 
                       "Blanco", "BVNL", "CDA",
                       "ChristenUnie", "D66","Denk", 
                       "Forum for Democracy","GroenLinks", "JA21", 
                      "NSC", "PvdA",  "Animal Rights Party", "PVV",
                      "SGP", "SP", "VOLT","VVD")) %>%
  mutate(variable = factor(variable,
                           levels = c("Age", "Male",
                                      "Ethnicity: Dutch", 
                                      "Ethnicity: Non-Western Migrant", 
                                      "Ethnicity: Western Migrant", 
                                      "High Levels of Education",
                                      "Low Levels of Education", "Medium Levels of Education",
                                      "Region: East", "Region: North",
                                      "Region: South", 
                                      "Region: Big Cities","Region: West",  
                                      "Urbanization: Low", "Urbanization: Medium",
                                      "Urbanization: No",  "Urbanization: Strong", 
                                      "Urbanization: Very Strong",
                                      "Ideology: Center", "Ideology: Conservative",
                                      "Ideology: Pogressive", "Ideology: Very Conservative",
                                      "Ideology: Very Progressive",
                                      "Voted: Don't Know", "Voted: No",
                                      "Voted: Not eligle", "Voted: Yes",
                                      "50+","BBB","Bij1", 
                                      "Blanco", "BVNL", "CDA",
                                      "ChristenUnie", "D66","Denk", 
                                      "Forum for Democracy","GroenLinks", "JA21", 
                                      "NSC", "PvdA",  "Animal Rights Party", "PVV",
                                      "SGP", "SP", "VOLT","VVD")),
         difference = Corr.Un,2) %>%
  dplyr::select(variable, difference) %>%
  drop_na() %>%
  mutate(type = if_else(difference <= -.05, "below",
                        if_else(difference >= .05, "below", "above"))) |> 
  ggplot(aes(x = variable, y = difference, color = type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("above"=fig_cols[3],
                                "below"=fig_cols[2])) +
  
  labs(x="", y= "Standardized Mean Differences") +
  coord_flip() +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_hline(yintercept = -0.05, linetype = "dashed")
