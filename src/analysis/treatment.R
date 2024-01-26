#The left ideological in-groups (and right ideological out-groups) are: 
#left-wing people, Muslims, people who get state subsidies, climate activists;
#The right ideological in-groups (and left ideological out-groups) are: 
#right-wing people, neoliberal politicians, materialistic people, and big companies.

d <- d |> 
  mutate(treatment = ifelse(scapegoat == 0 & nostalgia == 0, "No Nostalgia, No Scapegoat", NA),
         treatment = ifelse(scapegoat == 0 & nostalgia == 1, "Nostalgia, No Scapegoat", treatment),
         treatment = ifelse(scapegoat == 1 & nostalgia == 0, "No Nostalgia, Scapegoat", treatment),
         treatment = ifelse(scapegoat == 1 & nostalgia == 1, "Nostalgia, Scapegoat", treatment))
df <- d %>% 
  dplyr::select(H19_1:H19_8, H19_10:H19_11, treatment, ideology) |> 
  pivot_longer(cols = H19_1:H19_11) |> 
  mutate(name = recode(name,
                        `H19_1` = "Multinationals",
                        `H19_2` = "Neo-Liberal Politicians",
                        `H19_3` = "Right-Wing Voters",
                        `H19_4` = "Left-Wing Voters",
                        `H19_5` = "Materialistic People",
                        `H19_6` = "Climate Activists",
                        `H19_7` = "Immigrants",
                        `H19_8` = "Living on Benefits",
                        #`H19_9` = "Government",
                        `H19_10` = "Muslims",
                        `H19_11` = "Christians"),
         ingroup = name,
         outgroup = name,
         ingroup = recode(ingroup,
                          `Multinationals` = "Right-Wing",
                          `Neo-Liberal Politicians` = "Right-Wing",
                          `Right-Wing Voters` = "Right-Wing",
                          `Left-Wing Voters` = "Left-Wing",
                          `Materialistic People` = "Right-Wing",
                          `Climate Activists` = "Left-Wing",
                          `Immigrants` = "Left-Wing",
                          `Living on Benefits` = "Left-Wing",
                          #`Government` = "Left-Wing",
                          `Muslims` = "Left-Wing",
                          `Christians` = "Right-Wing"),
         outgroup = recode(outgroup,
                          `Multinationals` = "Left-Wing",
                          `Neo-Liberal Politicians` = "Left-Wing",
                          `Right-Wing Voters` = "Left-Wing",
                          `Left-Wing Voters` = "Right-Wing",
                          `Materialistic People` = "Left-Wing",
                          `Climate Activists` = "Right-Wing",
                          `Immigrants` = "Right-Wing",
                          `Living on Benefits` = "Right-Wing",
                          #`Government` = "Right-Wing",
                          `Muslims` = "Right-Wing",
                          `Christians` = "Left-Wing"),
         ideology = recode(ideology,
                           `0` = "Progressive Voters",
                           `1` = "Conservative Voters"
                           )) 
                        #`H19_12` = "Past",
                        #`H19_13` = "Future",
                        #`H19_14` = "Present"))
p1 <- df |> 
  filter(ingroup == "Left-Wing") |> 
  group_by(treatment, ideology) |> 
  ggplot(aes(x = treatment, y = value)) +
  see::geom_violinhalf(fill = fig_cols[9], alpha = .4,
                       color = "white") +
  stat_summary(aes(shape = ideology), fun.data = "mean_cl_boot", geom = "pointrange",
               colour = "gray55") +
  labs(x = "", y = "") +
  facet_grid(name~.) +
  #facet_wrap(vars(name), ncol = 1) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p2 <- df |> 
  filter(ingroup == "Right-Wing") |> 
  group_by(treatment, ideology) |> 
  ggplot(aes(x = treatment, y = value)) +
  see::geom_violinhalf(fill = fig_cols[9], alpha = .4,
                       color = "white") +
  stat_summary(aes(shape = ideology), fun.data = "mean_cl_boot", geom = "pointrange",
               colour = "gray55") +
  labs(x = "", y = "Affective Sentiment \n (1 = Very negative, 10 = Very positive") +
  facet_grid(name~.) +
  #facet_wrap(vars(name), ncol = 1) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
