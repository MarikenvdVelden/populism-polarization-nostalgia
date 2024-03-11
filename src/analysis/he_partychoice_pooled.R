dl <- dl |> 
  mutate(nostalgia = factor(nostalgia),
         scapegoat = factor(scapegoat),
         A2b = recode(A2b,
                             `Lid Omtzigt` = "New Social Contract",
                             `PvdA` = "GroenLinks/PvdA",
                             `GL` = "GroenLinks/PvdA",
                             `BBB` = "Farmers' Party",
                             `PVV` = "PVV",
                             `FvD` = "Forum for Democracy",
                             `VVD` = "VVD",
                             `CDA` = "CDA",
                             .default = "Other Party"),
                A2b = factor(A2b))

d_in <- dl |> 
  filter(group_type == "In-group")
d_out <- dl |> 
  filter(group_type == "Out-group")
d_in <- within(d_in, A2b <- relevel(A2b, ref = "Other Party"))
d_out <- within(d_out, A2b <- relevel(A2b, ref = "Other Party"))

h1a <- broom.mixed::tidy(lmer(sentiment ~  nostalgia*A2b + factor(urbanisation) +
                                (1 | groups), data= d_in)) |> 
  mutate(var = "Nostalgic Message",
         hyp = "In-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, var, hyp, estimate, upper, lower)
h1b <- broom.mixed::tidy(lmer(sentiment ~  scapegoat*A2b + factor(urbanisation) +
                                (1 | groups), data= d_in)) |> 
  mutate(var = "Scapegoating Message",
         hyp = "In-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, var, hyp, estimate, upper, lower)

h2a <- broom.mixed::tidy(lmer(sentiment ~  nostalgia*A2b + factor(urbanisation) +
                                (1 | groups), data= d_out)) |> 
  mutate(var = "Nostalgic Message",
         hyp = "Out-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, var, hyp, estimate, upper, lower)
h2b <- broom.mixed::tidy(lmer(sentiment ~  scapegoat*A2b + factor(urbanisation) +
                                (1 | groups), data= d_out)) |> 
  mutate(var = "Scapegoating Message",
         hyp = "Out-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, var, hyp, estimate, upper, lower)

phe_vc <- h1a |> 
  add_case(h1b) |> 
  add_case(h2a) |> 
  add_case(h2b) |> 
  mutate(term = recode(term,
                       `nostalgia1` = "Nostalgic Message",
                       `A2bFarmers' Party` = "Support for Farmers' Party",
                       `A2bCDA` = "Support for CDA",
                       `A2bForum for Democracy` = "Support for Forum for Democracy",
                       `A2bPVV` = "Support for PVV",
                       `A2bGroenLinks/PvdA` = "Support for GroenLinks/PvdA",
                       `A2bVVD` = "Support for VVD",
                       `A2bNew Social Contract` = "Support for New Social Contract",
                       `scapegoat1` = "Scapegoating Message",
                       `nostalgia1:A2bCDA` = "Nostalgic Message * Support for CDA",
                       `nostalgia1:A2bGroenLinks/PvdA` = "Nostalgic Message * Support for GroenLinks/PvdA",
                       `nostalgia1:A2bVVD` = "Nostalgic Message * Support for VVD",
                       `nostalgia1:A2bNew Social Contract` = "Nostalgic Message * Support for New Social Contract",
                       `nostalgia1:A2bPVV` = "Nostalgic Message * Support for PVV",
                       `nostalgia1:A2bFarmers' Party` = "Nostalgic Message * Support for Farmers' Party",
                       `nostalgia1:A2bForum for Democracy` = "Nostalgic Message * Support for Forum for Democracy",
                       `scapegoat1:A2bCDA` = "Scapegoating Message * Support for CDA",
                       `scapegoat1:A2bGroenLinks/PvdA` = "Scapegoating Message * Support for GroenLinks/PvdA",
                       `scapegoat1:A2bVVD` = "Scapegoating Message * Support for VVD",
                       `scapegoat1:A2bNew Social Contract` = "Scapegoating Message * Support for New Social Contract",
                       `scapegoat1:A2bPVV` = "Scapegoating Message * Support for PVV",
                       `scapegoat1:A2bFarmers' Party` = "Scapegoating Message * Support for Farmers' Party",
                       `scapegoat1:A2bForum for Democracy` = "Scapegoating Message * Support for Forum for Democracy",
                       .default = "tmp")) |> 
  filter(term != "tmp") |> 
  mutate(var = factor(var,
                      levels = c("Nostalgic Message",
                                 "Scapegoating Message")),
         term = factor(term,
                       levels = c("Scapegoating Message * Support for VVD",
                                  "Scapegoating Message * Support for PVV",
                                  "Scapegoating Message * Support for New Social Contract",
                                  "Scapegoating Message * Support for GroenLinks/PvdA",
                                  "Scapegoating Message * Support for Forum for Democracy",
                                  "Scapegoating Message * Support for Farmers' Party",
                                  "Scapegoating Message * Support for CDA",
                                  "Nostalgic Message * Support for VVD",
                                  "Nostalgic Message * Support for PVV",
                                  "Nostalgic Message * Support for New Social Contract",
                                  "Nostalgic Message * Support for GroenLinks/PvdA",
                                  "Nostalgic Message * Support for Forum for Democracy",
                                  "Nostalgic Message * Support for Farmers' Party",
                                  "Nostalgic Message * Support for CDA",
                                  "Support for VVD",
                                  "Support for PVV",
                                  "Support for New Social Contract",
                                  "Support for GroenLinks/PvdA",
                                  "Support for Forum for Democracy",
                                  "Support for Farmers' Party",
                                  "Support for CDA",
                                  "Scapegoating Message",
                                  "Nostalgic Message"))) |> 
  ggplot(aes(y = term, x = estimate,
             xmin = lower, xmax = upper,
             color = var)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~hyp, scales = "free") +
  labs(y = "", 
       x = "Predicted Effect for Affective Sentiment for Social In-Groups",
       caption = "Results are based on a multilevel model, with respondents clustered in social groups. \n Analyses are controlled for unbalanced covariate Urbanization.") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))





