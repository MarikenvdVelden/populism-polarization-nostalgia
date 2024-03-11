dl <- dl |> 
  mutate(nostalgia = factor(nostalgia),
         scapegoat = factor(scapegoat))

d_in <- dl |> 
  filter(group_type == "In-group")
d_out <- dl |> 
  filter(group_type == "Out-group")

h1a <- broom.mixed::tidy(lmer(sentiment ~  nostalgia*sex + factor(urbanisation) +
                                (1 | groups), data= d_in)) |> 
  mutate(var = "Nostalgic Message",
         hyp = "In-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  filter(term %in% c("nostalgia1", "sexMale", "nostalgia1:sexMale")) |> 
  dplyr::select(term, var, hyp, estimate, upper, lower)
h1b <- broom.mixed::tidy(lmer(sentiment ~  scapegoat*sex + factor(urbanisation) +
                                (1 | groups), data= d_in)) |> 
  mutate(var = "Scapegoating Message",
         hyp = "In-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  filter(term %in% c("scapegoat1", "sexMale", "scapegoat1:sexMale")) |> 
  dplyr::select(term, var, hyp, estimate, upper, lower)

h2a <- broom.mixed::tidy(lmer(sentiment ~  nostalgia*sex + factor(urbanisation) +
                                (1 | groups), data= d_out)) |> 
  mutate(var = "Nostalgic Message",
         hyp = "Out-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  filter(term %in% c("nostalgia1", "sexMale", "nostalgia1:sexMale")) |> 
  dplyr::select(term, var, hyp, estimate, upper, lower)
h2b <- broom.mixed::tidy(lmer(sentiment ~  scapegoat*sex + factor(urbanisation) +
                                (1 | groups), data= d_out)) |> 
  mutate(var = "Scapegoating Message",
         hyp = "Out-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  filter(term %in% c("scapegoat1", "sexMale", "scapegoat1:sexMale")) |> 
  dplyr::select(term, var, hyp, estimate, upper, lower)

phe_gen <- h1a |> 
  add_case(h1b) |> 
  add_case(h2a) |> 
  add_case(h2b) |> 
  mutate(term = recode(term,
                       `nostalgia1` = "Nostalgic Message",
                       `sexMale` = "Male",
                       `scapegoat1` = "Scapegoating Message",
                       `nostalgia1:sexMale` = "Nostalgic Message * Male",
                       `scapegoat1:sexMale` = "Scapegoating Message * Male"),
         var = factor(var,
                      levels = c("Nostalgic Message",
                                 "Scapegoating Message")),
         term = factor(term,
                       levels = c("Scapegoating Message * Male",
                                  "Nostalgic Message * Male",
                                  "Male",
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
