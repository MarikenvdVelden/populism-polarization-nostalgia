#H1
#The left ideological in-groups (and right ideological out-groups) are: 
#left-wing people, Muslims, people who get state subsidies, climate activists;
#The right ideological in-groups (and left ideological out-groups) are: 
#right-wing people, neoliberal politicians, materialistic people, and big companies.
# for pooled groups

dl <- dl |> 
  mutate(nostalgia = factor(nostalgia),
         scapegoat = factor(scapegoat))
d_in <- dl |> 
  filter(group_type == "In-group")
d_out <- dl |> 
  filter(group_type == "Out-group")
h1a <- broom.mixed::tidy(lmer(sentiment ~  nostalgia + factor(urbanisation) +
                                (1 | groups), data= d_in)) |> 
  mutate(var = "Nostalgic Message",
         hyp = "In-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  filter(term=="nostalgia1") |> 
  dplyr::select(var, hyp, estimate, upper, lower)
h1b <- broom.mixed::tidy(lmer(sentiment ~  scapegoat + factor(urbanisation) +
                                (1 | groups), data= d_in)) |> 
  mutate(var = "Scapegoating Message",
         hyp = "In-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  filter(term=="scapegoat1") |> 
  dplyr::select(var, hyp, estimate, upper, lower)

h2a <- broom.mixed::tidy(lmer(sentiment ~  nostalgia + factor(urbanisation) +
                                (1 | groups), data= d_out)) |> 
  mutate(var = "Nostalgic Message",
         hyp = "Out-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  filter(term=="nostalgia1") |> 
  dplyr::select(var, hyp, estimate, upper, lower)
h2b <- broom.mixed::tidy(lmer(sentiment ~  scapegoat + factor(urbanisation) +
                                (1 | groups), data= d_out)) |> 
  mutate(var = "Scapegoating Message",
         hyp = "Out-groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  filter(term=="scapegoat1") |> 
  dplyr::select(var, hyp, estimate, upper, lower)

p1_h1 <- h1a |> 
  add_case(h1b) |> 
  add_case(h2a) |> 
  add_case(h2b) |> 
  mutate(var = factor(var,
                      levels = c("Scapegoating Message",
                                 "Nostalgic Message"))) |> 
  ggplot(aes(y = var, x = estimate,
             xmin = lower, xmax = upper,
             color = hyp)) +
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

# For each group
d_left <- d |> 
  filter(ideology==0)
d_right <- d |>  
  filter(ideology==1)

h1 <- tidy(lm(H19_1 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

tmp <- tidy(lm(H19_1 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_2 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_2 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_3 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_3 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_4 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_4 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_5 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_5 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_6 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_6 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_7 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_7 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_8 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Living on Benefits",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_8 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Living on Benefits",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)


h1 <- h1 |> 
  add_case(tmp)

#tmp <- tidy(lm(H19_9 ~  treatment + factor(urbanisation), d_left)) |> 
#  mutate(y = "Goverment",
#         var = "Affective Sentiment for Ideological Social In-Groups",
#         id = "Progressive Voters",
#         hyp = "a",
#         upper = (estimate + (std.error * 1.56)),
#         lower = (estimate - (std.error * 1.56))) |> 
#  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

#h1 <- h1 |> 
#  add_case(tmp)

#tmp <- tidy(lm(H19_9 ~  treatment + factor(urbanisation), d_right)) |> 
#  mutate(y = "Goverment",
#         var = "Affective Sentiment for Ideological Social Out-Groups",
#         id = "Conservative Voters",
#         hyp = "b",
#         upper = (estimate + (std.error * 1.56)),
#         lower = (estimate - (std.error * 1.56))) |> 
#  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

#h1 <- h1 |> 
#  add_case(tmp)

tmp <- tidy(lm(H19_10 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_10 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_11 ~  treatment + factor(urbanisation), d_right)) |> 
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_11 ~  treatment + factor(urbanisation), d_left)) |> 
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |>  
  add_case(tmp) |> 
  mutate(term = recode(term,
                       `treatmentNo Nostalgia, Scapegoat` = "No Nostalgia, Scapegoat",
                       `treatmentNostalgia, No Scapegoat` = "Nostalgia, No Scapegoat",
                       `treatmentNostalgia, Scapegoat` = "Nostalgia, Scapegoat"))

p1a_l <- h1 |> 
  filter(hyp == "a", id == "Progressive Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(y = y, x = estimate,
             xmin = lower, xmax = upper,
             color = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~id, scales = "free") +
  labs(y = "", x = "") + 
  #x = "Predicted Effect for Support for Social In-Groups",
  #caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1a_r <- h1 |> 
  filter(hyp == "a", id == "Conservative Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(y = y, x = estimate,
             xmin = lower, xmax = upper,
             color = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~id, scales = "free") +
  labs(y = "", 
       x = "Predicted Effect for Affective Sentiment for Social In-Groups",
       caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariate Urbanization") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1b_l <- h1 |> 
  filter(hyp == "b", id == "Progressive Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(y = y, x = estimate,
             xmin = lower, xmax = upper,
             color = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~id, scales = "free") +
  labs(y = "", x = "") + 
  #x = "Predicted Effect for Support for Social Out-Groups",
  #caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1b_r <- h1 |> 
  filter(hyp == "b", id == "Conservative Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |>   ggplot(aes(y = y, x = estimate,
             xmin = lower, xmax = upper,
             color = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~id, scales = "free") +
  labs(y = "", 
       x = "Predicted Effect for Affective Sentiment for Social Out-Groups",
       caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariate Urbanization") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))
