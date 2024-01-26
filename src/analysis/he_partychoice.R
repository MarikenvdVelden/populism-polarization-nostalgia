## HE Party Choice
d <- d |> 
  mutate(A2b = recode(A2b,
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

d_left <- d |> 
  filter(ideology==0)
d_right <- d |>  
  filter(ideology==1)

d_left <- within(d_left, A2b <- relevel(A2b, ref = "Other Party"))
d_right <- within(d_right, A2b <- relevel(A2b, ref = "Other Party"))

tmp <- lm(H19_1 ~  treatment*A2b + factor(urbanisation), d_right)

he_i <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list(A2b = c("CDA", "Farmers' Party",
                                               "Forum for Democracy",
                                               "GroenLinks/PvdA", "New Social Contract",
                                               "PVV","VVD")))) |> 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

tmp <- lm(H19_1 ~  treatment*A2b + factor(urbanisation), d_left)
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list(A2b = c("CDA", "Farmers' Party",
                                               "Forum for Democracy",
                                               "GroenLinks/PvdA", "New Social Contract",
                                               "PVV","VVD")))) |> 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_2 ~  treatment*A2b + factor(urbanisation), d_right)
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |> 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_2 ~  treatment*A2b + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |> 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_3 ~  treatment*A2b + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_3 ~  treatment*A2b+ factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_4 ~  treatment*A2b+ factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_4 ~  treatment*A2b+ factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_5 ~  treatment*A2b+ factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_5 ~  treatment*A2b+ factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_6 ~  treatment*A2b+ factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_6 ~  treatment*A2b+ factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_7 ~  treatment*A2b+ factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_7 ~  treatment*A2b+ factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_8 ~  treatment*A2b+ factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Living on Benefits",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_8 ~  treatment*A2b+ factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Living on Benefits",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)


he_i <- he_i |> 
  add_case(tmp)

#tmp <- lm(H19_9 ~  treatment*A2b+ factor(urbanisation), d_left) 
#tmp <- tidy(margins::margins(tmp, variables = "treatment",
#                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
#  mutate(y = "Goverment",
#         var = "Affective Sentiment for Ideological Social In-Groups",
#         id = "Progressive Voters",
#         hyp = "a",
#         upper = (estimate + (std.error * 1.56)),
#         lower = (estimate - (std.error * 1.56))) |> 
#  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

#he_i <- he_i |> 
#  add_case(tmp)

#tmp <- lm(H19_9 ~  treatment*A2b+ factor(urbanisation), d_right) 
#tmp <- tidy(margins::margins(tmp, variables = "treatment",
#                            at = list(A2b = c("CDA", "Farmers' Party",     
#                                              "Forum for Democracy",      
#                                              "GroenLinks/PvdA", "New Social Contract", 
#                                              "PVV","VVD")))) |>
#  mutate(y = "Goverment",
#         var = "Affective Sentiment for Ideological Social Out-Groups",
#         id = "Conservative Voters",
#         hyp = "b",
#         upper = (estimate + (std.error * 1.56)),
#         lower = (estimate - (std.error * 1.56))) |> 
#  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

#he_i <- he_i |> 
#  add_case(tmp)

tmp <- lm(H19_10 ~  treatment*A2b+ factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_10 ~  treatment*A2b+ factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_11 ~  treatment*A2b+ factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_11 ~  treatment*A2b+ factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                            at = list(A2b = c("CDA", "Farmers' Party",                                                "Forum for Democracy",                                                "GroenLinks/PvdA", "New Social Contract",                                                "PVV","VVD")))) |>
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |>  
  add_case(tmp) |> 
  mutate(term = recode(term,
                       `treatmentNo Nostalgia, Scapegoat` = "No Nostalgia, Scapegoat",
                       `treatmentNostalgia, No Scapegoat` = "Nostalgia, No Scapegoat",
                       `treatmentNostalgia, Scapegoat` = "Nostalgia, Scapegoat"))

p1e_l <- he_i |> 
  filter(hyp == "a", id == "Progressive Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(x = estimate, y = `at.value`,
             xmin = lower, xmax = upper,
             color = term, fill = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(y~id, scales = "free") +
  labs(y = "", x = "") + 
  #x = "Predicted Effect for Support for Social In-Groups",
  #caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1e_r <- he_i |> 
  filter(hyp == "a", id == "Conservative Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(x = estimate, y = `at.value`,
             xmin = lower, xmax = upper,
             color = term, fill = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(y~id, scales = "free") +
  labs(y = "", 
       x = "Marginal Effect of Support for Social In-Groups",
       caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariate Urbanization") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p2e_l <- he_i |> 
  filter(hyp == "b", id == "Progressive Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(x = estimate, y = `at.value`,
             xmin = lower, xmax = upper,
             color = term, fill = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(y~id, scales = "free") +
  labs(y = "", x = "") + 
  #x = "Predicted Effect for Support for Social In-Groups",
  #caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p2e_r <- he_i |> 
  filter(hyp == "b", id == "Conservative Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(x = estimate, y = `at.value`,
             xmin = lower, xmax = upper,
             color = term, fill = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(y~id, scales = "free") +
  labs(y = "", 
       x = "Marginal Effect of Support for Social Out-Groups",
       caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariate Urbanization") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))