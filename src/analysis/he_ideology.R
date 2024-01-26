## HE Ideology
d <- d |> 
  mutate(E2_2 = recode(E2_2,
                       `very conservative` = 1,
                       `consersative` = 2,
                       `center` = 3,
                       `progressive` = 4,
                       `very progressive` = 5))
d_left <- d |> 
  filter(ideology==0)
d_right <- d |>  
  filter(ideology==1)

tmp <- lm(H19_1 ~  treatment*E2_2 + factor(urbanisation), d_right)

he_i <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |> 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

tmp <- lm(H19_1 ~  treatment*E2_2 + factor(urbanisation), d_left)
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |> 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_2 ~  treatment*E2_2 + factor(urbanisation), d_right)
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |> 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_2 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |> 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_3 ~  treatment*E2_2 + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_3 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_4 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_4 ~  treatment*E2_2 + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_5 ~  treatment*E2_2 + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_5 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Progressive Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_6 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_6 ~  treatment*E2_2 + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_7 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_7 ~  treatment*E2_2 + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_8 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Living on Benefits",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_8 ~  treatment*E2_2 + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Living on Benefits",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)


he_i <- he_i |> 
  add_case(tmp)

#tmp <- lm(H19_9 ~  treatment*E2_2 + factor(urbanisation), d_left) 
#tmp <- tidy(margins::margins(tmp, variables = "treatment",
#                             at = list("E2_2" = 1:5))) |>
#  mutate(y = "Goverment",
#         var = "Affective Sentiment for Ideological Social In-Groups",
#         id = "Progressive Voters",
#         hyp = "a",
#         upper = (estimate + (std.error * 1.56)),
#         lower = (estimate - (std.error * 1.56))) |> 
#  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

#he_i <- he_i |> 
#  add_case(tmp)

#tmp <- lm(H19_9 ~  treatment*E2_2 + factor(urbanisation), d_right) 
#tmp <- tidy(margins::margins(tmp, variables = "treatment",
#                             at = list("E2_2" = 1:5))) |>
#  mutate(y = "Goverment",
#         var = "Affective Sentiment for Ideological Social Out-Groups",
#         id = "Conservative Voters",
#         hyp = "b",
#         upper = (estimate + (std.error * 1.56)),
#         lower = (estimate - (std.error * 1.56))) |> 
#  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

#he_i <- he_i |> 
#  add_case(tmp)

tmp <- lm(H19_10 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Progressive Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_10 ~  treatment*E2_2 + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Conservative Voters",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_11 ~  treatment*E2_2 + factor(urbanisation), d_right) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Conservative Voters",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp, `at.value`)

he_i <- he_i |> 
  add_case(tmp)

tmp <- lm(H19_11 ~  treatment*E2_2 + factor(urbanisation), d_left) 
tmp <- tidy(margins::margins(tmp, variables = "treatment",
                             at = list("E2_2" = 1:5))) |>
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
  ggplot(aes(y = estimate, x = `at.value`,
            ymin = lower, ymax = upper,
             color = term, fill = term)) +
  #geom_ribbon(alpha = 0.2) +
  geom_line() +
  facet_grid(y~id, scales = "free") +
  labs(y = "", x = "") + 
  #x = "Predicted Effect for Support for Social In-Groups",
  #caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1e_r <- he_i |> 
  filter(hyp == "a", id == "Conservative Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(y = estimate, x = `at.value`,
             ymin = lower, ymax = upper,
             color = term, fill = term)) +
  #geom_ribbon(alpha = 0.2) +
  geom_line() +
  facet_grid(y~id, scales = "free") +
  labs(x = "Ideological Self-Placement \n (1 = Very Conservative, 5 = Very Progressive)", 
       y = "Marginal Effect of Support for Social In-Groups",
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
  ggplot(aes(y = estimate, x = `at.value`,
             ymin = lower, ymax = upper,
             color = term, fill = term)) +
  #geom_ribbon(alpha = 0.2) +
  geom_line() +
  facet_grid(y~id, scales = "free") +
  labs(y = "", x = "") + 
  #x = "Predicted Effect for Support for Social In-Groups",
  #caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p2e_r <- he_i |> 
  filter(hyp == "b", id == "Conservative Voters",
         term %in% c("No Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat",
                     "Nostalgia, Scapegoat")) |> 
  ggplot(aes(y = estimate, x = `at.value`,
             ymin = lower, ymax = upper,
             color = term, fill = term)) +
  #geom_ribbon(alpha = 0.2) +
  geom_line() +
  facet_grid(y~id, scales = "free") +
  labs(x = "Ideological Self-Placement \n (1 = Very Conservative, 5 = Very Progressive)", 
       y = "Marginal Effect of Support for Social Out-Groups",
       caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariate Urbanization") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))
