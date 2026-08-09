# coppock_2022/maintained/figure_3.1_oped_target_nontarget.R
# Output: output/figure_3.1_oped_target_nontarget.pdf
# Depends on: helpers.R, data/elite_opeds_cleaned.rds, data/mturk_opeds_cleaned.rds
# Description: Op-ed ATEs on target vs. non-target attitudes, both samples.
# Note: the archive calls geom_brace(), which it gets from the GitHub build of ggbrace its
#   README asks for. The CRAN release exports stat_brace() and stat_bracetext() and no
#   geom_brace(), so the deposited call fails against a library installed the ordinary
#   way. stat_brace() is the substitution.

source(here::here("maintained", "helpers.R"))

elite_opeds <- read_rds(path_original("data", "elite_opeds_cleaned.rds"))
mturk_opeds <- read_rds(path_original("data", "mturk_opeds_cleaned.rds"))

opeds <-
  bind_rows(
    `Policy Professional Sample` = elite_opeds,
    `Mechanical Turk Sample` = mturk_opeds,
    .id = "sample"
  )

gathered_df <-
  opeds |>
  pivot_longer(
    cols = c(dv_amtrak_1_s_w1, dv_climate_1_s_w1, dv_vets_1_s_w1,
                  dv_wall_1_s_w1, dv_flat_1_s_w1),
    names_to = "dv_question",
    values_to = "dv_value"
  ) |>
  filter(!is.na(dv_value)) |>
  mutate(Z_fac = relevel(factor(Z), ref = "control"))

gg_df <-
  gathered_df |>
  group_by(dv_question, sample) |>
  reframe(tidy(lm_robust(dv_value ~ Z_fac, data = pick(everything())))) |>
  filter(term != "(Intercept)") |>
  mutate(
    Outcome = factor(
      dv_question,
      levels = c("dv_amtrak_1_s_w1", "dv_climate_1_s_w1", "dv_flat_1_s_w1",
                 "dv_vets_1_s_w1", "dv_wall_1_s_w1"),
      labels = c("Amtrak", "Climate", "Flat Tax", "Veterans", "Wall Street")
    ),
    Treatment = factor(
      term,
      levels = c("Z_facamtrak", "Z_facclimate", "Z_facpaul", "Z_facveterans", "Z_facwallstreet"),
      labels = c("Amtrak", "Climate", "Flat Tax", "Veterans", "Wall Street")
    ),
    Treatment = factor(Treatment, levels = rev(levels(Treatment))),
    target = if_else(Treatment == Outcome, "Target attitude", "Nontarget attitude"),
    Outcome_label = factor(
      dv_question,
      levels = c("dv_amtrak_1_s_w1", "dv_climate_1_s_w1", "dv_flat_1_s_w1",
                 "dv_vets_1_s_w1", "dv_wall_1_s_w1"),
      labels = c("Outcome: Amtrak", "Outcome: Climate", "Outcome: Flat Tax",
                 "Outcome: Veterans", "Outcome: Wall Street")
    ),
    Treatment_label = factor(
      term,
      levels = c("Z_facamtrak", "Z_facclimate", "Z_facpaul", "Z_facveterans", "Z_facwallstreet"),
      labels = c("Treatment:\nAmtrak", "Treatment:\nClimate", "Treatment:\nFlat Tax",
                 "Treatment:\nVeterans", "Treatment:\nWall Street")
    ),
    Treatment_label = factor(Treatment_label, levels = rev(levels(Treatment_label))),
    Sample = factor(
      sample,
      levels = c("Mechanical Turk Sample", "Policy Professional Sample"),
      labels = c("Sample: Mechanical Turk", "Sample: Policy Professionals")
    )
  )

label_df <-
  gg_df |>
  filter(
    Sample    == "Sample: Mechanical Turk",
    Outcome   == "Amtrak",
    Treatment == "Amtrak",
    target    == "Target attitude"
  )

brace_df <-
  tibble(
    Sample = "Sample: Mechanical Turk",
    Outcome_label = "Outcome: Amtrak",
    label = "Non-target attitudes",
    x = c(0.2, 0.5),
    y = c(1, 4)
  )

brace_label_df <-
  tibble(
    Sample = "Sample: Mechanical Turk",
    Outcome_label = "Outcome: Amtrak",
    label = "Non-target attitudes",
    x = 0.6,
    y = 2.5
  )

figure_3.1 <-
  ggplot(gg_df, aes(estimate, Treatment_label, color = target, shape = target)) +
  geom_point(size = 1) +
  # geom_linerange replaces deprecated geom_errorbarh
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = gray(0.5), linewidth = 0.25) +
  geom_text(data = label_df, aes(label = target),
            nudge_y = 0.3, nudge_x = -0.25,
            color = grays[1], size = 1.5) +
  geom_text(data = brace_label_df,
            aes(x = x, y = y, label = label),
            angle = 90, color = grays[1], size = 1.5, inherit.aes = FALSE) +
  # stat_brace replaces removed geom_brace from ggbrace
  stat_brace(data = brace_df,
             aes(x = x, y = y),
             inherit.aes = FALSE,
             color = grays[1],
             linewidth = 0.25,
             rotate = 90) +
  scale_color_manual(values = rev(grays)) +
  facet_grid(Sample ~ Outcome_label, scales = "free_y", space = "free_y") +
  theme_bw() +
  coord_cartesian(xlim = c(-.8, .8)) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 4),
    axis.title.y = element_blank(),
    axis.ticks = element_line(linewidth = 0.25)
  ) +
  ylab("Treatment op-ed") +
  xlab("Estimated average treatment effect in standardized units")

ggsave(path_output("figure_3.1_oped_target_nontarget.pdf"),
       plot = figure_3.1, width = 9, height = 5)
ggsave(path_output("figure_3.1_oped_target_nontarget.png"),
       plot = figure_3.1, width = 9, height = 5, dpi = 300)

write_csv(gg_df, path_output("figure_3.1_oped_target_nontarget.csv"))
