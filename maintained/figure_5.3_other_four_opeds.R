# coppock_2022/maintained/figure_5.3_other_four_opeds.R
# Output: output/figure_5.3_other_four_opeds.pdf
# Depends on: helpers.R, data/elite_opeds_cleaned.rds, data/mturk_opeds_cleaned.rds
# Description: Amtrak, Climate, Veterans, Wall Street op-ed effects by sample and party.

source(here::here("maintained", "helpers.R"))

# The Veterans and Wall Street panels scatter their raw responses with runif() rather than
# with sunflower(), which the deposit does too and does without a seed, so the figure it
# draws is different every time it is run. Nothing in the estimates moves; only where the
# background points land. Seeded here so the committed PNG and PDF are reproducible.
set.seed(script_seed(20220526))

elite_opeds <- read_rds(path_original("data", "elite_opeds_cleaned.rds"))
mturk_opeds <- read_rds(path_original("data", "mturk_opeds_cleaned.rds"))

opeds <-
  bind_rows(
    `Policy Professional Sample` = elite_opeds,
    `Mechanical Turk Sample` = mturk_opeds,
    .id = "sample"
  ) |>
  filter(!is.na(pid_3_cat), pid_3_cat %in% c("Democrat", "Republican"))

make_oped_df <- function(data, treatment_name, dv_col, use_sunflower = TRUE) {
  d <-
    data |>
    filter(Z %in% c("control", treatment_name)) |>
    mutate(Z = factor(if_else(Z == "control", "Did not read op-ed", "Read op-ed"))) |>
    transmute(sample, pid_3_cat, Z, Y = .data[[dv_col]])

  if (use_sunflower) {
    d <-
      d |>
      group_by(sample, pid_3_cat, Z, Y) |>
      mutate(
        y_s = sunflower_compat(y = Y, width = 0.08, height = 0.3),
        x_s = sunflower_compat(x = as.numeric(Z), width = 0.08, height = 0.3),
        x_s = if_else(pid_3_cat == "Democrat", x_s - (1 / 8), x_s + (1 / 8))
      )
  } else {
    d <-
      d |>
      mutate(
        y_s = Y,
        x_s = as.numeric(Z) + runif(length(Z), -0.045, 0.045),
        x_s = if_else(pid_3_cat == "Democrat", x_s - (1 / 8), x_s + (1 / 8))
      )
  }
  d |> mutate(plot_letter = case_when(pid_3_cat == "Democrat" ~ "D", pid_3_cat == "Republican" ~ "R"))
}

amtrak <- make_oped_df(opeds, "amtrak",     "dv_amtrak_1_w1",  TRUE)
climate <- make_oped_df(opeds, "climate",    "dv_climate_1_w1", TRUE)
veterans <- make_oped_df(opeds, "veterans",   "dv_vets_1_w1",    FALSE)
wallstreet <- make_oped_df(opeds, "wallstreet", "dv_wall_1_w1",   FALSE)

dat <-
  bind_rows(amtrak = amtrak, climate = climate, veterans = veterans, wallstreet = wallstreet, .id = "oped") |>
  filter(!is.na(Y), !is.na(Z)) |>
  mutate(
    treatment = Z,
    strip = factor(oped, levels = c("amtrak", "climate", "veterans", "wallstreet"),
                        labels = c("Amtrak", "Climate", "Veterans", "Wall Street"))
  )

summary_df <-
  dat |>
  group_by(strip, oped, treatment, pid_3_cat, sample) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))) |>
  mutate(Y = estimate)

label_df <-
  summary_df |>
  filter(treatment == "Read op-ed", strip == "Climate") |>
  ungroup() |>
  mutate(Y = c(2, 5))

oped_names <- unique(dat$oped)
y_axes <- c(
  "[G]overnment should spend [1: A lot more, 7: A lot less]\non transportation and infrastructure",
  "Would you say that climate change is best described as a\n[1: Crisis, 7: Not a problem at all]",
  "How would you rate your feelings toward [the VA]\non a scale of 0 to 100 [Reversed]",
  "How would you rate your feelings toward\n[Wall Street bankers] on a scale of 0 to 100"
)
scale_breaks <- list(1:7, 1:7, seq(0, 100, 20), seq(0, 100, 20))

panel_list <- vector("list", length(oped_names))
for (i in seq_along(oped_names)) {
  panel_list[[i]] <-
    ggplot(filter(summary_df, oped == oped_names[i]),
           aes(treatment, Y, group = pid_3_cat, shape = pid_3_cat)) +
    geom_point(size = 1, position = position_dodge(width = 0.5)) +
    geom_line(position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
    geom_text(data = filter(dat, oped == oped_names[i]),
              aes(x = x_s, y = y_s, label = plot_letter), alpha = 0.2, size = 1) +
    geom_text(data = filter(label_df, oped == oped_names[i]),
              aes(label = pid_3_cat), position = position_dodge(width = 0.5), size = 2) +
    scale_y_continuous(breaks = scale_breaks[[i]]) +
    facet_grid(strip ~ sample) +
    ylab(y_axes[i]) +
    theme_bw() +
    theme(legend.position = "none", axis.title.x = element_blank(),
          axis.title.y = element_text(size = 4.5),
          panel.grid.minor = element_blank(), strip.background = element_blank())
}

design <- "AA\nB#\nCC\nDD"
figure_5.3 <- wrap_plots(A = panel_list[[1]], B = panel_list[[2]],
                          C = panel_list[[3]], D = panel_list[[4]],
                          design = design)

ggsave(path_output("figure_5.3_other_four_opeds.pdf"),
       plot = figure_5.3, width = 9, height = 12)
ggsave(path_output("figure_5.3_other_four_opeds.png"),
       plot = figure_5.3, width = 9, height = 12, dpi = 300)

write_csv(summary_df, path_output("figure_5.3_other_four_opeds.csv"))
