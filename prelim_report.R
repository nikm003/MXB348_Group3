BASE_DIR <- "C:/Users/kirti/Downloads/MXB348"
xlsx_path <- file.path(BASE_DIR, "formatted_data.xlsx")

dir.create(file.path(BASE_DIR, "outputs"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(BASE_DIR, "figs"),    showWarnings = FALSE, recursive = TRUE)

# helper functions so every file path is built off BASE_DIR in one place
OUT  <- function(f) file.path(BASE_DIR, "outputs", f)
FIG  <- function(f) file.path(BASE_DIR, "figs", f)
DATA <- function(f) file.path(BASE_DIR, f)   # for saved .rds intermediates

# ---- 0. Install/load required packages ----
required_pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "MASS", "car")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
suppressMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
})


# LOAD + RESHAPE DATA (one row per touchpoint)

raw <- read_excel(xlsx_path)
# raw columns: user_id, session_id, channels, times, conversion
# "channels"/"times" are " -> "-delimited strings, one entry per touchpoint

journey_full <- raw %>%
  mutate(row_id = row_number()) %>%              # tag each original session row before splitting
  separate_rows(channels, times, sep = " -> ") %>%  # explode "A -> B -> C" into 3 separate rows
  group_by(row_id) %>%
  mutate(touch_order = row_number() - 1) %>%      # 0 = first touch in the session, increasing after
  ungroup() %>%
  transmute(
    user_id,
    session_id,
    touch_order,
    channel = trimws(channels),
    time_till_session_end = as.numeric(trimws(times)),
    conversion
  ) %>%
  arrange(user_id, session_id, touch_order)

cat("Touchpoints:", nrow(journey_full), "\n")
cat("Sessions:", journey_full %>% distinct(user_id, session_id) %>% nrow(), "\n")
cat("Users:", n_distinct(journey_full$user_id), "\n")
print(head(journey_full, 10))

cat("channels:\n")
print(sort(unique(journey_full$channel)))


# EDA — descriptive stats and plots only

n_touchpoints <- nrow(journey_full)
n_sessions <- journey_full %>% distinct(user_id, session_id) %>% nrow()
n_users <- n_distinct(journey_full$user_id)

conv_tbl <- journey_full %>% distinct(user_id, session_id, conversion)
conversion_rate <- mean(conv_tbl$conversion)

# sink() redirects console output to a text file until sink() is called again with no args
sink(OUT("eda_summary.txt"))
cat("Touchpoints:", n_touchpoints, "\n")
cat("Sessions:", n_sessions, "\n")
cat("Users:", n_users, "\n")
cat("Overall session-level conversion rate:", round(conversion_rate, 4), "\n")

sessions_per_user <- journey_full %>% distinct(user_id, session_id) %>% count(user_id, name="n_sessions")
cat("\nSessions per user summary:\n")
print(summary(sessions_per_user$n_sessions))

cat("\nShare of users with 1 vs 2+ sessions:\n")
print(table(cut(sessions_per_user$n_sessions, breaks=c(0,1,Inf), labels=c("1 session","2+ sessions"))))
sink()

# --- channel volume: which channels get the most touchpoints ---
channel_volume <- journey_full %>% count(channel, name="touchpoints") %>% arrange(desc(touchpoints))
write.csv(channel_volume, OUT("channel_volume.csv"), row.names = FALSE)

p1 <- ggplot(channel_volume, aes(x = reorder(channel, touchpoints), y = touchpoints)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Touchpoints by Channel", x = NULL, y = "Touchpoints") +
  theme_minimal(base_size = 12)
ggsave(FIG("channel_volume.png"), p1, width=7, height=4.5, dpi=150)

# --- path length: how many touches a typical session has ---
path_length <- journey_full %>%
  count(user_id, session_id, name = "n_touches") %>%
  mutate(bucket = if_else(n_touches >= 8, "8+", as.character(n_touches))) %>%
  count(bucket, name = "sessions") %>%
  mutate(bucket = factor(bucket, levels = c(as.character(1:7), "8+")))
write.csv(path_length, OUT("path_length.csv"), row.names=FALSE)

p2 <- ggplot(path_length, aes(x = bucket, y = sessions)) +
  geom_col(fill = "steelblue") +
  labs(title = "Touches per Session (Path Length)", x = "Touches", y = "Sessions") +
  theme_minimal(base_size = 12)
ggsave(FIG("path_length.png"), p2, width=7, height=4.5, dpi=150)

# --- does conversion rate differ by whether a channel is first vs last touch? ---
first_touch <- journey_full %>% group_by(user_id, session_id) %>% filter(touch_order == min(touch_order)) %>% ungroup() %>%
  group_by(channel) %>% summarise(conversion_rate = mean(conversion), n=n(), .groups="drop") %>% mutate(position="First touch")
last_touch <- journey_full %>% group_by(user_id, session_id) %>% filter(touch_order == max(touch_order)) %>% ungroup() %>%
  group_by(channel) %>% summarise(conversion_rate = mean(conversion), n=n(), .groups="drop") %>% mutate(position="Last touch")
touch_position_conv <- bind_rows(first_touch, last_touch)
write.csv(touch_position_conv, OUT("touch_position_conv.csv"), row.names=FALSE)

p3 <- ggplot(touch_position_conv, aes(x = channel, y = conversion_rate, fill = position)) +
  geom_col(position = "dodge") +
  labs(title = "Conversion Rate by Channel: First vs Last Touch", x = NULL, y = "Conversion Rate", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(FIG("conv_by_position.png"), p3, width=8, height=5, dpi=150)

p4 <- ggplot(sessions_per_user, aes(x = n_sessions)) +
  geom_histogram(binwidth=1, fill="steelblue", color="white") +
  coord_cartesian(xlim=c(0,15)) +
  labs(title="Sessions per User", x="Number of sessions", y="Number of users") +
  theme_minimal(base_size=12)
ggsave(FIG("sessions_per_user.png"), p4, width=7, height=4.5, dpi=150)

cat("EDA done\n")


# MODEL 1 — feature engineering: return-visit conversion
# journey_full already in memory from Section 1 - no need to reload

# collapse touch-level data down to one row per session
session_level <- journey_full %>%
  group_by(user_id, session_id) %>%
  summarise(
    conversion = first(conversion),
    n_touches = n(),
    entry_channel = channel[touch_order == 0],       # channel that started this session
    session_duration = max(time_till_session_end),
    .groups = "drop"
  ) %>%
  arrange(user_id, session_id)

# touch counts per channel per session (wide), used to build historical channel-mix features
channel_counts <- journey_full %>%
  count(user_id, session_id, channel) %>%
  pivot_wider(names_from = channel, values_from = n, values_fill = 0)

session_level <- session_level %>% left_join(channel_counts, by = c("user_id","session_id"))
channels <- c("Direct","Email Marketing","Facebook","Google Display","Google Search","Instagram","Organic","Youtube")

session_level <- session_level %>% arrange(user_id, session_id)

# builds "prior_*" features per user using ONLY strictly earlier sessions (avoids information leakage)
build_prior <- function(df) {
  df <- df %>% arrange(session_id)
  n <- nrow(df)
  df$visit_number <- seq_len(n)
  df$prior_sessions <- seq_len(n) - 1
  # lag(cumsum(...)) shifts the running total back by one row, so session N only sees sessions 1..N-1
  df$prior_conversions <- lag(cumsum(df$conversion), default = 0)
  df$prior_conv_rate <- ifelse(df$prior_sessions > 0, df$prior_conversions / df$prior_sessions, NA_real_)
  df$prior_avg_touches <- lag(cummean(df$n_touches), default = NA_real_)
  df$prior_avg_duration <- lag(cummean(df$session_duration), default = NA_real_)
  for (ch in channels) {
    cs <- cumsum(df[[ch]])
    df[[paste0("prior_", make.names(ch), "_share")]] <- lag(cs, default = 0) / pmax(df$prior_sessions, 1)
  }
  df
}

session_level <- session_level %>% group_by(user_id) %>% group_modify(~ build_prior(.x)) %>% ungroup()

# only 2nd+ visits qualify as "return visits" (need at least one prior session to build prior_* features)
return_visits <- session_level %>% filter(visit_number >= 2)

cat("Return-visit rows:", nrow(return_visits), "\n")
cat("Conversion rate among return visits:", round(mean(return_visits$conversion),4), "\n")

write.csv(head(return_visits, 200), OUT("model1_features_preview.csv"), row.names=FALSE)


# MODEL 1 — fit logistic regression

set.seed(42)
d <- return_visits %>% mutate(
  entry_channel = factor(entry_channel),
  conversion = as.integer(conversion)
)

n <- nrow(d)
train_idx <- sample(seq_len(n), size = floor(0.8*n))   # 80/20 train/test split
train <- d[train_idx, ]
test  <- d[-train_idx, ]

form <- conversion ~ visit_number + prior_conv_rate + prior_avg_touches + prior_avg_duration +
  entry_channel + prior_Direct_share + prior_Email.Marketing_share + prior_Facebook_share +
  prior_Google.Display_share + prior_Google.Search_share + prior_Instagram_share +
  prior_Organic_share
# prior_Youtube_share omitted as reference level (8 shares sum to 1 -> drop one to avoid perfect collinearity)

fit <- glm(form, data = train, family = binomial())   # logistic regression

sink(OUT("model1_summary.txt"))
print(summary(fit))
cat("\n\nVIF check (values > 5 flag concerning multicollinearity):\n")
vif_vals <- tryCatch(car::vif(fit), error = function(e) NULL)
if (!is.null(vif_vals)) print(vif_vals) else cat("car::vif failed (likely aliased coefficients)\n")
sink()

test$pred_prob <- predict(fit, newdata = test, type = "response")

# manual AUC via Mann-Whitney U statistic on ranks (no extra package needed)
auc_manual <- function(labels, scores) {
  pos <- scores[labels == 1]; neg <- scores[labels == 0]
  n1 <- length(pos); n0 <- length(neg)
  r <- rank(c(pos, neg))
  (sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n0)
}
auc_val <- auc_manual(test$conversion, test$pred_prob)

# calibration check: do predicted probabilities match actual outcome rates within each decile?
test <- test %>% mutate(decile = ntile(pred_prob, 10))
calib <- test %>% group_by(decile) %>% summarise(mean_pred = mean(pred_prob), mean_actual = mean(conversion), n=n())
write.csv(calib, OUT("model1_calibration.csv"), row.names = FALSE)

# Brier score vs a naive baseline (always predict the overall rate) - is the model actually adding value?
baseline_rate <- mean(train$conversion)
brier_model <- mean((test$pred_prob - test$conversion)^2)
brier_baseline <- mean((baseline_rate - test$conversion)^2)

cat("AUC:", round(auc_val,4), "\n")
cat("Brier (model):", round(brier_model,4), " Brier (baseline):", round(brier_baseline,4), "\n")

sink(OUT("model1_performance.txt"))
cat("Test AUC:", round(auc_val,4), "\n")
cat("Test Brier score (model):", round(brier_model,4), "\n")
cat("Test Brier score (naive baseline = overall rate):", round(brier_baseline,4), "\n")
cat("Baseline (train) conversion rate used:", round(baseline_rate,4), "\n")
sink()

saveRDS(fit, DATA("model1_fit.rds"))   # keep the fitted model so it can be reloaded without refitting


# MODEL 2 — feature engineering: steps to conversion

sess_info <- journey_full %>%
  group_by(user_id, session_id) %>%
  summarise(conversion = first(conversion), max_touch = max(touch_order), .groups = "drop")

# "steps to conversion" only makes sense for sessions that actually converted (censoring caveat)
converting_sessions <- sess_info %>% filter(conversion == 1)
cat("Converting sessions:", nrow(converting_sessions), "out of", nrow(sess_info), "\n")

stage_data <- journey_full %>%
  inner_join(converting_sessions %>% dplyr::select(user_id, session_id, max_touch), by = c("user_id","session_id")) %>%
  mutate(remaining_steps = max_touch - touch_order)   # target: touches remaining until conversion

stage_data <- stage_data %>%
  arrange(user_id, session_id, touch_order) %>%
  group_by(user_id, session_id) %>%
  mutate(
    session_start_time = max(time_till_session_end),
    time_elapsed = session_start_time - time_till_session_end,   # time since session started, so far
    n_distinct_channels_so_far = cumsum(!duplicated(channel)),
    is_repeat_channel = as.integer(duplicated(channel))
  ) %>%
  ungroup()

sess_order <- sess_info %>% arrange(user_id, session_id) %>% group_by(user_id) %>%
  mutate(visit_number = row_number()) %>% ungroup() %>% dplyr::select(user_id, session_id, visit_number)
stage_data <- stage_data %>% left_join(sess_order, by = c("user_id","session_id"))

model2_df <- stage_data %>%
  transmute(user_id, session_id, touch_order, channel = factor(channel), remaining_steps,
            time_elapsed, n_distinct_channels_so_far, is_repeat_channel, visit_number)

cat("Model2 rows:", nrow(model2_df), "\n")
print(summary(model2_df$remaining_steps))
cat("Mean:", mean(model2_df$remaining_steps), " Var:", var(model2_df$remaining_steps), "\n")

write.csv(head(model2_df, 200), OUT("model2_features_preview.csv"), row.names = FALSE)


# MODEL 2 — fit Poisson + Negative Binomial

suppressMessages(library(MASS))   # needed for glm.nb() - loaded here, not at the top, on purpose
set.seed(42)

d <- model2_df
n <- nrow(d)
train_idx <- sample(seq_len(n), floor(0.8*n))
train <- d[train_idx,]; test <- d[-train_idx,]

form <- remaining_steps ~ touch_order + time_elapsed + n_distinct_channels_so_far + is_repeat_channel + channel + visit_number

pois_fit <- glm(form, data = train, family = poisson())
# dispersion stat >> 1 means variance >> mean, i.e. Poisson's equidispersion assumption is violated
disp_stat <- sum(residuals(pois_fit, type = "pearson")^2) / pois_fit$df.residual

sink(OUT("model2_poisson_summary.txt"))
print(summary(pois_fit))
cat("\nDispersion statistic (Pearson chisq / df):", round(disp_stat,3), "\n")
cat("(Values >> 1 indicate overdispersion -> Poisson SEs are too small -> use Negative Binomial)\n")
sink()
cat("Dispersion stat:", round(disp_stat,3), "\n")

nb_fit <- glm.nb(form, data = train)   # Negative Binomial corrects for the overdispersion above

sink(OUT("model2_nb_summary.txt"))
print(summary(nb_fit))
cat("\nEstimated theta (dispersion parameter):", nb_fit$theta, "\n")
sink()

test$pred_pois <- predict(pois_fit, newdata = test, type = "response")
test$pred_nb <- predict(nb_fit, newdata = test, type = "response")
naive_pred <- mean(train$remaining_steps)   # baseline: just predict the average

rmse <- function(a,b) sqrt(mean((a-b)^2))
mae  <- function(a,b) mean(abs(a-b))

perf <- data.frame(
  model = c("Naive (mean)", "Poisson GLM", "Negative Binomial GLM"),
  RMSE = c(rmse(test$remaining_steps, naive_pred), rmse(test$remaining_steps, test$pred_pois), rmse(test$remaining_steps, test$pred_nb)),
  MAE  = c(mae(test$remaining_steps, naive_pred), mae(test$remaining_steps, test$pred_pois), mae(test$remaining_steps, test$pred_nb))
)
print(perf)
write.csv(perf, OUT("model2_performance.csv"), row.names = FALSE)

saveRDS(pois_fit, DATA("model2_pois_fit.rds"))
saveRDS(nb_fit, DATA("model2_nb_fit.rds"))


# BUSINESS ATTRIBUTION — overall rate + heuristics + Markov

sess <- journey_full %>% distinct(user_id, session_id, conversion)
overall_conv_rate <- mean(sess$conversion)
cat("Overall conversion rate:", round(overall_conv_rate, 4), "\n")

# collapse each session into a single path string, e.g. "Google Search > Organic"
paths <- journey_full %>%
  arrange(user_id, session_id, touch_order) %>%
  group_by(user_id, session_id) %>%
  summarise(path = paste(channel, collapse = " > "), conversion = first(conversion), n_touch = n(), .groups = "drop")

converting_paths <- sum(paths$conversion == 1)
cat("Total paths:", nrow(paths), " Converting:", converting_paths, "\n")

# heuristic attribution only makes sense over paths that actually converted
conv_paths_only <- paths %>% filter(conversion == 1)

# --- first-touch: 100% credit to the first channel touched ---
first_touch_credit <- journey_full %>%
  inner_join(conv_paths_only %>% dplyr::select(user_id, session_id), by=c("user_id","session_id")) %>%
  group_by(user_id, session_id) %>% filter(touch_order == min(touch_order)) %>% ungroup() %>%
  count(channel, name = "credit") %>% mutate(method = "First-touch")

# --- last-touch: 100% credit to the final channel touched before converting ---
last_touch_credit <- journey_full %>%
  inner_join(conv_paths_only %>% dplyr::select(user_id, session_id), by=c("user_id","session_id")) %>%
  group_by(user_id, session_id) %>% filter(touch_order == max(touch_order)) %>% ungroup() %>%
  count(channel, name = "credit") %>% mutate(method = "Last-touch")

# --- linear: credit split evenly across every touch in the session ---
linear_credit <- journey_full %>%
  inner_join(conv_paths_only %>% dplyr::select(user_id, session_id), by=c("user_id","session_id")) %>%
  group_by(user_id, session_id) %>% mutate(w = 1/n()) %>% ungroup() %>%
  group_by(channel) %>% summarise(credit = sum(w), .groups="drop") %>% mutate(method = "Linear")

# ---- time-decay credit: touches closer to conversion get more weight ----
half_life <- 60   # tune this: smaller = more weight concentrated near conversion
time_decay_credit <- journey_full %>%
  inner_join(conv_paths_only %>% dplyr::select(user_id, session_id), by=c("user_id","session_id")) %>%
  group_by(user_id, session_id) %>%
  mutate(w = 2^(-time_till_session_end / half_life),   # exponential decay: less time-remaining = more weight
         w = w / sum(w)) %>%     # normalize weights to sum to 1 within each session
  ungroup() %>%
  group_by(channel) %>% summarise(credit = sum(w), .groups="drop") %>% mutate(method = "Time-decay")

heuristics <- bind_rows(first_touch_credit, last_touch_credit, linear_credit, time_decay_credit) %>%
  group_by(method) %>% mutate(pct = 100 * credit / sum(credit)) %>% ungroup()

write.csv(heuristics, OUT("attribution_heuristics.csv"), row.names = FALSE)
print(heuristics %>% dplyr::select(method, channel, pct) %>% pivot_wider(names_from = method, values_from = pct))


# MARKOV CHAIN REMOVAL-EFFECT ATTRIBUTION

channels <- sort(unique(journey_full$channel))
states <- c("Start", channels, "Conversion", "Null")

# counts every A->B transition across all paths, including Start-> and ->Conversion/Null
build_transitions <- function(paths_df) {
  trans <- list()
  add <- function(a,b) {
    key <- paste(a,b,sep="->")
    trans[[key]] <<- (if (is.null(trans[[key]])) 0 else trans[[key]]) + 1
  }
  seqs <- strsplit(paths_df$path, " > ")
  for (i in seq_along(seqs)) {
    s <- seqs[[i]]
    full <- c("Start", s, if (paths_df$conversion[i]==1) "Conversion" else "Null")
    for (j in 1:(length(full)-1)) add(full[j], full[j+1])
  }
  trans
}

trans_counts <- build_transitions(paths)

# build the transition probability matrix from the raw counts
mat <- matrix(0, nrow=length(states), ncol=length(states), dimnames=list(states, states))
for (key in names(trans_counts)) {
  parts <- strsplit(key, "->")[[1]]
  mat[parts[1], parts[2]] <- trans_counts[[key]]
}
mat <- mat / rowSums(mat)      # normalize counts into probabilities
mat[is.nan(mat)] <- 0
mat["Conversion","Conversion"] <- 1   # absorbing states: once here, chain stays here
mat["Null","Null"] <- 1

# fundamental matrix method for absorbing Markov chains: N = (I - Q)^-1
transient <- c("Start", channels)
absorbing <- c("Conversion","Null")
Q <- mat[transient, transient]
R <- mat[transient, absorbing]
N <- solve(diag(length(transient)) - Q)
B <- N %*% R
total_conv_prob <- B["Start","Conversion"]   # model's implied P(convert | start) - should match overall_conv_rate
cat("\nModel-based total conversion probability from Start:", round(total_conv_prob,4), "\n")

# removal effect: reroute a channel's traffic to Null, recompute conversion prob, measure the drop
removal_effect <- function(mat, channel_to_remove, transient, absorbing) {
  m2 <- mat
  m2[, "Null"] <- m2[, "Null"] + m2[, channel_to_remove]
  m2[, channel_to_remove] <- 0
  m2[channel_to_remove, ] <- 0
  m2[channel_to_remove, "Null"] <- 1
  Q2 <- m2[transient, transient]; R2 <- m2[transient, absorbing]
  N2 <- solve(diag(length(transient)) - Q2)
  B2 <- N2 %*% R2
  B2["Start","Conversion"]
}

effects <- sapply(channels, function(ch) {
  new_prob <- removal_effect(mat, ch, transient, absorbing)
  1 - (new_prob / total_conv_prob)   # % drop in conversion probability caused by removing this channel
})

markov_attr <- data.frame(channel = channels, removal_effect = effects) %>%
  mutate(pct = 100 * removal_effect / sum(removal_effect)) %>%   # normalize effects to sum to 100%
  arrange(desc(pct))

write.csv(markov_attr, OUT("attribution_markov.csv"), row.names = FALSE)
print(markov_attr)


# ATTRIBUTION — journey length, path collapsing, granularity

# reusable version of the Markov logic, so it can be re-fit on any subset of paths
run_markov <- function(paths_df, channels) {
  states <- c("Start", channels, "Conversion", "Null")
  trans_counts <- build_transitions(paths_df)
  mat <- matrix(0, nrow=length(states), ncol=length(states), dimnames=list(states, states))
  for (key in names(trans_counts)) {
    parts <- strsplit(key, "->")[[1]]
    mat[parts[1], parts[2]] <- trans_counts[[key]]
  }
  mat <- mat / rowSums(mat)
  mat[is.nan(mat)] <- 0
  mat["Conversion","Conversion"] <- 1
  mat["Null","Null"] <- 1
  transient <- c("Start", channels); absorbing <- c("Conversion","Null")
  Q <- mat[transient, transient]; R <- mat[transient, absorbing]
  N <- solve(diag(length(transient)) - Q)
  B <- N %*% R
  total_conv_prob <- B["Start","Conversion"]
  
  removal_effect_local <- function(mat, ch) {
    m2 <- mat
    m2[, "Null"] <- m2[, "Null"] + m2[, ch]
    m2[, ch] <- 0
    m2[ch, ] <- 0; m2[ch, "Null"] <- 1
    Q2 <- m2[transient, transient]; R2 <- m2[transient, absorbing]
    N2 <- solve(diag(length(transient)) - Q2)
    B2 <- N2 %*% R2
    B2["Start","Conversion"]
  }
  effects <- sapply(channels, function(ch) 1 - (removal_effect_local(mat, ch) / total_conv_prob))
  data.frame(channel = channels, pct = 100 * effects / sum(effects))
}

# Attribution by journey DURATION (not touch count)
path_durations <- journey_full %>%
  group_by(user_id, session_id) %>%
  summarise(duration = max(time_till_session_end), .groups = "drop")

paths <- paths %>% left_join(path_durations, by = c("user_id","session_id")) %>%
  # ntile() (quartiles) instead of cut() on quantiles - avoids errors from duplicate breakpoints
  # (many sessions have duration = 0, so quantile-based cut() breaks are often not unique)
  mutate(duration_bucket = ntile(duration, 4),
         duration_bucket = factor(duration_bucket, levels = 1:4,
                                  labels = c("Shortest 25%","25-50%","50-75%","Longest 25%")))

by_duration <- lapply(split(paths, paths$duration_bucket), run_markov, channels = channels)
by_duration_df <- bind_rows(lapply(names(by_duration), function(nm) by_duration[[nm]] %>% mutate(duration_bucket = nm)))
by_duration_wide <- by_duration_df %>% pivot_wider(names_from = duration_bucket, values_from = pct)
write.csv(by_duration_wide, OUT("attribution_by_duration.csv"), row.names = FALSE)
print(by_duration_wide)

# Journey-length buckets (by touch count)
paths <- paths %>% mutate(len_bucket = case_when(
  n_touch < 3 ~ "1-2",
  n_touch <= 5 ~ "3-5",
  n_touch <= 10 ~ "6-10",
  TRUE ~ "10+"
))
print(table(paths$len_bucket))

by_length <- lapply(split(paths, paths$len_bucket), run_markov, channels = channels)
by_length_df <- bind_rows(lapply(names(by_length), function(nm) by_length[[nm]] %>% mutate(len_bucket = nm)))
by_length_wide <- by_length_df %>% pivot_wider(names_from = len_bucket, values_from = pct)
write.csv(by_length_wide, OUT("attribution_by_length.csv"), row.names = FALSE)
print(by_length_wide)

# Collapsed adjacent-repeat paths
collapse_path <- function(p) {
  s <- strsplit(p, " > ")[[1]]
  keep <- c(TRUE, s[-1] != s[-length(s)])   # keep a step only if it differs from the one before it
  paste(s[keep], collapse = " > ")
}
paths_collapsed <- paths %>% mutate(path = sapply(path, collapse_path))
n_touch_collapsed <- lengths(strsplit(paths_collapsed$path, " > "))
cat("\nShare of paths shortened by collapsing:", round(mean(n_touch_collapsed < paths$n_touch),4), "\n")

# compare attribution on collapsed vs original paths
collapsed_result <- run_markov(paths_collapsed, channels) %>% rename(pct_collapsed = pct)
original_result <- run_markov(paths, channels) %>% rename(pct_original = pct)
compare_collapse <- original_result %>% left_join(collapsed_result, by="channel") %>%
  mutate(abs_diff = abs(pct_original - pct_collapsed))
write.csv(compare_collapse, OUT("attribution_collapsed_compare.csv"), row.names=FALSE)
print(compare_collapse)

# Level 1 (3 groups) vs Level 2 (8 channels) consistency
group_map <- c(
  "Organic"="Free","Direct"="Free","Email Marketing"="Free",
  "Google Search"="Google Paid","Google Display"="Google Paid","Youtube"="Google Paid",
  "Facebook"="Meta Paid","Instagram"="Meta Paid"
)
# re-run Markov directly
paths_grouped <- paths %>% mutate(path = sapply(strsplit(path, " > "), function(s) paste(group_map[s], collapse=" > ")))
level1_channels <- c("Free","Google Paid","Meta Paid")
level1_result <- run_markov(paths_grouped, level1_channels)

# vs. summing the original 8-channel (Level 2) results within each group
level2_result <- run_markov(paths, channels) %>% mutate(group = group_map[channel])
level2_rollup <- level2_result %>% group_by(group) %>% summarise(pct_rollup_from_L2 = sum(pct), .groups="drop")

# do the two levels agree?
consistency_check <- level1_result %>% rename(group = channel, pct_level1 = pct) %>%
  left_join(level2_rollup, by = "group") %>%
  mutate(diff = pct_level1 - pct_rollup_from_L2)

write.csv(consistency_check, OUT("attribution_level_consistency.csv"), row.names=FALSE)
print(consistency_check)


# ATTRIBUTION PLOTS

heur <- read.csv(OUT("attribution_heuristics.csv"))
markov <- read.csv(OUT("attribution_markov.csv")) %>% dplyr::select(channel, pct) %>% mutate(method="Markov removal-effect")
combo <- bind_rows(heur %>% dplyr::select(channel, pct, method), markov)
combo$method <- factor(combo$method, levels=c("First-touch","Last-touch","Linear","Markov removal-effect"))

p <- ggplot(combo, aes(x=reorder(channel, pct, FUN=median), y=pct, fill=method)) +
  geom_col(position="dodge") +
  coord_flip() +
  labs(title="Channel Attribution: Heuristic vs Markov Methods", x=NULL, y="Attribution (%)", fill=NULL) +
  theme_minimal(base_size=12)
ggsave(FIG("attribution_compare.png"), p, width=8, height=5, dpi=150)

by_len <- read.csv(OUT("attribution_by_length.csv"))
by_len_long <- by_len %>% pivot_longer(-channel, names_to="len_bucket", values_to="pct") %>%
  mutate(len_bucket = factor(len_bucket, levels=c("X1.2","X3.5","X6.10","X10.")))
levels(by_len_long$len_bucket) <- c("1-2","3-5","6-10","10+")

p2 <- ggplot(by_len_long, aes(x=len_bucket, y=pct, color=channel, group=channel)) +
  geom_line(linewidth=1) + geom_point(size=2) +
  labs(title="Markov Attribution by Journey Length", x="Journey length (touches)", y="Attribution (%)", color=NULL) +
  theme_minimal(base_size=12)
ggsave(FIG("attribution_by_length.png"), p2, width=8, height=5, dpi=150)

cat("\nALL DONE. Check the 'outputs' and 'figs' subfolders inside:\n", BASE_DIR, "\n")