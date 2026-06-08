# =============================================================================

library(tidyverse)
library(psych)
library(car)
library(corrplot)
library(lm.beta)
library(broom)

# =============================================================================
# КРОК 2. ВІДБІР КОЛОНОК
# =============================================================================

keep_idx <- c(0,1,2,3,4,5,6,7,
              8,9,10,11,
              12,
              13,14,15,16,17,
              18,19,20,21,22,23,24,25,26,
              27,28,29,30,31,32,
              33,35,36,
              37,
              38,39,40,41,42,
              43,44,
              50,51,52,53,
              55,56,57,58,
              60,62,63,64,
              66,67,68,69,70,71,
              72,73,
              75,76,77,78,79,80,
              81,82,83,84,
              85,
              86,87,88,89,
              91,92,93,
              94,95,96,97,98,
              99)

df_clean <- df_raw[, keep_idx + 1]
cat("Робочих колонок після відбору:", ncol(df_clean), "\n")

# =============================================================================
# КРОК 3. ПЕРЕЙМЕНУВАННЯ КОЛОНОК
# =============================================================================

names(df_clean) <- c(
  "timestamp",
  "gender", "age", "education", "income", "welfare", "region", "settlement",
  "dig1_gov", "dig2_data", "dig3_app", "dig4_site",
  "egov_freq",
  "dtrust1_bank", "dtrust2_egov", "dtrust3_bankid", "dtrust4_surveys", "dtrust5_petitions",
  "pol_interest", "vote_history", "vote_motivation", "vote_barrier",
  "social_media_time", "news_source", "news_check", "digital_civic", "digital_civic_eff",
  "cog1_programs", "cog2_unprep_bad", "cog3_info_time", "cog4_impact",
  "cog5_nodiff_R", "cog6_quick_R",
  "cog7_compare", "cog8_read_prog", "cog9_multi_src",
  "time_decision",
  "norm1_duty", "norm2_guilt", "norm3_even_bad", "norm4_collective", "norm5_indif_R",
  "ev_awareness", "ev_concept",
  "ev_security", "ev_transparency", "ev_anonymity", "ev_access",
  "trad_security", "trad_transparency", "trad_anonymity", "trad_access",
  "imp_security", "imp_access", "imp_anonymity", "imp_transparency",
  "evgen1_convenient", "evgen2_others", "evgen3_risks_R", "evgen4_sec_doubt_R",
  "evgen5_trust_ctrl", "evgen6_no_guar_R",
  "info_fraud", "info_digi",
  "src_gov", "src_media", "src_intl", "src_bloggers", "src_friends", "src_telegram",
  "etrust1_count", "etrust2_results", "etrust3_rules", "etrust4_secret",
  "ev_admin_trust",
  "eveff1_less_serious", "eveff2_less_deliber", "eveff3_no_loss", "eveff4_format",
  "ev_conditions", "ev_would_use", "info_ev_reliability",
  "inst_rada", "inst_cabinet", "inst_cec", "inst_local", "inst_courts",
  "cog10_delay"
)
cat("Перейменування завершено.\n")

# =============================================================================
# КРОК 3.5. ОЧИЩЕННЯ vote_history
# =============================================================================

df_clean$vote_clean <- case_when(
  df_clean$vote_history %in% c(
    "Брав/ла участь регулярно",
    "Брав/ла участь регулярно (на більшості проведених виборів)"
  ) ~ "Регулярно",
  df_clean$vote_history %in% c(
    "Брав/ла участь іноді",
    "Брав/ла участь іноді (на деяких з проведених виборів)"
  ) ~ "Іноді",
  df_clean$vote_history == "Ніколи" ~ "Ніколи",
  df_clean$vote_history == "Ще не мав/мала можливості (наприклад, не досяг/ла 18 років під час виборів)" ~ "Не мав/мала можливості",
  TRUE ~ NA_character_
)
df_clean$vote_clean <- factor(df_clean$vote_clean,
  levels = c("Регулярно", "Іноді", "Ніколи", "Не мав/мала можливості"))
df_clean$has_voted <- df_clean$vote_clean %in% c("Регулярно", "Іноді")

cat("Розподіл vote_clean:\n")
print(table(df_clean$vote_clean, useNA = "always"))

# =============================================================================
# КРОК 3.6. ДВА ДАТАФРЕЙМИ
# =============================================================================

df_norm <- df_clean
df_cog  <- df_clean %>% filter(!is.na(cog1_programs))

cat("df_norm (N=", nrow(df_norm), ") | df_cog (N=", nrow(df_cog), ")\n")

# =============================================================================
# КРОК 4. КОДУВАННЯ
# =============================================================================

code_demographics <- function(df) {
  df$gender_num <- case_when(
    df$gender == "Жіноча"   ~ 1,
    df$gender == "Чоловіча" ~ 2,
    TRUE ~ NA_real_)
  df$gender_f <- factor(df$gender_num, levels=c(1,2), labels=c("Жінка","Чоловік"))
  df$age_num <- case_when(
    df$age == "18-24 роки"  ~ 1,
    df$age == "25-34 роки"  ~ 2,
    df$age == "35-44 роки"  ~ 3,
    df$age == "45-60 років" ~ 4,
    df$age == "60+ років"   ~ 5,
    TRUE ~ NA_real_)
  df$education_num <- case_when(
    df$education == "Середня освіта"              ~ 1,
    df$education == "Професійна освіта / коледж" ~ 2,
    df$education == "Бакалавр"                   ~ 3,
    df$education == "Магістр"                    ~ 4,
    df$education == "Науковий ступінь (PhD)"     ~ 5,
    TRUE ~ NA_real_)
  df$income_num <- case_when(
    df$income == "Менше ніж 100 тис. гривень"     ~ 1,
    df$income == "Від 101 до 200 тис. гривень"    ~ 2,
    df$income == "Від 201 до 400 тис. гривень"    ~ 3,
    df$income == "Від 401 до 600 тис. гривень"    ~ 4,
    df$income == "Від 601 до 800 тис. гривень"    ~ 5,
    df$income == "Від 801 до 1 мільйонна гривень" ~ 6,
    df$income == "Понад 1 мільйон гривень"        ~ 7,
    TRUE ~ NA_real_)
  df$welfare_num <- case_when(
    df$welfare == "Не вистачає на базові потреби"                                ~ 1,
    df$welfare == "Вистачає на харчування та одяг, але дорогі покупки недоступні" ~ 2,
    df$welfare == "Можемо купувати дорогі речі"                                  ~ 3,
    df$welfare == "Живемо заможно"                                               ~ 4,
    TRUE ~ NA_real_)
  df$settlement_num <- case_when(
    df$settlement == "Місто (понад 100 тис. мешканців)" ~ 4,
    df$settlement == "Місто (до 100 тис. мешканців)"    ~ 3,
    df$settlement == "Селище міського типу"              ~ 2,
    df$settlement == "Село"                              ~ 1,
    TRUE ~ NA_real_)
  df$region_f <- as.factor(df$region)
  df$pol_interest_num <- case_when(
    df$pol_interest == "Зовсім не цікавлюся"    ~ 1,
    df$pol_interest == "Радше не цікавлюся"     ~ 2,
    df$pol_interest == "Певною мірою цікавлюся" ~ 3,
    df$pol_interest == "Радше цікавлюся"        ~ 4,
    df$pol_interest == "Дуже цікавлюся"         ~ 5,
    TRUE ~ NA_real_)
  df$ev_would_use_num <- case_when(
    df$ev_would_use == "Так"                          ~ 5,
    df$ev_would_use == "Скоріше так"                 ~ 4,
    df$ev_would_use == "Не знаю / залежить від умов" ~ 3,
    df$ev_would_use == "Скоріше ні"                  ~ 2,
    df$ev_would_use == "Ні"                          ~ 1,
    TRUE ~ NA_real_)
  df$vote_clean <- case_when(
    df$vote_history %in% c("Брав/ла участь регулярно",
      "Брав/ла участь регулярно (на більшості проведених виборів)") ~ "Регулярно",
    df$vote_history %in% c("Брав/ла участь іноді",
      "Брав/ла участь іноді (на деяких з проведених виборів)")      ~ "Іноді",
    df$vote_history == "Ніколи"                                     ~ "Ніколи",
    df$vote_history == "Ще не мав/мала можливості (наприклад, не досяг/ла 18 років під час виборів)" ~ "Не мав/мала можливості",
    TRUE ~ NA_character_)
  df$vote_clean <- factor(df$vote_clean,
    levels = c("Регулярно","Іноді","Ніколи","Не мав/мала можливості"))
  df$has_voted <- df$vote_clean %in% c("Регулярно","Іноді")
  df
}

df_norm <- code_demographics(df_norm)
df_cog  <- code_demographics(df_cog)
cat("Кодування завершено.\n")

# =============================================================================
# КРОК 5. ЧИСЛОВІ ШКАЛИ
# =============================================================================

likert_cols <- c(
  "dig1_gov","dig2_data","dig3_app","dig4_site",
  "dtrust1_bank","dtrust2_egov","dtrust3_bankid","dtrust4_surveys","dtrust5_petitions",
  "cog1_programs","cog2_unprep_bad","cog3_info_time","cog4_impact",
  "cog5_nodiff_R","cog6_quick_R",
  "cog7_compare","cog8_read_prog","cog9_multi_src","cog10_delay",
  "norm1_duty","norm2_guilt","norm3_even_bad","norm4_collective","norm5_indif_R",
  "ev_security","ev_transparency","ev_anonymity","ev_access",
  "trad_security","trad_transparency","trad_anonymity","trad_access",
  "imp_security","imp_access","imp_anonymity","imp_transparency",
  "evgen1_convenient","evgen2_others","evgen3_risks_R","evgen4_sec_doubt_R",
  "evgen5_trust_ctrl","evgen6_no_guar_R",
  "src_gov","src_media","src_intl","src_bloggers","src_friends","src_telegram",
  "etrust1_count","etrust2_results","etrust3_rules","etrust4_secret",
  "eveff1_less_serious","eveff2_less_deliber","eveff3_no_loss","eveff4_format",
  "inst_rada","inst_cabinet","inst_cec","inst_local","inst_courts"
)

df_norm[likert_cols] <- lapply(df_norm[likert_cols], as.numeric)
df_cog[likert_cols]  <- lapply(df_cog[likert_cols],  as.numeric)

# =============================================================================
# КРОК 6. REVERSE CODING
# =============================================================================

reverse_code <- function(df) {
  df$cog5_nodiff   <- 6 - df$cog5_nodiff_R
  df$cog6_quick    <- 6 - df$cog6_quick_R
  df$norm5_indif   <- 6 - df$norm5_indif_R
  df$evgen3_risks  <- 6 - df$evgen3_risks_R
  df$evgen4_sec    <- 6 - df$evgen4_sec_doubt_R
  df$evgen6_noguar <- 6 - df$evgen6_no_guar_R
  df
}

df_norm <- reverse_code(df_norm)
df_cog  <- reverse_code(df_cog)

# =============================================================================
# КРОК 7. ІНДЕКСИ + АЛЬФА
# =============================================================================

cat("\n========== АЛЬФА КРОНБАХА ==========\n")

cog_items <- df_cog %>%
  select(cog1_programs, cog2_unprep_bad, cog3_info_time, cog4_impact,
         cog5_nodiff, cog6_quick,
         cog7_compare, cog8_read_prog, cog9_multi_src, cog10_delay)
a_cog <- alpha(cog_items)
cat("Когнітивний ERR (deliberation, N=523), alpha =", round(a_cog$total$raw_alpha, 3), "\n")
df_cog$ERR_cognitive <- rowMeans(cog_items, na.rm = TRUE)

norm_items_norm <- df_norm %>%
  select(norm1_duty, norm2_guilt, norm3_even_bad, norm4_collective, norm5_indif)
a_norm <- alpha(norm_items_norm)
cat("Нормативний ERR (civic duty, N=554), alpha =", round(a_norm$total$raw_alpha, 3), "\n")
df_norm$ERR_normative <- rowMeans(norm_items_norm, na.rm = TRUE)

norm_items_cog <- df_cog %>%
  select(norm1_duty, norm2_guilt, norm3_even_bad, norm4_collective, norm5_indif)
df_cog$ERR_normative <- rowMeans(norm_items_cog, na.rm = TRUE)

all_err_items <- df_cog %>%
  select(cog1_programs, cog2_unprep_bad, cog3_info_time, cog4_impact,
         cog5_nodiff, cog6_quick,
         cog7_compare, cog8_read_prog, cog9_multi_src, cog10_delay,
         norm1_duty, norm2_guilt, norm3_even_bad, norm4_collective, norm5_indif)
a_err <- alpha(all_err_items)
cat("Зведений ERR (N=523), alpha =", round(a_err$total$raw_alpha, 3), "\n")
df_cog$ERR_index <- rowMeans(all_err_items, na.rm = TRUE)

for (dfname in c("df_norm", "df_cog")) {
  d <- get(dfname)
  d$digital_literacy <- rowMeans(
    d %>% select(dig1_gov, dig2_data, dig3_app, dig4_site), na.rm=TRUE)
  d$digital_trust <- rowMeans(
    d %>% select(dtrust1_bank, dtrust2_egov, dtrust3_bankid,
                 dtrust4_surveys, dtrust5_petitions), na.rm=TRUE)
  d$electoral_trust <- rowMeans(
    d %>% select(etrust1_count, etrust2_results, etrust3_rules, etrust4_secret), na.rm=TRUE)
  d$institutional_trust <- rowMeans(
    d %>% select(inst_rada, inst_cabinet, inst_cec, inst_local, inst_courts), na.rm=TRUE)
  d$delta_security     <- d$ev_security     - d$trad_security
  d$delta_transparency <- d$ev_transparency - d$trad_transparency
  d$delta_anonymity    <- d$ev_anonymity    - d$trad_anonymity
  d$delta_access       <- d$ev_access       - d$trad_access
  assign(dfname, d)
}
cat("Індекси побудовано.\n")

# =============================================================================
# КРОК 8. ОПИСОВА СТАТИСТИКА
# =============================================================================

cat("\n========== ОПИСОВА СТАТИСТИКА ==========\n")
index_vars <- c("ERR_index","ERR_cognitive","ERR_normative",
                "digital_literacy","digital_trust","electoral_trust",
                "institutional_trust",
                "ev_security","ev_transparency","ev_anonymity","ev_access")
print(describe(df_cog[index_vars])[, c("n","mean","sd","min","max","skew","kurtosis")])

# =============================================================================
# КРОК 9. КОРЕЛЯЦІЇ
# =============================================================================

cat("\n========== КОРЕЛЯЦІЙНА МАТРИЦЯ ==========\n")
iv_cor <- df_cog %>%
  select(ev_security, ev_transparency, ev_anonymity, ev_access,
         digital_literacy, digital_trust, electoral_trust,
         pol_interest_num, age_num, education_num, income_num, welfare_num)
cor_mat <- cor(iv_cor, use = "complete.obs", method = "pearson")
print(round(cor_mat, 2))

# =============================================================================
# КРОК 10. РЕГРЕСІЙНІ МОДЕЛІ
# =============================================================================

cat("\n========== РЕГРЕСІЙНІ МОДЕЛІ ==========\n")

controls <- "gender_num + age_num + education_num + income_num + welfare_num +
             pol_interest_num + digital_literacy + electoral_trust"

f1 <- as.formula(paste("ERR_index ~",
  "ev_security + ev_transparency + ev_anonymity + ev_access +", controls))
m1 <- lm(f1, data = df_cog)
cat("Модель 1 — Зведений ERR (N=523):\n"); print(summary(m1))

f2 <- as.formula(paste("ERR_cognitive ~",
  "ev_security + ev_transparency + ev_anonymity + ev_access +", controls))
m2 <- lm(f2, data = df_cog)
cat("Модель 2 — Когнітивний ERR (N=523):\n"); print(summary(m2))

f3 <- as.formula(paste("ERR_normative ~",
  "ev_security + ev_transparency + ev_anonymity + ev_access +", controls))
m3 <- lm(f3, data = df_norm)
cat("Модель 3 — Нормативний ERR (N=554):\n"); print(summary(m3))

# =============================================================================
# КРОК 11. СУБГРУПОВІ АНАЛІЗИ
# =============================================================================

cat("\n========== СУБГРУПОВІ АНАЛІЗИ ==========\n")

cat("T-тест за статтю:\n")
print(t.test(ERR_index ~ gender_f, data = df_cog %>% filter(!is.na(gender_f))))

cat("\nANOVA за освітою:\n")
aov_edu <- aov(ERR_index ~ as.factor(education_num), data = df_cog)
print(summary(aov_edu))
print(TukeyHSD(aov_edu))

cat("\nKореляції Спірмена:\n")
for (nm in c("education_num","age_num","income_num","pol_interest_num")) {
  r <- cor.test(df_cog$ERR_index, df_cog[[nm]], method="spearman")
  cat(sprintf("  ERR ~ %s: rho=%.3f, p=%.4f\n", nm, r$estimate, r$p.value))
}

# =============================================================================
# КРОК 12. ГОТОВНІСТЬ ДО Е-ГОЛОСУВАННЯ
# =============================================================================

cat("\n========== ГОТОВНІСТЬ ДО Е-ГОЛОСУВАННЯ ==========\n")
print(table(df_norm$ev_would_use, useNA="no"))

# =============================================================================
# КРОК 15. ЗБЕРЕЖЕННЯ
# =============================================================================

write.csv(df_cog, "klus_clean_v2.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("Збережено: klus_clean_v2.csv\n")

# =============================================================================
# БЛОК: АНАЛІЗ НЕВИБОРЦІВ
# =============================================================================

cat("\n========== АНАЛІЗ НЕВИБОРЦІВ ==========\n")

# 1. Розподіл груп
cat("Розподіл vote_clean (df_norm, N=554):\n")
print(table(df_norm$vote_clean, useNA="always"))

nonvoters_norm <- df_norm %>%
  filter(vote_clean %in% c("Ніколи", "Не мав/мала можливості"))
cat("Невиборців:", nrow(nonvoters_norm), "\n")

# 2. ERR_normative за групами
cat("\n--- ERR_normative за групами (N=554) ---\n")
print(df_norm %>%
  filter(!is.na(vote_clean)) %>%
  group_by(vote_clean) %>%
  summarise(n=n(),
    M  = round(mean(ERR_normative, na.rm=TRUE), 3),
    SD = round(sd(ERR_normative,   na.rm=TRUE), 3)))

aov_norm <- aov(ERR_normative ~ vote_clean, data=df_norm)
print(summary(aov_norm))
print(TukeyHSD(aov_norm))

# 3. Сприйняття е-голосування — невиборці vs виборці
df_norm <- df_norm %>%
  mutate(nonvoter = vote_clean %in% c("Ніколи", "Не мав/мала можливості"))

cat("\n--- Сприйняття е-голосування (N=554) ---\n")
print(df_norm %>%
  filter(!is.na(vote_clean)) %>%
  mutate(voter_label = ifelse(nonvoter, "Невиборці", "Виборці")) %>%
  group_by(voter_label) %>%
  summarise(n=n(),
    Безпека     = round(mean(ev_security,     na.rm=TRUE), 3),
    Прозорість  = round(mean(ev_transparency, na.rm=TRUE), 3),
    Анонімність = round(mean(ev_anonymity,    na.rm=TRUE), 3),
    Доступність = round(mean(ev_access,       na.rm=TRUE), 3)))

cat("\nKruskal-Wallis:\n")
for (var in c("ev_security","ev_transparency","ev_anonymity","ev_access")) {
  kt <- kruskal.test(df_norm[[var]] ~ df_norm$vote_clean)
  cat(sprintf("  %s: chi2=%.3f, p=%.4f\n", var, kt$statistic, kt$p.value))
}

# 4. Готовність невиборців
cat("\n--- Готовність невиборців ---\n")
print(nonvoters_norm %>%
  count(ev_would_use) %>%
  mutate(pct=round(n/sum(n)*100, 1)) %>%
  arrange(desc(n)))

cat("\n--- Готовність виборців ---\n")
print(df_norm %>%
  filter(has_voted) %>%
  count(ev_would_use) %>%
  mutate(pct=round(n/sum(n)*100, 1)) %>%
  arrange(desc(n)))

# 5. has_voted як контрольна змінна
cat("\n--- Модель 1 + has_voted ---\n")
df_cog <- df_cog %>%
  mutate(has_voted_num = as.numeric(has_voted))

m1_with_vote <- lm(ERR_index ~
  ev_security + ev_transparency + ev_anonymity + ev_access +
  has_voted_num +
  gender_num + age_num + education_num + income_num + welfare_num +
  pol_interest_num + digital_literacy + electoral_trust,
  data=df_cog)
print(summary(m1_with_vote))

cat("\nПорівняння моделей:\n")
print(anova(m1, m1_with_vote))

coef_vote <- tidy(m1_with_vote) %>% filter(term=="has_voted_num")
cat(sprintf("has_voted: beta=%.3f, SE=%.3f, p=%.4f\n",
  coef_vote$estimate, coef_vote$std.error, coef_vote$p.value))

# 6. Регресія лише для невиборців
cat("\n--- Регресія ERR_normative на невиборцях (N=",
    nrow(nonvoters_norm), ") ---\n")
m_nonvoters <- lm(ERR_normative ~
  ev_security + ev_transparency + ev_anonymity + ev_access +
  gender_num + age_num + education_num + income_num +
  pol_interest_num + digital_literacy,
  data=nonvoters_norm)
print(summary(m_nonvoters))

cat("\n========== АНАЛІЗ ЗАВЕРШЕНО ==========\n")

# =============================================================================
# ЗАПУСКАТИ ПІСЛЯ ТОГО ЯК df_raw ВЖЕ ЗАВАНТАЖЕНО ЧЕРЕЗ file.choose()
# =============================================================================

library(tidyverse)
library(psych)
library(car)
library(corrplot)
library(lm.beta)
library(broom)

# =============================================================================
# КРОК 2. ВІДБІР КОЛОНОК
# =============================================================================

keep_idx <- c(0,1,2,3,4,5,6,7,
              8,9,10,11,
              12,
              13,14,15,16,17,
              18,19,20,21,22,23,24,25,26,
              27,28,29,30,31,32,
              33,35,36,
              37,
              38,39,40,41,42,
              43,44,
              50,51,52,53,
              55,56,57,58,
              60,62,63,64,
              66,67,68,69,70,71,
              72,73,
              75,76,77,78,79,80,
              81,82,83,84,
              85,
              86,87,88,89,
              91,92,93,
              94,95,96,97,98,
              99)

df_clean <- df_raw[, keep_idx + 1]
cat("Робочих колонок після відбору:", ncol(df_clean), "\n")

# =============================================================================
# КРОК 3. ПЕРЕЙМЕНУВАННЯ КОЛОНОК
# =============================================================================

names(df_clean) <- c(
  "timestamp",
  "gender", "age", "education", "income", "welfare", "region", "settlement",
  "dig1_gov", "dig2_data", "dig3_app", "dig4_site",
  "egov_freq",
  "dtrust1_bank", "dtrust2_egov", "dtrust3_bankid", "dtrust4_surveys", "dtrust5_petitions",
  "pol_interest", "vote_history", "vote_motivation", "vote_barrier",
  "social_media_time", "news_source", "news_check", "digital_civic", "digital_civic_eff",
  "cog1_programs", "cog2_unprep_bad", "cog3_info_time", "cog4_impact",
  "cog5_nodiff_R", "cog6_quick_R",
  "cog7_compare", "cog8_read_prog", "cog9_multi_src",
  "time_decision",
  "norm1_duty", "norm2_guilt", "norm3_even_bad", "norm4_collective", "norm5_indif_R",
  "ev_awareness", "ev_concept",
  "ev_security", "ev_transparency", "ev_anonymity", "ev_access",
  "trad_security", "trad_transparency", "trad_anonymity", "trad_access",
  "imp_security", "imp_access", "imp_anonymity", "imp_transparency",
  "evgen1_convenient", "evgen2_others", "evgen3_risks_R", "evgen4_sec_doubt_R",
  "evgen5_trust_ctrl", "evgen6_no_guar_R",
  "info_fraud", "info_digi",
  "src_gov", "src_media", "src_intl", "src_bloggers", "src_friends", "src_telegram",
  "etrust1_count", "etrust2_results", "etrust3_rules", "etrust4_secret",
  "ev_admin_trust",
  "eveff1_less_serious", "eveff2_less_deliber", "eveff3_no_loss", "eveff4_format",
  "ev_conditions", "ev_would_use", "info_ev_reliability",
  "inst_rada", "inst_cabinet", "inst_cec", "inst_local", "inst_courts",
  "cog10_delay"
)
cat("Перейменування завершено.\n")

# =============================================================================
# КРОК 3.5. ОЧИЩЕННЯ vote_history
# =============================================================================

df_clean$vote_clean <- case_when(
  df_clean$vote_history %in% c(
    "Брав/ла участь регулярно",
    "Брав/ла участь регулярно (на більшості проведених виборів)"
  ) ~ "Регулярно",
  df_clean$vote_history %in% c(
    "Брав/ла участь іноді",
    "Брав/ла участь іноді (на деяких з проведених виборів)"
  ) ~ "Іноді",
  df_clean$vote_history == "Ніколи" ~ "Ніколи",
  df_clean$vote_history == "Ще не мав/мала можливості (наприклад, не досяг/ла 18 років під час виборів)" ~ "Не мав/мала можливості",
  TRUE ~ NA_character_
)
df_clean$vote_clean <- factor(df_clean$vote_clean,
                              levels = c("Регулярно", "Іноді", "Ніколи", "Не мав/мала можливості"))
df_clean$has_voted <- df_clean$vote_clean %in% c("Регулярно", "Іноді")

cat("Розподіл vote_clean:\n")
print(table(df_clean$vote_clean, useNA = "always"))

# =============================================================================
# КРОК 3.6. ДВА ДАТАФРЕЙМИ
# =============================================================================

df_norm <- df_clean
df_cog  <- df_clean %>% filter(!is.na(cog1_programs))

cat("df_norm (N=", nrow(df_norm), ") | df_cog (N=", nrow(df_cog), ")\n")

# =============================================================================
# КРОК 4. КОДУВАННЯ
# =============================================================================

code_demographics <- function(df) {
  df$gender_num <- case_when(
    df$gender == "Жіноча"   ~ 1,
    df$gender == "Чоловіча" ~ 2,
    TRUE ~ NA_real_)
  df$gender_f <- factor(df$gender_num, levels=c(1,2), labels=c("Жінка","Чоловік"))
  df$age_num <- case_when(
    df$age == "18-24 роки"  ~ 1,
    df$age == "25-34 роки"  ~ 2,
    df$age == "35-44 роки"  ~ 3,
    df$age == "45-60 років" ~ 4,
    df$age == "60+ років"   ~ 5,
    TRUE ~ NA_real_)
  df$education_num <- case_when(
    df$education == "Середня освіта"              ~ 1,
    df$education == "Професійна освіта / коледж" ~ 2,
    df$education == "Бакалавр"                   ~ 3,
    df$education == "Магістр"                    ~ 4,
    df$education == "Науковий ступінь (PhD)"     ~ 5,
    TRUE ~ NA_real_)
  df$income_num <- case_when(
    df$income == "Менше ніж 100 тис. гривень"     ~ 1,
    df$income == "Від 101 до 200 тис. гривень"    ~ 2,
    df$income == "Від 201 до 400 тис. гривень"    ~ 3,
    df$income == "Від 401 до 600 тис. гривень"    ~ 4,
    df$income == "Від 601 до 800 тис. гривень"    ~ 5,
    df$income == "Від 801 до 1 мільйонна гривень" ~ 6,
    df$income == "Понад 1 мільйон гривень"        ~ 7,
    TRUE ~ NA_real_)
  df$welfare_num <- case_when(
    df$welfare == "Не вистачає на базові потреби"                                ~ 1,
    df$welfare == "Вистачає на харчування та одяг, але дорогі покупки недоступні" ~ 2,
    df$welfare == "Можемо купувати дорогі речі"                                  ~ 3,
    df$welfare == "Живемо заможно"                                               ~ 4,
    TRUE ~ NA_real_)
  df$settlement_num <- case_when(
    df$settlement == "Місто (понад 100 тис. мешканців)" ~ 4,
    df$settlement == "Місто (до 100 тис. мешканців)"    ~ 3,
    df$settlement == "Селище міського типу"              ~ 2,
    df$settlement == "Село"                              ~ 1,
    TRUE ~ NA_real_)
  df$region_f <- as.factor(df$region)
  df$pol_interest_num <- case_when(
    df$pol_interest == "Зовсім не цікавлюся"    ~ 1,
    df$pol_interest == "Радше не цікавлюся"     ~ 2,
    df$pol_interest == "Певною мірою цікавлюся" ~ 3,
    df$pol_interest == "Радше цікавлюся"        ~ 4,
    df$pol_interest == "Дуже цікавлюся"         ~ 5,
    TRUE ~ NA_real_)
  df$ev_would_use_num <- case_when(
    df$ev_would_use == "Так"                          ~ 5,
    df$ev_would_use == "Скоріше так"                 ~ 4,
    df$ev_would_use == "Не знаю / залежить від умов" ~ 3,
    df$ev_would_use == "Скоріше ні"                  ~ 2,
    df$ev_would_use == "Ні"                          ~ 1,
    TRUE ~ NA_real_)
  df$vote_clean <- case_when(
    df$vote_history %in% c("Брав/ла участь регулярно",
                           "Брав/ла участь регулярно (на більшості проведених виборів)") ~ "Регулярно",
    df$vote_history %in% c("Брав/ла участь іноді",
                           "Брав/ла участь іноді (на деяких з проведених виборів)")      ~ "Іноді",
    df$vote_history == "Ніколи"                                     ~ "Ніколи",
    df$vote_history == "Ще не мав/мала можливості (наприклад, не досяг/ла 18 років під час виборів)" ~ "Не мав/мала можливості",
    TRUE ~ NA_character_)
  df$vote_clean <- factor(df$vote_clean,
                          levels = c("Регулярно","Іноді","Ніколи","Не мав/мала можливості"))
  df$has_voted <- df$vote_clean %in% c("Регулярно","Іноді")
  df
}

df_norm <- code_demographics(df_norm)
df_cog  <- code_demographics(df_cog)
cat("Кодування завершено.\n")

# =============================================================================
# КРОК 5. ЧИСЛОВІ ШКАЛИ
# =============================================================================

likert_cols <- c(
  "dig1_gov","dig2_data","dig3_app","dig4_site",
  "dtrust1_bank","dtrust2_egov","dtrust3_bankid","dtrust4_surveys","dtrust5_petitions",
  "cog1_programs","cog2_unprep_bad","cog3_info_time","cog4_impact",
  "cog5_nodiff_R","cog6_quick_R",
  "cog7_compare","cog8_read_prog","cog9_multi_src","cog10_delay",
  "norm1_duty","norm2_guilt","norm3_even_bad","norm4_collective","norm5_indif_R",
  "ev_security","ev_transparency","ev_anonymity","ev_access",
  "trad_security","trad_transparency","trad_anonymity","trad_access",
  "imp_security","imp_access","imp_anonymity","imp_transparency",
  "evgen1_convenient","evgen2_others","evgen3_risks_R","evgen4_sec_doubt_R",
  "evgen5_trust_ctrl","evgen6_no_guar_R",
  "src_gov","src_media","src_intl","src_bloggers","src_friends","src_telegram",
  "etrust1_count","etrust2_results","etrust3_rules","etrust4_secret",
  "eveff1_less_serious","eveff2_less_deliber","eveff3_no_loss","eveff4_format",
  "inst_rada","inst_cabinet","inst_cec","inst_local","inst_courts"
)

df_norm[likert_cols] <- lapply(df_norm[likert_cols], as.numeric)
df_cog[likert_cols]  <- lapply(df_cog[likert_cols],  as.numeric)

# =============================================================================
# КРОК 6. REVERSE CODING
# =============================================================================

reverse_code <- function(df) {
  df$cog5_nodiff   <- 6 - df$cog5_nodiff_R
  df$cog6_quick    <- 6 - df$cog6_quick_R
  df$norm5_indif   <- 6 - df$norm5_indif_R
  df$evgen3_risks  <- 6 - df$evgen3_risks_R
  df$evgen4_sec    <- 6 - df$evgen4_sec_doubt_R
  df$evgen6_noguar <- 6 - df$evgen6_no_guar_R
  df
}

df_norm <- reverse_code(df_norm)
df_cog  <- reverse_code(df_cog)

# =============================================================================
# КРОК 7. ІНДЕКСИ + АЛЬФА
# =============================================================================

cat("\n========== АЛЬФА КРОНБАХА ==========\n")

cog_items <- df_cog %>%
  select(cog1_programs, cog2_unprep_bad, cog3_info_time, cog4_impact,
         cog5_nodiff, cog6_quick,
         cog7_compare, cog8_read_prog, cog9_multi_src, cog10_delay)
a_cog <- alpha(cog_items)
cat("Когнітивний ERR (deliberation, N=523), alpha =", round(a_cog$total$raw_alpha, 3), "\n")
df_cog$ERR_cognitive <- rowMeans(cog_items, na.rm = TRUE)

norm_items_norm <- df_norm %>%
  select(norm1_duty, norm2_guilt, norm3_even_bad, norm4_collective, norm5_indif)
a_norm <- alpha(norm_items_norm)
cat("Нормативний ERR (civic duty, N=554), alpha =", round(a_norm$total$raw_alpha, 3), "\n")
df_norm$ERR_normative <- rowMeans(norm_items_norm, na.rm = TRUE)

norm_items_cog <- df_cog %>%
  select(norm1_duty, norm2_guilt, norm3_even_bad, norm4_collective, norm5_indif)
df_cog$ERR_normative <- rowMeans(norm_items_cog, na.rm = TRUE)

all_err_items <- df_cog %>%
  select(cog1_programs, cog2_unprep_bad, cog3_info_time, cog4_impact,
         cog5_nodiff, cog6_quick,
         cog7_compare, cog8_read_prog, cog9_multi_src, cog10_delay,
         norm1_duty, norm2_guilt, norm3_even_bad, norm4_collective, norm5_indif)
a_err <- alpha(all_err_items)
cat("Зведений ERR (N=523), alpha =", round(a_err$total$raw_alpha, 3), "\n")
df_cog$ERR_index <- rowMeans(all_err_items, na.rm = TRUE)

for (dfname in c("df_norm", "df_cog")) {
  d <- get(dfname)
  d$digital_literacy <- rowMeans(
    d %>% select(dig1_gov, dig2_data, dig3_app, dig4_site), na.rm=TRUE)
  d$digital_trust <- rowMeans(
    d %>% select(dtrust1_bank, dtrust2_egov, dtrust3_bankid,
                 dtrust4_surveys, dtrust5_petitions), na.rm=TRUE)
  d$electoral_trust <- rowMeans(
    d %>% select(etrust1_count, etrust2_results, etrust3_rules, etrust4_secret), na.rm=TRUE)
  d$institutional_trust <- rowMeans(
    d %>% select(inst_rada, inst_cabinet, inst_cec, inst_local, inst_courts), na.rm=TRUE)
  d$delta_security     <- d$ev_security     - d$trad_security
  d$delta_transparency <- d$ev_transparency - d$trad_transparency
  d$delta_anonymity    <- d$ev_anonymity    - d$trad_anonymity
  d$delta_access       <- d$ev_access       - d$trad_access
  assign(dfname, d)
}
cat("Індекси побудовано.\n")

# =============================================================================
# КРОК 8. ОПИСОВА СТАТИСТИКА
# =============================================================================

cat("\n========== ОПИСОВА СТАТИСТИКА ==========\n")
index_vars <- c("ERR_index","ERR_cognitive","ERR_normative",
                "digital_literacy","digital_trust","electoral_trust",
                "institutional_trust",
                "ev_security","ev_transparency","ev_anonymity","ev_access")
print(describe(df_cog[index_vars])[, c("n","mean","sd","min","max","skew","kurtosis")])

# =============================================================================
# КРОК 9. КОРЕЛЯЦІЇ
# =============================================================================

cat("\n========== КОРЕЛЯЦІЙНА МАТРИЦЯ ==========\n")
iv_cor <- df_cog %>%
  select(ev_security, ev_transparency, ev_anonymity, ev_access,
         digital_literacy, digital_trust, electoral_trust,
         pol_interest_num, age_num, education_num, income_num, welfare_num)
cor_mat <- cor(iv_cor, use = "complete.obs", method = "pearson")
print(round(cor_mat, 2))

# =============================================================================
# КРОК 10. РЕГРЕСІЙНІ МОДЕЛІ
# =============================================================================

cat("\n========== РЕГРЕСІЙНІ МОДЕЛІ ==========\n")

controls <- "gender_num + age_num + education_num + income_num + welfare_num +
             pol_interest_num + digital_literacy + electoral_trust"

f1 <- as.formula(paste("ERR_index ~",
                       "ev_security + ev_transparency + ev_anonymity + ev_access +", controls))
m1 <- lm(f1, data = df_cog)
cat("Модель 1 — Зведений ERR (N=523):\n"); print(summary(m1))

f2 <- as.formula(paste("ERR_cognitive ~",
                       "ev_security + ev_transparency + ev_anonymity + ev_access +", controls))
m2 <- lm(f2, data = df_cog)
cat("Модель 2 — Когнітивний ERR (N=523):\n"); print(summary(m2))

f3 <- as.formula(paste("ERR_normative ~",
                       "ev_security + ev_transparency + ev_anonymity + ev_access +", controls))
m3 <- lm(f3, data = df_norm)
cat("Модель 3 — Нормативний ERR (N=554):\n"); print(summary(m3))

# =============================================================================
# КРОК 11. СУБГРУПОВІ АНАЛІЗИ
# =============================================================================

cat("\n========== СУБГРУПОВІ АНАЛІЗИ ==========\n")

cat("T-тест за статтю:\n")
print(t.test(ERR_index ~ gender_f, data = df_cog %>% filter(!is.na(gender_f))))

cat("\nANOVA за освітою:\n")
aov_edu <- aov(ERR_index ~ as.factor(education_num), data = df_cog)
print(summary(aov_edu))
print(TukeyHSD(aov_edu))

cat("\nKореляції Спірмена:\n")
for (nm in c("education_num","age_num","income_num","pol_interest_num")) {
  r <- cor.test(df_cog$ERR_index, df_cog[[nm]], method="spearman")
  cat(sprintf("  ERR ~ %s: rho=%.3f, p=%.4f\n", nm, r$estimate, r$p.value))
}

# =============================================================================
# КРОК 12. ГОТОВНІСТЬ ДО Е-ГОЛОСУВАННЯ
# =============================================================================

cat("\n========== ГОТОВНІСТЬ ДО Е-ГОЛОСУВАННЯ ==========\n")
print(table(df_norm$ev_would_use, useNA="no"))

# =============================================================================
# КРОК 15. ЗБЕРЕЖЕННЯ
# =============================================================================

write.csv(df_cog, "klus_clean_v2.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("Збережено: klus_clean_v2.csv\n")

# =============================================================================
# БЛОК: АНАЛІЗ НЕВИБОРЦІВ
# =============================================================================

cat("\n========== АНАЛІЗ НЕВИБОРЦІВ ==========\n")

exists("df_norm")
exists("df_cog")
exists("df_clean")

keep_idx <- c(0,1,2,3,4,5,6,7,
              8,9,10,11,
              12,
              13,14,15,16,17,
              18,19,20,21,22,23,24,25,26,
              27,28,29,30,31,32,
              33,35,36,
              37,
              38,39,40,41,42,
              43,44,
              50,51,52,53,
              55,56,57,58,
              60,62,63,64,
              66,67,68,69,70,71,
              72,73,
              75,76,77,78,79,80,
              81,82,83,84,
              85,
              86,87,88,89,
              91,92,93,
              94,95,96,97,98,
              99)

df_clean <- df_raw[, keep_idx + 1]
cat("Колонок:", ncol(df_clean), "\n")
cat("df_clean існує:", exists("df_clean"), "\n")
names(df_clean) <- c(
# 1. Розподіл груп
  # =============================================================================
  # ЗАПУСКАТИ ПІСЛЯ ТОГО ЯК df_clean ВЖЕ СТВОРЕНО (88 колонок)
  # =============================================================================
  
  library(tidyverse)
  library(psych)
  library(car)
  library(lm.beta)
  library(broom)
  
  # =============================================================================
  # КРОК 3. ПЕРЕЙМЕНУВАННЯ
  # =============================================================================
  
  names(df_clean) <- c(
    "timestamp",
    "gender", "age", "education", "income", "welfare", "region", "settlement",
    "dig1_gov", "dig2_data", "dig3_app", "dig4_site",
    "egov_freq",
    "dtrust1_bank", "dtrust2_egov", "dtrust3_bankid", "dtrust4_surveys", "dtrust5_petitions",
    "pol_interest", "vote_history", "vote_motivation", "vote_barrier",
    "social_media_time", "news_source", "news_check", "digital_civic", "digital_civic_eff",
    "cog1_programs", "cog2_unprep_bad", "cog3_info_time", "cog4_impact",
    "cog5_nodiff_R", "cog6_quick_R",
    "cog7_compare", "cog8_read_prog", "cog9_multi_src",
    "time_decision",
    "norm1_duty", "norm2_guilt", "norm3_even_bad", "norm4_collective", "norm5_indif_R",
    "ev_awareness", "ev_concept",
    "ev_security", "ev_transparency", "ev_anonymity", "ev_access",
    "trad_security", "trad_transparency", "trad_anonymity", "trad_access",
    "imp_security", "imp_access", "imp_anonymity", "imp_transparency",
    "evgen1_convenient", "evgen2_others", "evgen3_risks_R", "evgen4_sec_doubt_R",
    "evgen5_trust_ctrl", "evgen6_no_guar_R",
    "info_fraud", "info_digi",
    "src_gov", "src_media", "src_intl", "src_bloggers", "src_friends", "src_telegram",
    "etrust1_count", "etrust2_results", "etrust3_rules", "etrust4_secret",
    "ev_admin_trust",
    "eveff1_less_serious", "eveff2_less_deliber", "eveff3_no_loss", "eveff4_format",
    "ev_conditions", "ev_would_use", "info_ev_reliability",
    "inst_rada", "inst_cabinet", "inst_cec", "inst_local", "inst_courts",
    "cog10_delay"
  )
  cat("Перейменування завершено.\n")
  
  # =============================================================================
  # КРОК 3.5. ОЧИЩЕННЯ vote_history + ДВА ДАТАФРЕЙМИ
  # =============================================================================
  
  df_clean$vote_clean <- case_when(
    df_clean$vote_history %in% c(
      "Брав/ла участь регулярно",
      "Брав/ла участь регулярно (на більшості проведених виборів)") ~ "Регулярно",
    df_clean$vote_history %in% c(
      "Брав/ла участь іноді",
      "Брав/ла участь іноді (на деяких з проведених виборів)")      ~ "Іноді",
    df_clean$vote_history == "Ніколи"                               ~ "Ніколи",
    df_clean$vote_history == "Ще не мав/мала можливості (наприклад, не досяг/ла 18 років під час виборів)" ~ "Не мав/мала можливості",
    TRUE ~ NA_character_
  )
  df_clean$vote_clean <- factor(df_clean$vote_clean,
                                levels = c("Регулярно","Іноді","Ніколи","Не мав/мала можливості"))
  df_clean$has_voted <- df_clean$vote_clean %in% c("Регулярно","Іноді")
  
  cat("Розподіл vote_clean:\n")
  print(table(df_clean$vote_clean, useNA="always"))
  
  df_norm <- df_clean
  df_cog  <- df_clean %>% filter(!is.na(cog1_programs))
  cat("df_norm N=", nrow(df_norm), "| df_cog N=", nrow(df_cog), "\n")
  
  # =============================================================================
  # КРОК 4. КОДУВАННЯ
  # =============================================================================
  
  code_demographics <- function(df) {
    df$gender_num <- case_when(
      df$gender == "Жіноча"   ~ 1,
      df$gender == "Чоловіча" ~ 2,
      TRUE ~ NA_real_)
    df$gender_f <- factor(df$gender_num, levels=c(1,2), labels=c("Жінка","Чоловік"))
    df$age_num <- case_when(
      df$age == "18-24 роки"  ~ 1,
      df$age == "25-34 роки"  ~ 2,
      df$age == "35-44 роки"  ~ 3,
      df$age == "45-60 років" ~ 4,
      df$age == "60+ років"   ~ 5,
      TRUE ~ NA_real_)
    df$education_num <- case_when(
      df$education == "Середня освіта"              ~ 1,
      df$education == "Професійна освіта / коледж" ~ 2,
      df$education == "Бакалавр"                   ~ 3,
      df$education == "Магістр"                    ~ 4,
      df$education == "Науковий ступінь (PhD)"     ~ 5,
      TRUE ~ NA_real_)
    df$income_num <- case_when(
      df$income == "Менше ніж 100 тис. гривень"     ~ 1,
      df$income == "Від 101 до 200 тис. гривень"    ~ 2,
      df$income == "Від 201 до 400 тис. гривень"    ~ 3,
      df$income == "Від 401 до 600 тис. гривень"    ~ 4,
      df$income == "Від 601 до 800 тис. гривень"    ~ 5,
      df$income == "Від 801 до 1 мільйонна гривень" ~ 6,
      df$income == "Понад 1 мільйон гривень"        ~ 7,
      TRUE ~ NA_real_)
    df$welfare_num <- case_when(
      df$welfare == "Не вистачає на базові потреби"                                  ~ 1,
      df$welfare == "Вистачає на харчування та одяг, але дорогі покупки недоступні"  ~ 2,
      df$welfare == "Можемо купувати дорогі речі"                                    ~ 3,
      df$welfare == "Живемо заможно"                                                 ~ 4,
      TRUE ~ NA_real_)
    df$pol_interest_num <- case_when(
      df$pol_interest == "Зовсім не цікавлюся"    ~ 1,
      df$pol_interest == "Радше не цікавлюся"     ~ 2,
      df$pol_interest == "Певною мірою цікавлюся" ~ 3,
      df$pol_interest == "Радше цікавлюся"        ~ 4,
      df$pol_interest == "Дуже цікавлюся"         ~ 5,
      TRUE ~ NA_real_)
    df$ev_would_use_num <- case_when(
      df$ev_would_use == "Так"                          ~ 5,
      df$ev_would_use == "Скоріше так"                 ~ 4,
      df$ev_would_use == "Не знаю / залежить від умов" ~ 3,
      df$ev_would_use == "Скоріше ні"                  ~ 2,
      df$ev_would_use == "Ні"                          ~ 1,
      TRUE ~ NA_real_)
    df$has_voted_num <- as.numeric(df$has_voted)
    df
  }
  
  df_norm <- code_demographics(df_norm)
  df_cog  <- code_demographics(df_cog)
  cat("Кодування завершено.\n")
  
  # =============================================================================
  # КРОК 5-6. ШКАЛИ ЛІКЕРТА + REVERSE CODING
  # =============================================================================
  
  likert_cols <- c(
    "dig1_gov","dig2_data","dig3_app","dig4_site",
    "dtrust1_bank","dtrust2_egov","dtrust3_bankid","dtrust4_surveys","dtrust5_petitions",
    "cog1_programs","cog2_unprep_bad","cog3_info_time","cog4_impact",
    "cog5_nodiff_R","cog6_quick_R",
    "cog7_compare","cog8_read_prog","cog9_multi_src","cog10_delay",
    "norm1_duty","norm2_guilt","norm3_even_bad","norm4_collective","norm5_indif_R",
    "ev_security","ev_transparency","ev_anonymity","ev_access",
    "trad_security","trad_transparency","trad_anonymity","trad_access",
    "evgen1_convenient","evgen2_others","evgen3_risks_R","evgen4_sec_doubt_R",
    "evgen5_trust_ctrl","evgen6_no_guar_R",
    "etrust1_count","etrust2_results","etrust3_rules","etrust4_secret",
    "inst_rada","inst_cabinet","inst_cec","inst_local","inst_courts"
  )
  
  df_norm[likert_cols] <- lapply(df_norm[likert_cols], as.numeric)
  df_cog[likert_cols]  <- lapply(df_cog[likert_cols],  as.numeric)
  
  reverse_code <- function(df) {
    df$cog5_nodiff   <- 6 - df$cog5_nodiff_R
    df$cog6_quick    <- 6 - df$cog6_quick_R
    df$norm5_indif   <- 6 - df$norm5_indif_R
    df$evgen3_risks  <- 6 - df$evgen3_risks_R
    df$evgen4_sec    <- 6 - df$evgen4_sec_doubt_R
    df$evgen6_noguar <- 6 - df$evgen6_no_guar_R
    df
  }
  df_norm <- reverse_code(df_norm)
  df_cog  <- reverse_code(df_cog)
  cat("Шкали і reverse coding готові.\n")
  
  # =============================================================================
  # КРОК 7. ІНДЕКСИ
  # =============================================================================
  
  cat("\n========== АЛЬФА КРОНБАХА ==========\n")
  
  cog_items <- df_cog %>%
    select(cog1_programs,cog2_unprep_bad,cog3_info_time,cog4_impact,
           cog5_nodiff,cog6_quick,cog7_compare,cog8_read_prog,cog9_multi_src,cog10_delay)
  cat("Когнітивний ERR, alpha=", round(alpha(cog_items)$total$raw_alpha,3),"\n")
  df_cog$ERR_cognitive <- rowMeans(cog_items, na.rm=TRUE)
  
  norm_items_554 <- df_norm %>%
    select(norm1_duty,norm2_guilt,norm3_even_bad,norm4_collective,norm5_indif)
  cat("Нормативний ERR, alpha=", round(alpha(norm_items_554)$total$raw_alpha,3),"\n")
  df_norm$ERR_normative <- rowMeans(norm_items_554, na.rm=TRUE)
  
  norm_items_523 <- df_cog %>%
    select(norm1_duty,norm2_guilt,norm3_even_bad,norm4_collective,norm5_indif)
  df_cog$ERR_normative <- rowMeans(norm_items_523, na.rm=TRUE)
  
  all_err <- df_cog %>%
    select(cog1_programs,cog2_unprep_bad,cog3_info_time,cog4_impact,
           cog5_nodiff,cog6_quick,cog7_compare,cog8_read_prog,cog9_multi_src,cog10_delay,
           norm1_duty,norm2_guilt,norm3_even_bad,norm4_collective,norm5_indif)
  cat("Зведений ERR, alpha=", round(alpha(all_err)$total$raw_alpha,3),"\n")
  df_cog$ERR_index <- rowMeans(all_err, na.rm=TRUE)
  
  for (dfname in c("df_norm","df_cog")) {
    d <- get(dfname)
    d$digital_literacy    <- rowMeans(d %>% select(dig1_gov,dig2_data,dig3_app,dig4_site), na.rm=TRUE)
    d$digital_trust       <- rowMeans(d %>% select(dtrust1_bank,dtrust2_egov,dtrust3_bankid,dtrust4_surveys,dtrust5_petitions), na.rm=TRUE)
    d$electoral_trust     <- rowMeans(d %>% select(etrust1_count,etrust2_results,etrust3_rules,etrust4_secret), na.rm=TRUE)
    d$institutional_trust <- rowMeans(d %>% select(inst_rada,inst_cabinet,inst_cec,inst_local,inst_courts), na.rm=TRUE)
    d$delta_security      <- d$ev_security     - d$trad_security
    d$delta_transparency  <- d$ev_transparency - d$trad_transparency
    d$delta_anonymity     <- d$ev_anonymity    - d$trad_anonymity
    d$delta_access        <- d$ev_access       - d$trad_access
    assign(dfname, d)
  }
  cat("Усі індекси побудовано.\n")
  
  # =============================================================================
  # КРОК 8. ОПИСОВА СТАТИСТИКА
  # =============================================================================
  
  cat("\n========== ОПИСОВА СТАТИСТИКА ==========\n")
  print(describe(df_cog %>% select(ERR_index,ERR_cognitive,ERR_normative,
                                   digital_literacy,digital_trust,electoral_trust,
                                   ev_security,ev_transparency,ev_anonymity,ev_access))[,
                                                                                        c("n","mean","sd","min","max")])
  
  # =============================================================================
  # КРОК 9-10. РЕГРЕСІЇ
  # =============================================================================
  
  cat("\n========== РЕГРЕСІЇ ==========\n")
  
  controls <- "gender_num + age_num + education_num + income_num + welfare_num +
             pol_interest_num + digital_literacy + electoral_trust"
  
  m1 <- lm(as.formula(paste("ERR_index ~ ev_security + ev_transparency + ev_anonymity + ev_access +", controls)), data=df_cog)
  m2 <- lm(as.formula(paste("ERR_cognitive ~ ev_security + ev_transparency + ev_anonymity + ev_access +", controls)), data=df_cog)
  m3 <- lm(as.formula(paste("ERR_normative ~ ev_security + ev_transparency + ev_anonymity + ev_access +", controls)), data=df_norm)
  
  cat("Модель 1 — Зведений ERR (N=523):\n"); print(summary(m1))
  cat("Модель 2 — Когнітивний ERR (N=523):\n"); print(summary(m2))
  cat("Модель 3 — Нормативний ERR (N=554):\n"); print(summary(m3))
  
  # =============================================================================
  # АНАЛІЗ НЕВИБОРЦІВ
  # =============================================================================
  
  cat("\n========== АНАЛІЗ НЕВИБОРЦІВ ==========\n")
  
  nonvoters_norm <- df_norm %>%
    filter(vote_clean %in% c("Ніколи","Не мав/мала можливості"))
  cat("Невиборців:", nrow(nonvoters_norm), "\n")
  
  cat("\nERR_normative за групами (N=554):\n")
  print(df_norm %>%
          filter(!is.na(vote_clean)) %>%
          group_by(vote_clean) %>%
          summarise(n=n(),
                    M  = round(mean(ERR_normative, na.rm=TRUE),3),
                    SD = round(sd(ERR_normative,   na.rm=TRUE),3)))
  
  aov_norm <- aov(ERR_normative ~ vote_clean, data=df_norm)
  print(summary(aov_norm))
  print(TukeyHSD(aov_norm))
  
  df_norm <- df_norm %>%
    mutate(nonvoter = vote_clean %in% c("Ніколи","Не мав/мала можливості"))
  
  cat("\nСприйняття е-голосування — виборці vs невиборці:\n")
  print(df_norm %>%
          filter(!is.na(vote_clean)) %>%
          mutate(label = ifelse(nonvoter,"Невиборці","Виборці")) %>%
          group_by(label) %>%
          summarise(n=n(),
                    Безпека     = round(mean(ev_security,     na.rm=TRUE),3),
                    Прозорість  = round(mean(ev_transparency, na.rm=TRUE),3),
                    Анонімність = round(mean(ev_anonymity,    na.rm=TRUE),3),
                    Доступність = round(mean(ev_access,       na.rm=TRUE),3)))
  
  cat("\nKruskal-Wallis:\n")
  for (var in c("ev_security","ev_transparency","ev_anonymity","ev_access")) {
    kt <- kruskal.test(df_norm[[var]] ~ df_norm$vote_clean)
    cat(sprintf("  %s: chi2=%.3f, p=%.4f\n", var, kt$statistic, kt$p.value))
  }
  
  cat("\nГотовність невиборців до е-голосування:\n")
  print(nonvoters_norm %>%
          count(ev_would_use) %>%
          mutate(pct=round(n/sum(n)*100,1)) %>%
          arrange(desc(n)))
  
  cat("\nГотовність виборців:\n")
  print(df_norm %>%
          filter(has_voted) %>%
          count(ev_would_use) %>%
          mutate(pct=round(n/sum(n)*100,1)) %>%
          arrange(desc(n)))
  
  cat("\nМодель 1 + has_voted як контрольна:\n")
  m1_vote <- lm(as.formula(paste(
    "ERR_index ~ ev_security + ev_transparency + ev_anonymity + ev_access +
   has_voted_num +", controls)), data=df_cog)
  print(summary(m1_vote))
  print(anova(m1, m1_vote))
  
  coef_v <- tidy(m1_vote) %>% filter(term=="has_voted_num")
  cat(sprintf("has_voted: beta=%.3f, SE=%.3f, p=%.4f\n",
              coef_v$estimate, coef_v$std.error, coef_v$p.value))
  
  cat("\nРегресія ERR_normative тільки для невиборців (N=",nrow(nonvoters_norm),"):\n")
  m_nv <- lm(ERR_normative ~
               ev_security + ev_transparency + ev_anonymity + ev_access +
               gender_num + age_num + education_num + income_num +
               pol_interest_num + digital_literacy,
             data=nonvoters_norm)
  print(summary(m_nv))
  
  cat("\n========== ГОТОВО ==========\n")
  
  cat("Розподіл vote_clean (df_norm, N=554):\n")
print(table(df_norm$vote_clean, useNA="always"))

nonvoters_norm <- df_norm %>%
  filter(vote_clean %in% c("Ніколи", "Не мав/мала можливості"))
cat("Невиборців:", nrow(nonvoters_norm), "\n")

# 2. ERR_normative за групами
cat("\n--- ERR_normative за групами (N=554) ---\n")
print(df_norm %>%
        filter(!is.na(vote_clean)) %>%
        group_by(vote_clean) %>%
        summarise(n=n(),
                  M  = round(mean(ERR_normative, na.rm=TRUE), 3),
                  SD = round(sd(ERR_normative,   na.rm=TRUE), 3)))

aov_norm <- aov(ERR_normative ~ vote_clean, data=df_norm)
print(summary(aov_norm))
print(TukeyHSD(aov_norm))

# 3. Сприйняття е-голосування — невиборці vs виборці
df_norm <- df_norm %>%
  mutate(nonvoter = vote_clean %in% c("Ніколи", "Не мав/мала можливості"))

cat("\n--- Сприйняття е-голосування (N=554) ---\n")
print(df_norm %>%
        filter(!is.na(vote_clean)) %>%
        mutate(voter_label = ifelse(nonvoter, "Невиборці", "Виборці")) %>%
        group_by(voter_label) %>%
        summarise(n=n(),
                  Безпека     = round(mean(ev_security,     na.rm=TRUE), 3),
                  Прозорість  = round(mean(ev_transparency, na.rm=TRUE), 3),
                  Анонімність = round(mean(ev_anonymity,    na.rm=TRUE), 3),
                  Доступність = round(mean(ev_access,       na.rm=TRUE), 3)))

cat("\nKruskal-Wallis:\n")
for (var in c("ev_security","ev_transparency","ev_anonymity","ev_access")) {
  kt <- kruskal.test(df_norm[[var]] ~ df_norm$vote_clean)
  cat(sprintf("  %s: chi2=%.3f, p=%.4f\n", var, kt$statistic, kt$p.value))
}

# 4. Готовність невиборців
cat("\n--- Готовність невиборців ---\n")
print(nonvoters_norm %>%
        count(ev_would_use) %>%
        mutate(pct=round(n/sum(n)*100, 1)) %>%
        arrange(desc(n)))

cat("\n--- Готовність виборців ---\n")
print(df_norm %>%
        filter(has_voted) %>%
        count(ev_would_use) %>%
        mutate(pct=round(n/sum(n)*100, 1)) %>%
        arrange(desc(n)))

# 5. has_voted як контрольна змінна
cat("\n--- Модель 1 + has_voted ---\n")
df_cog <- df_cog %>%
  mutate(has_voted_num = as.numeric(has_voted))

m1_with_vote <- lm(ERR_index ~
                     ev_security + ev_transparency + ev_anonymity + ev_access +
                     has_voted_num +
                     gender_num + age_num + education_num + income_num + welfare_num +
                     pol_interest_num + digital_literacy + electoral_trust,
                   data=df_cog)
print(summary(m1_with_vote))

cat("\nПорівняння моделей:\n")
print(anova(m1, m1_with_vote))

coef_vote <- tidy(m1_with_vote) %>% filter(term=="has_voted_num")
cat(sprintf("has_voted: beta=%.3f, SE=%.3f, p=%.4f\n",
            coef_vote$estimate, coef_vote$std.error, coef_vote$p.value))

# 6. Регресія лише для невиборців
cat("\n--- Регресія ERR_normative на невиборцях (N=",
    nrow(nonvoters_norm), ") ---\n")
m_nonvoters <- lm(ERR_normative ~
                    ev_security + ev_transparency + ev_anonymity + ev_access +
                    gender_num + age_num + education_num + income_num +
                    pol_interest_num + digital_literacy,
                  data=nonvoters_norm)
print(summary(m_nonvoters))

cat("\n========== АНАЛІЗ ЗАВЕРШЕНО ==========\n")
