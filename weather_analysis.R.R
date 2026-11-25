# install.packages(c("corrplot", "ggplot2")) 
library(corrplot) 
library(ggplot2) 

CLR_BLUE   <- "#2C7BB6"   # синій  — фактичні / навчальна / холодний 
CLR_ORANGE <- "#E87722"   # помаранч — прогноз / тестова / теплий 
CLR_GREEN  <- "#2CA02C"   # зелений — перехідний весна/осінь 
CLR_BLACK  <- "#1A1A1A"   # чорний  — лінії, текст 
CLR_GRID   <- "#EEEEEE"   # сітка 

theme_diploma <- theme_minimal(base_size = 12) + 
  theme( 
    plot.title       = element_text(face = "bold", size = 13, colour = CLR_BLACK, hjust = 0.5), 
    plot.subtitle    = element_text(size = 9.5, colour = "#555555"), 
    # Підписи осей — збільшені та жирні (читаються з будь-якої відстані) 
    axis.title       = element_text(face = "bold", size = 13, colour = CLR_BLACK), 
    axis.text        = element_text(face = "bold", size = 11, colour = "#333333"), 
    legend.text      = element_text(face = "bold", size = 11, colour = CLR_BLACK), 
    legend.position  = "top", 
    panel.grid.major = element_line(colour = CLR_GRID), 
    panel.grid.minor = element_blank(), 
    plot.background  = element_rect(fill = "white", colour = NA), 
    panel.background = element_rect(fill = "white", colour = NA) 
  ) 

# РОЗДІЛ 1. Завантаження даних 

data_raw <- read.csv("weather.csv", fileEncoding = "UTF-8-BOM")

# Якщо BOM все одно проскочив у першу колонку — прибираємо:
if (names(data_raw)[1] != "date") names(data_raw)[1] <- "date"

# Дати у CSV мають формат "2024-12-31 00:00:00" (з часом).
data_raw$date <- as.Date(data_raw$date, format = "%Y-%m-%d %H:%M:%S")


cat("\n Пропущені значення (до очищення) \n") 
na_counts <- colSums(is.na(data_raw)) 
na_pct    <- round(na_counts / nrow(data_raw) * 100, 1) 
print(data.frame(NA_кількість = na_counts, NA_відсоток = na_pct)) 

# РОЗДІЛ 2. Очищення 

data_raw <- data_raw[, !(names(data_raw) %in% c("snow", "wdir", "wpgt", "tsun"))] 

# Строгий фільтр за датою: від 2021-01-01 до 2024-12-31 включно. 
# Порівнюємо об'єкти Date напряму — точніше ніж format()+%Y. 
ANALYSIS_FROM <- as.Date("2021-01-01") 
ANALYSIS_TO   <- as.Date("2024-12-31") 
data <- data_raw[data_raw$date >= ANALYSIS_FROM & data_raw$date <= ANALYSIS_TO, ] 
data <- na.omit(data) 

cat("\n=== Робоча вибірка (2021–2024) ===\n") 
cat(sprintf("Спостережень: %d  |  %s — %s\n", 
            nrow(data), min(data$date), max(data$date))) 
print(table(format(data$date, "%Y"))) 
summary(data) 

# РОЗДІЛ 3. Розподіли та часовий ряд 

# 3.1 Розподіл середньої температури 

par(bg = "white", col.main = CLR_BLACK, col.lab = CLR_BLACK, 
    col.axis = "#333333", fg = CLR_BLACK, 
    font.lab = 2,          # жирні підписи осей (xlab / ylab) 
    mar = c(5, 5, 4, 2)) 

hist(data$tavg, 
     main = "Розподіл середньої температури 2021–2024", 
     xlab = "Температура, °C", ylab = "Частота", 
     col = CLR_BLUE, border = "white", 
     xlim = c(-20, 35), 
     xaxt = "n",                        # вимикаємо авто-вісь X 
     cex.lab = 1.3, cex.axis = 1.15, cex.main = 1.3) 
# Явні мітки: -20, -10, 0, 10, 20, 30, 35 


axis(1, at = seq(-20, 35, by = 10), 
     cex.axis = 1.15, col.axis = "#333333", font = 2) 

# 3.2 Розподіл опадів 

par(mar = c(5, 5, 4, 2), font.lab = 2, col.lab = CLR_BLACK, col.axis = "#333333") 
hist(data$prcp, 
     main = "Розподіл опадів 2021–2024", 
     xlab = "Опади, мм", ylab = "Частота", 
     col = CLR_BLUE, border = "white", 
     xlim = c(0, 20), 
     cex.lab = 1.3, cex.axis = 1.15, cex.main = 1.3) 

# 3.3 Розподіл швидкості вітру  

par(mar = c(5, 5, 4, 2), font.lab = 2, col.lab = CLR_BLACK, col.axis = "#333333") 
hist(data$wspd, 
     main = "Розподіл швидкості вітру 2021–2024", 
     xlab = "Швидкість вітру, км/год", ylab = "Частота", 
     col = CLR_BLUE, border = "white", 
     xlim = c(0, 30), 
     cex.lab = 1.3, cex.axis = 1.15, cex.main = 1.3) 

# 3.4 Розподіл атмосферного тиску 

par(mar = c(5, 5, 4, 2), font.lab = 2, col.lab = CLR_BLACK, col.axis = "#333333") 
hist(data$pres, 
     main = "Розподіл атмосферного тиску 2021–2024", 
     xlab = "Тиск, гПа", ylab = "Частота", 
     col = CLR_BLUE, border = "white", 
     xlim = c(990, 1050), 
     cex.lab = 1.3, cex.axis = 1.15, cex.main = 1.3) 

# 3.5 Динаміка температури 2021-01-01 по 2024-12-31  
date_from <- ANALYSIS_FROM 
date_to   <- ANALYSIS_TO 
data_plot <- data[data$date >= date_from & data$date <= date_to, ] 

x_ticks  <- as.Date(c("2021-01-01","2022-01-01","2023-01-01","2024-01-01")) 


x_labels <- c("2021","2022","2023","2024") 

par(mar = c(5, 5, 4, 2), font.lab = 2, col.lab = CLR_BLACK, col.axis = "#333333") 
plot(data_plot$date, data_plot$tavg, 
     type = "l", lwd = 1.5, col = CLR_BLUE, 
     main = "Динаміка середньої температури 2021–2024", 
     xlab = "Дата", ylab = "Середня температура, °C", 
     xlim = c(date_from, date_to), 
     xaxs = "i",    
     yaxs = "i",    
     xaxt = "n",    
     cex.lab = 1.3, cex.axis = 1.15, cex.main = 1.3, 
     panel.first = grid(col = CLR_GRID, lty = 1)) 

axis.Date(1, at = x_ticks, labels = x_labels, 
          cex.axis = 1.15, col.axis = "#333333", font = 2) 

# РОЗДІЛ 4. Кластеризація 

cluster_data        <- data[, c("tavg","tmin","tmax","prcp","wspd","pres")] 
cluster_data_scaled <- scale(cluster_data) 

# 4.1 Метод ліктя  
wss <- numeric(10) 
for (k in 1:10) { 
  set.seed(123) 
  wss[k] <- kmeans(cluster_data_scaled, centers = k, nstart = 25)$tot.withinss 
} 

par(mar = c(5, 5, 4, 2), font.lab = 2, col.lab = CLR_BLACK, col.axis = "#333333") 
plot(1:10, wss, 
     type = "b", pch = 19, lwd = 1.5, col = CLR_BLUE, 
     main = "Метод ліктя: вибір кількості кластерів", 
     xlab = "Кількість кластерів k", 
     ylab = "Сума внутрішньокластерних відстаней", 
     cex.lab = 1.3, cex.axis = 1.15, cex.main = 1.3, 
     panel.first = grid(col = CLR_GRID, lty = 1)) 
abline(v = 3, lty = 2, col = CLR_ORANGE, lwd = 2) 
text(3.2, max(wss) * 0.97, "k = 3", col = CLR_ORANGE, cex = 0.9, font = 2) 


# 4.2 k-means  
set.seed(123) 
kmeans_model <- kmeans(cluster_data_scaled, centers = 3, nstart = 25) 
data$cluster <- as.factor(kmeans_model$cluster) 

centers_tavg   <- tapply(data$tavg, data$cluster, mean) 
rank_order     <- order(centers_tavg) 
season_names   <- c("Холодний (зима)", "Перехідний (весна/осінь)", "Теплий (літо)") 
cluster_labels <- character(3) 
cluster_labels[rank_order] <- season_names 

cat("\nСемантика кластерів:\n") 
for (i in 1:3) cat(sprintf("  Кластер %d → %s (сер. %.1f°C)\n", 
                           i, cluster_labels[i], centers_tavg[i])) 

data$season <- factor(cluster_labels[as.integer(data$cluster)], levels = season_names) 

# Центри кластерів 
num_cols     <- c("tavg","tmin","tmax","prcp","wspd","pres") 
centers_real <- aggregate(data[, num_cols], 
                          by = list(Сезон = data$season), FUN = mean) 
centers_real[, num_cols] <- round(centers_real[, num_cols], 2) 
cat("\n=== Центри кластерів ===\n") 
print(centers_real) 

# Палітра кластерів: синій / зелений / помаранчевий 
season_colours <- c("Холодний (зима)"          = CLR_BLUE, 
                    "Перехідний (весна/осінь)"  = CLR_GREEN, 
                    "Теплий (літо)"             = CLR_ORANGE) 

# 4.3 Графік: температура і атмосферний тиск 
ggplot(data, aes(x = tavg, y = pres, colour = season)) + 
  geom_point(alpha = 0.6, size = 1.6) + 
  scale_colour_manual(values = season_colours) + 
  labs(title    = "Кластерний аналіз: залежність атмосферного тиску від температури 2021–2024", 
       subtitle = "", 
       x = "Середня температура, °C", 
       y = "Атмосферний тиск, гПа", 
       colour = "Сезон") + 
  theme_diploma 

# 4.4 Графік: температура і швидкість вітру 
ggplot(data, aes(x = tavg, y = wspd, colour = season)) + 
  geom_point(alpha = 0.6, size = 1.6) + 
  scale_colour_manual(values = season_colours) + 
  labs(title    = "Кластерний аналіз: залежність швидкості вітру від температури 2021–2024", 
       subtitle = "", 
       x = "Середня температура, °C", 
       y = "Швидкість вітру, км/год", 
       colour = "Сезон") + 
  theme_diploma 

# РОЗДІЛ 5. Кореляційний аналіз 

numeric_data <- data[, c("tavg","tmin","tmax","prcp","wspd","pres")] 
cor_matrix   <- cor(numeric_data) 

cat("\n=== Кореляційна матриця ===\n") 
print(round(cor_matrix, 3)) 

col_labels_x <- c( 
  "Серед.\nтемпература", 
  "Мін.\nтемпература", 
  "Макс.\nтемпература", 
  "Опади", 
  "Швидкість\nвітру", 
  "Атм.\nтиск" 
) 

col_labels_y <- c( 
  "Серед.\nтемпература", 
  "Мін.\nтемпература", 
  "Макс.\nтемпература", 
  "Опади", 
  "Швидкість\nвітру", 
  "Атм.\nтиск" 
) 

col_keys <- c("tavg", "tmin", "tmax", "prcp", "wspd", "pres") 


cor_ua <- cor_matrix 
rownames(cor_ua) <- col_keys 
colnames(cor_ua) <- col_keys 

# Кореляційна матриця через ggplot2 

n <- nrow(cor_ua) 
cor_long <- data.frame( 
  row_key = rep(col_keys, each = n), 
  col_key = rep(col_keys, times = n), 
  row_lbl = rep(col_labels_y, each = n), 
  col_lbl = rep(col_labels_x, times = n), 
  r       = as.vector(cor_ua), 
  row_idx = rep(seq_len(n), each = n), 
  col_idx = rep(seq_len(n), times = n) 
) 
cor_long <- cor_long[cor_long$col_idx >= cor_long$row_idx, ] 

cor_long$label   <- formatC(cor_long$r, digits = 2, format = "f") 
cor_long$txt_col <- ifelse(abs(cor_long$r) > 0.55, "white", "#1A1A1A") 

cor_long$row_lbl <- factor(cor_long$row_lbl, levels = rev(col_labels_y)) 
cor_long$col_lbl <- factor(cor_long$col_lbl, levels = col_labels_x) 

ggplot(cor_long, aes(x = col_lbl, y = row_lbl, fill = r)) + 
  geom_tile(colour = "white", linewidth = 0.5) + 
  geom_text(aes(label = label, colour = txt_col), 
            fontface = "bold", 
            size     = 5) + 
  scale_fill_gradient2( 
    low      = "#4393C3", 
    mid      = "white", 
    high     = "#D6604D", 
    midpoint = 0, 
    limits   = c(-1, 1), 
    name     = "Кореляція", 
    guide    = guide_colorbar( 
      barheight       = unit(0.90, "npc"), 
      barwidth        = unit(1.4, "cm"), 
      title.hjust     = 0.5, 
      label.theme     = element_text(size = 13, colour = CLR_BLACK, face = "bold"), 
      title.theme     = element_text(size = 13, face = "bold", colour = CLR_BLACK, 
                                     margin = margin(b = 8)), 
      frame.colour    = "#555555", 
      ticks.colour    = "#555555", 
      ticks.linewidth = 0.8, 
      nbin            = 300 
    ) 
  ) + 
  scale_colour_identity() + 
  coord_fixed() + 
  labs( 
    title = "Кореляційна матриця метеорологічних показників 2021–2024", 
    x = NULL, y = NULL 
  ) + 
  theme_minimal(base_size = 12) + 
  theme( 
    plot.title           = element_text(face = "bold", size = 13, 
                                        colour = CLR_BLACK, hjust = 0.5), 
    axis.text.x          = element_text(angle = 0, hjust = 0.5, vjust = 1, 
                                        colour = CLR_BLACK, size = 11, 
                                        face = "bold", lineheight = 0.85), 
    axis.text.y          = element_text(colour = CLR_BLACK, size = 11, 
                                        face = "bold", hjust = 1, 
                                        lineheight = 0.85), 
    panel.grid           = element_blank(), 
    legend.position      = "right", 
    legend.justification = "center", 
    legend.title.align   = 0.5, 
    plot.background      = element_rect(fill = "white", colour = NA), 
    panel.background     = element_rect(fill = "white", colour = NA) 
  )

# РОЗДІЛ 6. Поділ на вибірки 

split_boundary <- as.Date("2024-01-01") 
train <- data[data$date <  split_boundary, ] 
test  <- data[data$date >= split_boundary, ] 

cat("\n Вибірки \n") 
cat(sprintf("Навчальна (2021–2023): %d спостережень\n", nrow(train))) 
cat(sprintf("Тестова   (2024):      %d спостережень\n", nrow(test))) 

# РОЗДІЛ 7. Регресійна модель 

model <- lm(tavg ~ tmin + tmax + prcp + wspd + pres, data = train) 
summary(model) 

cf <- coef(model) 
cat("\n Рівняння моделі \n") 
cat(sprintf( 
  "tavg = %.4f + %.4f·tmin + %.4f·tmax + %.4f·prcp + %.4f·wspd + %.4f·pres\n", 
  cf[1], cf["tmin"], cf["tmax"], cf["prcp"], cf["wspd"], cf["pres"] 
)) 

pred_test  <- predict(model, newdata = test) 
pred_train <- predict(model, newdata = train) 

# РОЗДІЛ 8. Метрики точності 

calc_metrics <- function(actual, predicted, label = "") { 
  nonzero  <- actual != 0 
  MAE      <- mean(abs(actual - predicted)) 
  MSE      <- mean((actual - predicted)^2) 
  RMSE     <- sqrt(MSE) 
  MAPE     <- mean(abs((actual[nonzero] - predicted[nonzero]) / 
                         actual[nonzero])) * 100 
  R2       <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2) 
  mean_val <- mean(abs(actual)) 
  
  cat(sprintf("\n Метрики: %s \n", label)) 
  cat(sprintf("  MAE   = %.4f °C  (%.2f %%)\n", MAE,  MAE / mean_val * 100)) 
  cat(sprintf("  MSE   = %.4f °C²\n", MSE)) 
  cat(sprintf("  RMSE  = %.4f °C  (%.2f %%)\n", RMSE, RMSE / mean_val * 100)) 
  70 
  
  
  cat(sprintf("  MAPE  = %.2f %%  ← ключова метрика\n", MAPE)) 
  cat(sprintf("  R²    = %.4f    (%.2f %%)\n", R2, R2 * 100)) 
  
  invisible(list(MAE=MAE, MAE_pct=MAE/mean_val*100, MSE=MSE, 
                 RMSE=RMSE, RMSE_pct=RMSE/mean_val*100, MAPE=MAPE, R2=R2)) 
} 

m_test  <- calc_metrics(test$tavg,  pred_test,  "Тестова (2024) — ОСНОВНА") 
m_train <- calc_metrics(train$tavg, pred_train, "Навчальна (2021–2023)") 

# РОЗДІЛ 9. ГРАФІК РЕГРЕСІЙНОЇ МОДЕЛІ + ПРОГНОЗ НА 2025 

#  9.1 Оцінка моделі на тестовій вибірці  
pred_test_df <- data.frame(date = test$date, predicted = pred_test) 
actual_test_df <- data.frame(date = test$date, actual = test$tavg) 

#  9.2 Побудова прогнозу на 2025 рік  

data$month <- as.integer(format(data$date, "%m")) 

# Середні значення предикторів по місяцях за 2021–2024 
monthly_means <- aggregate( 
  cbind(tmin, tmax, prcp, wspd, pres) ~ month, 
  data = data, 
  FUN  = mean 
) 

# Генерація дати 2025 
dates_2025 <- as.Date(paste0("2025-", sprintf("%02d", 1:12), "-15")) 

forecast_2025 <- data.frame( 
  date  = dates_2025, 
  month = 1:12 
) 
forecast_2025 <- merge(forecast_2025, monthly_means, by = "month") 
forecast_2025 <- forecast_2025[order(forecast_2025$date), ] 

# Прогноз tavg на 2025 
forecast_2025$predicted <- predict(model, newdata = forecast_2025) 


cat(" 
=== Прогноз середньої температури на 2025 рік === 
") 
print(data.frame( 
  Місяць    = format(forecast_2025$date, "%B"), 
  Прогноз_C = round(forecast_2025$predicted, 2) 
)) 

#  9.3 Графік: навчальна і тестова (Y і Ŷ) з прогнозом 2025 ── 
# Межа між тестовою та прогнозом 
forecast_boundary <- as.Date("2025-01-01") 

ggplot() + 
  # Фактичні 2021–2023 (навчальна) — синя 
  geom_line(data = train, 
            aes(x = date, y = tavg, colour = "Фактичні 2021–2023 (навч.)"), 
            linewidth = 0.75) + 
  # Фактичні 2024 (тестова, Y) — зелена 
  geom_line(data = actual_test_df, 
            aes(x = date, y = actual, colour = "Фактичні 2024 (тест., Y)"), 
            linewidth = 0.9) + 
  # Модельна оцінка на тестовій (Ŷ) — помаранчева пунктир 
  geom_line(data = pred_test_df, 
            aes(x = date, y = predicted, colour = "Оцінка моделі 2024 (Ŷ)"), 
            linewidth = 0.9, linetype = "dashed") + 
  # Прогноз 2025 — червона пунктирна 
  geom_line(data = forecast_2025, 
            aes(x = date, y = predicted, colour = "Прогноз 2025"), 
            linewidth = 1.1, linetype = "dashed") + 
  geom_point(data = forecast_2025, 
             aes(x = date, y = predicted, colour = "Прогноз 2025"), 
             size = 2.5) + 
  # Межа навчальна | тестова 
  geom_vline(xintercept = as.numeric(split_boundary), 
             linetype = "dotted", colour = "#666666", linewidth = 0.7) + 
  annotate("text", x = split_boundary, y = max(data$tavg) * 0.93, 
           label = "Початок 
тестової", hjust = -0.05, size = 3.0, 
           colour = "#666666") + 
  # Межа тестова | прогноз 
  

geom_vline(xintercept = as.numeric(forecast_boundary), 
           linetype = "dotted", colour = "#CC0000", linewidth = 0.7) + 
  annotate("text", x = forecast_boundary, y = max(data$tavg) * 0.93, 
           label = "Початок 
прогнозу", hjust = -0.05, size = 3.0, 
           colour = "#CC0000") + 
  scale_colour_manual( 
    name   = NULL, 
    values = c( 
      "Фактичні 2021–2023 (навч.)" = CLR_BLUE, 
      "Фактичні 2024 (тест., Y)"   = CLR_GREEN, 
      "Оцінка моделі 2024 (Ŷ)"     = CLR_ORANGE, 
      "Прогноз 2025"                = "#CC0000" 
    ), 
    # Явний порядок у легенді: синій → помаранчевий → зелений → червоний 
    breaks = c( 
      "Фактичні 2021–2023 (навч.)", 
      "Оцінка моделі 2024 (Ŷ)", 
      "Фактичні 2024 (тест., Y)", 
      "Прогноз 2025" 
    ) 
  ) + 
  scale_x_date( 
    limits = c(as.Date("2021-01-01"), as.Date("2025-12-31")), 
    expand = c(0, 0) 
  ) + 
  labs( 
    title    = "Регресійна модель", 
    subtitle = "", 
    x        = "Час", 
    y        = "Середня температура, °C" 
  ) + 
  theme_diploma
