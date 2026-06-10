###############################################################
#                                                             #
#  PRACA MAGISTERSKA: Porównanie VAR / BVAR / FAVAR / ARIMA   #
#  w prognozowaniu inflacji w Polsce                          #
#                                                             #
#  Struktura pliku:                                           #
#    SEKCJA 1: Wczytanie i transformacja danych               #
#    SEKCJA 2: Model VAR (all-subset selection)               #
#    SEKCJA 3: Model BVAR (3 warianty + 2 podejścia λ)        #
#    SEKCJA 4: Model FAVAR (3 warianty + grid k x p)          #
#    SEKCJA 5: Benchmark ARIMA                                #
#    SEKCJA 6: Porównanie końcowe VAR / BVAR / FAVAR / ARIMA  #
#                                                             #
#                                                             #
###############################################################

rm(list = ls())

library(readxl); library(zoo); library(tseries); library(dplyr)
library(tidyr); library(urca); library(forecast); library(vars)
library(BVAR)


###############################################################
#                                                             #
#  SEKCJA 1: WCZYTANIE DANYCH I TRANSFORMACJE                 #
#                                                             #
###############################################################

# -------------------------------------------------------------
# 1.1 Wczytanie danych i podział train/val/test
# -------------------------------------------------------------
data_raw <- read_excel("C:\\Users\\przem\\OneDrive\\Pulpit\\dane_mgr.xlsx")
data_raw$Data <- as.yearqtr(data_raw$Data, format = "%Y Q%q")
ts_data <- ts(data_raw[, -1], start = c(2005, 1), frequency = 4)
ts_data[ts_data == 0] <- NA
ts_data_clean <- na.omit(ts_data)

train_end  <- c(2020, 4)
val_start  <- c(2021, 1); val_end <- c(2022, 4)
test_start <- c(2023, 1)

train_data <- window(ts_data_clean, end = train_end)
val_data   <- window(ts_data_clean, start = val_start, end = val_end)
test_data  <- window(ts_data_clean, start = test_start)

cat("==============================================================\n")
cat("SEKCJA 1: WCZYTANIE DANYCH\n")
cat("==============================================================\n")
cat("TRAIN:     ", dim(train_data), "(2005Q1 - 2020Q4)\n")
cat("VALIDATION:", dim(val_data),   "(2021Q1 - 2022Q4)\n")
cat("TEST:      ", dim(test_data),  "(2023Q1 - 2025Q3)\n\n")


# -------------------------------------------------------------
# 1.2 Słownik krótkich nazw zmiennych
# -------------------------------------------------------------
infl_name <- "Inflacja [ogółem - okres poprzedni = 100]"

nazwy_skrocone <- c(
  "Inflacja [ogółem - okres poprzedni = 100]" = "Inflacja",
  "Stopa referencyjna" = "Stopa_ref",
  "Przeciętne miesięczne wynagrodzenie nominalne brutto w gospodarce narodoweja (zł)" = "Wynagrodzenie",
  "Stopa bezrobocia rejestrowanego (stan w końcu okresu) [%]" = "Bezrobocie",
  "Produkt krajowy brutto (ceny bieżące) [mln zł]" = "PKB",
  "Popyt krajowy (ceny stale) [analogiczny okres roku poprzedniego=100]" = "Popyt_kraj",
  "spożycie w sektorze gospodarstw domowych [analogiczny okres roku poprzedniego=100]" = "Spozycie_HH",
  "Eksport towarów i usług (ceny stale) [analogiczny okres roku poprzedniego=100]" = "Eksport",
  "Import towarów i usług (ceny stale) [analogiczny okres roku poprzedniego=100]" = "Import",
  "Wskaźnik ogólnego klimatu koniunktury w przetwórstwie przemysłowym (w miesiącu kończącym okres)" = "Klimat",
  "Podaż pieniądza M3 (stan w końcu okresu) [mln zł]" = "M3",
  "pieniądz gotówkowy w obiegu poza kasami banków [mln zł]" = "Gotowka",
  "Należności ogółem (stan w końcu okresu) [mln zł]" = "Naleznosci",
  "Stopa oprocentowania depozytów złotowych gospodarstw domowych i instytucji niekomercyjnych działających na rzecz gospodarstw domowych w bankach komercyjnych (na rachunkach bieżących) [%]" = "Dep_biez",
  "Stopa oprocentowania depozytów złotowych gospodarstw domowych i instytucji niekomercyjnych działających na rzecz gospodarstw domowych w bankach komercyjnych (z terminem pierwotnym do 2 lat włącznie) [%]" = "Dep_2lat",
  "Kurs oficjalny NBP (100 USD)" = "USD",
  "Kurs oficjalny NBP (100 EUR)" = "EUR",
  "Kurs oficjalny NBP (100 CHF)" = "CHF",
  "Dochody budżetu państwa ogółem (od początku roku do końca okresu) [mln zł]" = "Dochody_bud",
  "Zadłużenie krajowe Skarbu Państwa (stan w końcu okresu) [mln zł]" = "Dlug_kraj",
  "Dług zagraniczny Skarbu Państwa (stan w końcu okresu) [mln zł]" = "Dlug_zagr",
  "Przychody ogółem przedsiębiorstw (od początku roku do końca okresu) [mln zł]" = "Przychody",
  "Kurs zamknięcia WIG20" = "WIG20"
)


# -------------------------------------------------------------
# 1.3 Transformacja: pierwsze różnice wybranych zmiennych I(1)
# -------------------------------------------------------------
zmienne_do_rozn <- c(
  "Stopa referencyjna",
  "Przeciętne miesięczne wynagrodzenie nominalne brutto w gospodarce narodoweja (zł)",
  "Stopa bezrobocia rejestrowanego (stan w końcu okresu) [%]",
  "Produkt krajowy brutto (ceny bieżące) [mln zł]",
  "Wskaźnik ogólnego klimatu koniunktury w przetwórstwie przemysłowym (w miesiącu kończącym okres)",
  "Podaż pieniądza M3 (stan w końcu okresu) [mln zł]",
  "pieniądz gotówkowy w obiegu poza kasami banków [mln zł]",
  "Należności ogółem (stan w końcu okresu) [mln zł]",
  "Stopa oprocentowania depozytów złotowych gospodarstw domowych i instytucji niekomercyjnych działających na rzecz gospodarstw domowych w bankach komercyjnych (na rachunkach bieżących) [%]",
  "Stopa oprocentowania depozytów złotowych gospodarstw domowych i instytucji niekomercyjnych działających na rzecz gospodarstw domowych w bankach komercyjnych (z terminem pierwotnym do 2 lat włącznie) [%]",
  "Zadłużenie krajowe Skarbu Państwa (stan w końcu okresu) [mln zł]",
  "Dług zagraniczny Skarbu Państwa (stan w końcu okresu) [mln zł]"
)

full_ts   <- rbind(train_data, val_data, test_data)
full_diff <- full_ts
for (zm in zmienne_do_rozn) {
  if (zm %in% colnames(full_diff)) {
    full_diff[, zm] <- c(NA, diff(full_ts[, zm]))
  }
}

n_train_orig <- nrow(train_data)
n_val        <- nrow(val_data)

train_diff <- full_diff[2:n_train_orig, ]
val_diff   <- full_diff[(n_train_orig + 1):(n_train_orig + n_val), ]
test_diff  <- full_diff[(n_train_orig + n_val + 1):nrow(full_diff), ]

cat("Po różnicowaniu:\n")
cat("  train_diff:", dim(train_diff), "\n")
cat("  val_diff  :", dim(val_diff),   "\n")
cat("  test_diff :", dim(test_diff),  "\n\n")


###############################################################
#                                                             #
#  SEKCJA 2: MODEL VAR                                        #
#                                                             #
###############################################################

cat("\n###############################################################\n")
cat("# SEKCJA 2: MODEL VAR                                          #\n")
cat("###############################################################\n\n")

zmienne_kandydaci <- setdiff(colnames(train_diff), infl_name)


# -------------------------------------------------------------
# 2.1 Funkcja prognozy 1-step-ahead 
# -------------------------------------------------------------
forecast_one_step <- function(Y_history, beta, p) {
  k <- ncol(Y_history); n <- nrow(Y_history)
  intercept <- beta[, 1]
  B <- beta[, 2:(k*p + 1), drop = FALSE]
  lagged <- numeric(k * p)
  for (j in seq_len(p)) {
    lagged[((j-1)*k + 1):(j*k)] <- Y_history[n - j + 1, ]
  }
  as.vector(intercept + B %*% lagged)
}


# -------------------------------------------------------------
# 2.2 All-subset selection: ocena każdej kombinacji 2-4 zmiennych
# -------------------------------------------------------------
ocen_kombinacje <- function(zmienne, train_diff, val_diff, lag_max = 8) {
  Y_tr  <- as.matrix(train_diff[, zmienne])
  Y_val <- as.matrix(val_diff[, zmienne])
  k_var <- ncol(Y_tr); n_tr <- nrow(Y_tr)
  
  lag_sel <- tryCatch(VARselect(Y_tr, lag.max = lag_max, type = "const"),
                      error = function(e) NULL)
  if (is.null(lag_sel)) return(NULL)
  p <- as.numeric(lag_sel$selection["SC(n)"])
  
  n_param <- k_var * (k_var * p + 1)
  T_eff   <- n_tr - p
  if (T_eff <= n_param + 5) return(NULL)
  
  var_obj <- tryCatch(VAR(Y_tr, p = p, type = "const"),
                      error = function(e) NULL)
  if (is.null(var_obj)) return(NULL)
  
  rt <- roots(var_obj)
  if (max(rt) >= 1) return(NULL)
  
  serial_t <- tryCatch(serial.test(var_obj, lags.pt = 16, type = "PT.asymptotic"),
                       error = function(e) NULL)
  p_serial <- if (is.null(serial_t)) NA else serial_t$serial$p.value
  
  B    <- Bcoef(var_obj)
  beta <- cbind(B[, ncol(B)], B[, 1:(k_var * p)])
  Y_combined <- rbind(Y_tr, Y_val)
  n_v <- nrow(Y_val)
  preds <- matrix(NA_real_, n_v, k_var, dimnames = list(NULL, colnames(Y_tr)))
  for (i in seq_len(n_v)) {
    preds[i, ] <- forecast_one_step(
      Y_combined[seq_len(n_tr + i - 1), , drop = FALSE], beta, p)
  }
  
  infl_col_local <- which(colnames(Y_val) == infl_name)
  pred_val <- preds[, infl_col_local]
  real_val <- Y_val[, infl_col_local]
  
  list(zmienne = zmienne, k_var = k_var, p = p,
       max_root = max(rt), p_serial = p_serial,
       RMSE_val = sqrt(mean((pred_val - real_val)^2)),
       MAE_val  = mean(abs(pred_val - real_val)),
       n_param = n_param, T_eff = T_eff)
}

kombinacje <- list()
licznik <- 0
for (k_size in 2:4) {
  combs <- combn(zmienne_kandydaci, k_size, simplify = FALSE)
  for (cb in combs) {
    licznik <- licznik + 1
    kombinacje[[licznik]] <- c(infl_name, cb)
  }
}
cat("Liczba kombinacji do przetestowania:", length(kombinacje), "\n\n")

cat("Estymacja", length(kombinacje), "kombinacji VAR...\n")
wyniki_subset <- vector("list", length(kombinacje))
prog_step <- max(1, floor(length(kombinacje) / 10))
t0 <- Sys.time()

for (i in seq_along(kombinacje)) {
  wyniki_subset[[i]] <- ocen_kombinacje(kombinacje[[i]], train_diff, val_diff)
  if (i %% prog_step == 0) {
    cat(sprintf("  %d/%d (%.0f%%) - %.0fs\n", i, length(kombinacje),
                100*i/length(kombinacje),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
}

wyniki_subset <- Filter(Negate(is.null), wyniki_subset)
cat("\nLiczba stabilnych modeli VAR:", length(wyniki_subset), "\n\n")


# -------------------------------------------------------------
# 2.3 Ranking + filtr Portmanteau po reestymacji na train+val
# -------------------------------------------------------------
ranking_var <- data.frame(
  idx = seq_along(wyniki_subset),
  k_var    = sapply(wyniki_subset, function(x) x$k_var),
  p        = sapply(wyniki_subset, function(x) x$p),
  max_root = sapply(wyniki_subset, function(x) round(x$max_root, 4)),
  p_serial = sapply(wyniki_subset, function(x) round(x$p_serial, 4)),
  RMSE_val = sapply(wyniki_subset, function(x) round(x$RMSE_val, 4)),
  MAE_val  = sapply(wyniki_subset, function(x) round(x$MAE_val, 4)),
  T_eff    = sapply(wyniki_subset, function(x) x$T_eff),
  n_param  = sapply(wyniki_subset, function(x) x$n_param)
)
ranking_var <- ranking_var[order(ranking_var$RMSE_val), ]

trainval_diff <- rbind(train_diff, val_diff)

cat("=== Wyszukiwanie modelu VAR spełniającego Portmanteau po reestymacji ===\n")
best_idx_in_ranking <- NA
najlepszy_idx       <- NA
max_do_sprawdzenia  <- 200

for (j in seq_len(min(nrow(ranking_var), max_do_sprawdzenia))) {
  idx_kand <- ranking_var$idx[j]
  zm_kand  <- wyniki_subset[[idx_kand]]$zmienne
  p_kand   <- wyniki_subset[[idx_kand]]$p
  
  Y_tv <- as.matrix(trainval_diff[, zm_kand])
  var_tv <- tryCatch(VAR(Y_tv, p = p_kand, type = "const"),
                     error = function(e) NULL)
  if (is.null(var_tv)) next
  rt_tv <- roots(var_tv)
  if (max(rt_tv) >= 1) next
  
  s_tv <- tryCatch(serial.test(var_tv, lags.pt = 16, type = "PT.asymptotic"),
                   error = function(e) NULL)
  if (is.null(s_tv)) next
  p_serial_tv <- s_tv$serial$p.value
  
  if (j <= 20 || p_serial_tv > 0.05) {
    cat(sprintf("  rank=%3d, k=%d, p=%d, RMSE_val=%.3f, p_Portmanteau(tv)=%.4f %s\n",
                j, ranking_var$k_var[j], p_kand, ranking_var$RMSE_val[j],
                p_serial_tv,
                if (p_serial_tv > 0.05) "*** WYBRANY" else ""))
  }
  
  if (p_serial_tv > 0.05) {
    best_idx_in_ranking <- j
    najlepszy_idx <- idx_kand
    break
  }
}

if (is.na(najlepszy_idx)) {
  cat("\nUWAGA: w pierwszych", max_do_sprawdzenia,
      "modelach żaden nie spełnia testu Portmanteau na train+val.\n")
  najlepszy_idx <- ranking_var$idx[1]
  best_idx_in_ranking <- 1
}

best_model_var <- wyniki_subset[[najlepszy_idx]]
cat("\n>>> Wybrany model VAR:\n")
cat("    Pozycja w rankingu RMSE_val:", best_idx_in_ranking, "\n")
cat("    Liczba zmiennych:", best_model_var$k_var, "\n")
cat("    Lag p:", best_model_var$p, "\n")
cat("    RMSE_val:", round(best_model_var$RMSE_val, 4), "\n\n")

cat("Zmienne wybranego modelu VAR:\n")
for (z in best_model_var$zmienne) {
  cat(sprintf("  %-15s : %s\n", nazwy_skrocone[z], z))
}


# -------------------------------------------------------------
# 2.4 Reestymacja VAR na train+val + diagnostyka
# -------------------------------------------------------------
zmienne_final_var <- best_model_var$zmienne
p_final_var       <- best_model_var$p

Y_trainval_var <- as.matrix(trainval_diff[, zmienne_final_var])
Y_test_var     <- as.matrix(test_diff[, zmienne_final_var])
colnames(Y_trainval_var) <- nazwy_skrocone[zmienne_final_var]
colnames(Y_test_var)     <- nazwy_skrocone[zmienne_final_var]
k_final_var <- ncol(Y_trainval_var)

var_final <- VAR(Y_trainval_var, p = p_final_var, type = "const")

cat("\n=== Diagnostyka VAR finalnego (TRAIN+VAL) ===\n")
rt_f <- roots(var_final)
cat("Max |pierwiastek|:", round(max(rt_f), 4), "| stabilny:", max(rt_f) < 1, "\n")

s_f <- serial.test(var_final, lags.pt = 16, type = "PT.asymptotic")
cat("Portmanteau (lags=16): p =", round(s_f$serial$p.value, 4),
    if (s_f$serial$p.value > 0.05) "OK" else "PROBLEM", "\n")

n_f <- normality.test(var_final)
cat("Jarque-Bera multi: p =", round(n_f$jb.mul$JB$p.value, 4),
    if (n_f$jb.mul$JB$p.value > 0.05) "OK" else "nie-normalne", "\n")

a_f <- arch.test(var_final, lags.multi = 5)
cat("ARCH multi (lags=5): p =", round(a_f$arch.mul$p.value, 4),
    if (a_f$arch.mul$p.value > 0.05) "OK" else "heteroskedastyczne", "\n")

cat("\n--- Równanie inflacji ---\n")
print(summary(var_final)$varresult$Inflacja)


# -------------------------------------------------------------
# 2.5 Test przyczynowości Grangera
# -------------------------------------------------------------
cat("\n=== Testy przyczynowości Grangera ===\n")
granger_results <- data.frame(
  Zmienna = character(0), F_stat = numeric(0),
  df1 = numeric(0), df2 = numeric(0), p_value = numeric(0),
  stringsAsFactors = FALSE
)
for (zm_short in setdiff(colnames(Y_trainval_var), "Inflacja")) {
  gr <- tryCatch(causality(var_final, cause = zm_short),
                 error = function(e) NULL)
  if (!is.null(gr)) {
    granger_results <- rbind(granger_results, data.frame(
      Zmienna = zm_short,
      F_stat  = round(as.numeric(gr$Granger$statistic), 4),
      df1     = as.numeric(gr$Granger$parameter[1]),
      df2     = as.numeric(gr$Granger$parameter[2]),
      p_value = round(as.numeric(gr$Granger$p.value), 4)
    ))
  }
}
print(granger_results, row.names = FALSE)


# -------------------------------------------------------------
# 2.6 Prognoza VAR na zbiorze testowym
# -------------------------------------------------------------
B_f    <- Bcoef(var_final)
beta_f <- cbind(B_f[, ncol(B_f)], B_f[, 1:(k_final_var * p_final_var)])

Y_combined_var <- rbind(Y_trainval_var, Y_test_var)
n_tv_var <- nrow(Y_trainval_var)
n_te_var <- nrow(Y_test_var)

preds_var <- matrix(NA_real_, n_te_var, k_final_var,
                    dimnames = list(NULL, colnames(Y_trainval_var)))
for (i in seq_len(n_te_var)) {
  preds_var[i, ] <- forecast_one_step(
    Y_combined_var[seq_len(n_tv_var + i - 1), , drop = FALSE], beta_f, p_final_var)
}

infl_pred_var <- preds_var[, "Inflacja"]
infl_real_var <- Y_test_var[, "Inflacja"]

RMSE_var   <- sqrt(mean((infl_pred_var - infl_real_var)^2))
MAE_var    <- mean(abs(infl_pred_var - infl_real_var))
MAPE_var   <- mean(abs((infl_pred_var - infl_real_var) / infl_real_var)) * 100
naive_pred <- rep(Y_trainval_var[n_tv_var, "Inflacja"], n_te_var)
RMSE_naive <- sqrt(mean((naive_pred - infl_real_var)^2))
TheilU_var <- RMSE_var / RMSE_naive

cat("\n==============================================================\n")
cat("WYNIKI VAR NA ZBIORZE TESTOWYM\n")
cat("==============================================================\n")
cat("RMSE   :", round(RMSE_var, 4), "\n")
cat("MAE    :", round(MAE_var, 4),  "\n")
cat("MAPE   :", round(MAPE_var, 4), "%\n")
cat("Theil U:", round(TheilU_var, 4),
    if (TheilU_var < 1) "(model bije naiwę)" else "(naiwa lepsza)", "\n")

tabela_var <- data.frame(
  Kwartal     = as.yearqtr(time(test_data))[seq_len(n_te_var)],
  Rzeczywista = round(infl_real_var, 3),
  Prognoza    = round(infl_pred_var, 3),
  Bład        = round(infl_pred_var - infl_real_var, 3)
)
cat("\n--- Tabela prognoz VAR ---\n")
print(tabela_var, row.names = FALSE)


# -------------------------------------------------------------
# 2.7 IRF + FEVD dla modelu VAR
# -------------------------------------------------------------
cat("\n=== IRF VAR (reakcja inflacji na szoki) ===\n")
irf_var <- vars::irf(var_final, response = "Inflacja", n.ahead = 12,
                     ortho = TRUE, boot = TRUE, ci = 0.95, runs = 200)

n_shocks_var  <- length(colnames(Y_trainval_var))
ncol_plot_var <- min(3, n_shocks_var)
nrow_plot_var <- ceiling(n_shocks_var / ncol_plot_var)
par(mfrow = c(nrow_plot_var, ncol_plot_var), mar = c(3.5, 4, 2.5, 1))
for (sh in colnames(Y_trainval_var)) {
  irf_vals <- irf_var$irf[[sh]][, "Inflacja"]
  ci_low   <- irf_var$Lower[[sh]][, "Inflacja"]
  ci_upp   <- irf_var$Upper[[sh]][, "Inflacja"]
  plot(0:12, irf_vals, type = "l", lwd = 2.5, col = "blue",
       xlab = "Kwartały po szoku", ylab = "Reakcja inflacji",
       main = sprintf("VAR - Szok: %s", sh),
       ylim = range(c(ci_low, ci_upp, 0)))
  polygon(c(0:12, 12:0), c(ci_low, rev(ci_upp)),
          col = adjustcolor("blue", alpha.f = 0.15), border = NA)
  abline(h = 0, lty = 3, col = "grey40")
}
par(mfrow = c(1, 1))

fevd_var <- vars::fevd(var_final, n.ahead = 12)
cat("\n=== FEVD inflacji (VAR) ===\n")
print(round(fevd_var$Inflacja[c(1, 4, 8, 12), ], 3))


# Wykres prognozy VAR
test_dates <- as.yearqtr(time(test_data))[seq_len(n_te_var)]
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3.5, 1))
plot(test_dates, infl_real_var,
     type = "o", pch = 16, col = "black", lwd = 2,
     xlab = "Kwartał",
     ylab = "Inflacja kw/kw (poprz. okres = 100)",
     main = sprintf("VAR(%d) %d-zm. - prognoza vs rzeczywistość\nRMSE = %.3f, Theil U = %.3f",
                    p_final_var, k_final_var, RMSE_var, TheilU_var),
     ylim = range(c(infl_real_var, infl_pred_var)) + c(-0.5, 0.5))
lines(test_dates, infl_pred_var, type = "o", pch = 17, col = "red", lwd = 2)
abline(h = 100, lty = 3, col = "grey60")
legend("topright", legend = c("Rzeczywista", "Prognoza VAR"),
       col = c("black", "red"), pch = c(16, 17), lty = 1, lwd = 2, bty = "n")


###############################################################
#                                                             #
#  SEKCJA 3: MODEL BVAR                                       #
#                                                             #
###############################################################

cat("\n\n###############################################################\n")
cat("# SEKCJA 3: MODEL BVAR                                         #\n")
cat("###############################################################\n\n")


# -------------------------------------------------------------
# 3.1 Zestawy zmiennych dla 3 wariantów BVAR
# -------------------------------------------------------------

# Wariant A: 5 zmiennych - identyczny zbiór jak finalny VAR
zmienne_maly <- c(
  "Inflacja [ogółem - okres poprzedni = 100]",
  "Przeciętne miesięczne wynagrodzenie nominalne brutto w gospodarce narodoweja (zł)",
  "spożycie w sektorze gospodarstw domowych [analogiczny okres roku poprzedniego=100]",
  "Eksport towarów i usług (ceny stale) [analogiczny okres roku poprzedniego=100]",
  "Import towarów i usług (ceny stale) [analogiczny okres roku poprzedniego=100]"
)

# Wariant B: 10 zmiennych - kanały transmisji inflacyjnej
zmienne_sredni <- c(
  "Inflacja [ogółem - okres poprzedni = 100]",
  "Stopa referencyjna",
  "Przeciętne miesięczne wynagrodzenie nominalne brutto w gospodarce narodoweja (zł)",
  "Stopa bezrobocia rejestrowanego (stan w końcu okresu) [%]",
  "spożycie w sektorze gospodarstw domowych [analogiczny okres roku poprzedniego=100]",
  "Podaż pieniądza M3 (stan w końcu okresu) [mln zł]",
  "Kurs oficjalny NBP (100 EUR)",
  "Import towarów i usług (ceny stale) [analogiczny okres roku poprzedniego=100]",
  "Wskaźnik ogólnego klimatu koniunktury w przetwórstwie przemysłowym (w miesiącu kończącym okres)",
  "Dochody budżetu państwa ogółem (od początku roku do końca okresu) [mln zł]"
)

# Wariant C: wszystkie 23 zmienne (kontrolny)
zmienne_duzy <- colnames(train_diff)


# -------------------------------------------------------------
# 3.2 Funkcje BVAR
# -------------------------------------------------------------

extract_beta_bvar <- function(bv_obj, k, p) {
  bcoef <- tryCatch(coef(bv_obj), error = function(e) NULL)
  if (!is.null(bcoef)) {
    if (is.matrix(bcoef) && all(dim(bcoef) == c(k*p + 1, k))) {
      bcoef_t <- t(bcoef)
      beta_final <- cbind(bcoef_t[, ncol(bcoef_t)], bcoef_t[, 1:(k*p)])
      colnames(beta_final) <- c("const",
                                if (!is.null(rownames(bcoef))) rownames(bcoef)[1:(k*p)] else paste0("L", 1:(k*p)))
      return(beta_final)
    }
    if (is.matrix(bcoef) && all(dim(bcoef) == c(k, k*p + 1))) {
      beta_final <- cbind(bcoef[, ncol(bcoef)], bcoef[, 1:(k*p)])
      colnames(beta_final) <- c("const",
                                if (!is.null(colnames(bcoef))) colnames(bcoef)[1:(k*p)] else paste0("L", 1:(k*p)))
      return(beta_final)
    }
  }
  beta_arr <- bv_obj$beta
  if (length(dim(beta_arr)) == 3) {
    beta_mean <- apply(beta_arr, c(2, 3), mean)
    beta_t <- t(beta_mean)
    beta_final <- cbind(beta_t[, ncol(beta_t)], beta_t[, 1:(k*p)])
    colnames(beta_final) <- c("const", paste0("L", 1:(k*p)))
    return(beta_final)
  }
  stop("Nie udało się wyciągnąć beta")
}

# psi (skala wariancji równania) liczone OLS-em - obejście problemu auto-psi w pakiecie BVAR
compute_psi_vector <- function(Y, p) {
  k <- ncol(Y)
  psi_vec <- numeric(k)
  for (j in seq_len(k)) {
    yj <- as.numeric(Y[, j])
    n_obs <- length(yj)
    psi_j <- tryCatch({
      X_lag <- sapply(1:p, function(l) c(rep(NA, l), yj[1:(n_obs - l)]))
      df_ar <- data.frame(y = yj, X_lag); df_ar <- na.omit(df_ar)
      lm_fit <- lm(y ~ ., data = df_ar)
      v <- var(residuals(lm_fit))
      if (is.na(v) || v <= 0) var(yj, na.rm = TRUE) else v
    }, error = function(e) var(yj, na.rm = TRUE))
    psi_vec[j] <- psi_j
  }
  psi_vec[psi_vec <= 0 | is.na(psi_vec)] <- 1
  psi_vec
}

# Estymacja BVAR + ocena na walidacji
# tryb: "minnesota_grid" | "minnesota_socsur" | "hierarchical"
ocen_bvar <- function(zmienne, p, lambda_val = NULL, tryb = "minnesota_grid",
                      train_diff, val_diff,
                      n_draws = 5000, n_burn = 2500, verbose = FALSE) {
  Y_tr_raw  <- as.matrix(train_diff[, zmienne])
  Y_val_raw <- as.matrix(val_diff[, zmienne])
  k_var <- ncol(Y_tr_raw); n_tr <- nrow(Y_tr_raw)
  
  means_tr <- colMeans(Y_tr_raw); sds_tr <- apply(Y_tr_raw, 2, sd)
  sds_tr[sds_tr == 0] <- 1
  Y_tr  <- scale(Y_tr_raw,  center = means_tr, scale = sds_tr)
  Y_val <- scale(Y_val_raw, center = means_tr, scale = sds_tr)
  attr(Y_tr,  "scaled:center") <- NULL; attr(Y_tr,  "scaled:scale")  <- NULL
  attr(Y_val, "scaled:center") <- NULL; attr(Y_val, "scaled:scale")  <- NULL
  
  psi_vec <- compute_psi_vector(Y_tr, p)
  
  if (tryb == "hierarchical") {
    prior_minn <- bv_minnesota(
      lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
      alpha  = bv_alpha(mode = 2),
      psi    = bv_psi(scale = 0.004, shape = 0.004,
                      mode = psi_vec, min = psi_vec/100, max = psi_vec*100),
      var    = 1e07
    )
    soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
    sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)
    priors_full <- bv_priors(hyper = c("lambda", "soc", "sur"),
                             mn = prior_minn, soc = soc, sur = sur)
  } else if (tryb == "minnesota_socsur") {
    if (is.null(lambda_val)) stop("Dla minnesota_socsur podaj lambda_val")
    prior_minn <- bv_minnesota(
      lambda = bv_lambda(mode = lambda_val, sd = 0.4, min = 0.0001, max = 5),
      alpha  = bv_alpha(mode = 2),
      psi    = bv_psi(scale = 0.004, shape = 0.004,
                      mode = psi_vec, min = psi_vec/100, max = psi_vec*100),
      var    = 1e07
    )
    soc <- bv_soc(mode = 1, sd = 0.5, min = 1e-04, max = 50)
    sur <- bv_sur(mode = 1, sd = 0.5, min = 1e-04, max = 50)
    priors_full <- bv_priors(hyper = "lambda",
                             mn = prior_minn, soc = soc, sur = sur)
  } else {
    if (is.null(lambda_val)) stop("Dla minnesota_grid podaj lambda_val")
    prior_minn <- bv_minnesota(
      lambda = bv_lambda(mode = lambda_val, sd = 0.4, min = 0.0001, max = 5),
      alpha  = bv_alpha(mode = 2),
      psi    = bv_psi(scale = 0.004, shape = 0.004,
                      mode = psi_vec, min = psi_vec/100, max = psi_vec*100),
      var    = 1e07
    )
    priors_full <- bv_priors(hyper = "lambda", mn = prior_minn)
  }
  
  bv_fit <- tryCatch(
    bvar(Y_tr, lags = p, n_draw = n_draws, n_burn = n_burn,
         priors = priors_full, verbose = verbose),
    error = function(e) { message("Błąd bvar(): ", conditionMessage(e)); NULL })
  if (is.null(bv_fit)) return(NULL)
  
  beta_pt <- tryCatch(extract_beta_bvar(bv_fit, k_var, p),
                      error = function(e) NULL)
  if (is.null(beta_pt)) return(NULL)
  
  B <- beta_pt[, 2:(k_var*p + 1), drop = FALSE]
  C <- matrix(0, k_var*p, k_var*p); C[1:k_var, ] <- B
  if (p > 1) C[(k_var+1):(k_var*p), 1:(k_var*(p-1))] <- diag(k_var*(p-1))
  max_root <- max(Mod(eigen(C, only.values = TRUE)$values))
  stable <- max_root < 1
  
  Y_combined_std <- rbind(Y_tr, Y_val)
  n_v <- nrow(Y_val)
  preds_std <- matrix(NA_real_, n_v, k_var,
                      dimnames = list(NULL, colnames(Y_tr)))
  for (i in seq_len(n_v)) {
    preds_std[i, ] <- forecast_one_step(
      Y_combined_std[seq_len(n_tr + i - 1), , drop = FALSE], beta_pt, p)
  }
  preds_orig <- sweep(preds_std, 2, sds_tr, "*")
  preds_orig <- sweep(preds_orig, 2, means_tr, "+")
  
  infl_col_local <- which(colnames(Y_val_raw) == infl_name)
  pred_val <- preds_orig[, infl_col_local]
  real_val <- Y_val_raw[, infl_col_local]
  
  lambda_eff <- if (tryb == "hierarchical") {
    mean(bv_fit$hyper[, "lambda"])
  } else lambda_val
  
  list(zmienne = zmienne, k_var = k_var, p = p, tryb = tryb,
       lambda_val = lambda_val, lambda_eff = lambda_eff,
       max_root = max_root, stable = stable,
       RMSE_val = sqrt(mean((pred_val - real_val)^2)),
       MAE_val  = mean(abs(pred_val - real_val)),
       bv_fit = bv_fit, beta_pt = beta_pt,
       means_tr = means_tr, sds_tr = sds_tr)
}

build_ranking <- function(wyniki_list) {
  wyniki_ok <- Filter(Negate(is.null), wyniki_list)
  if (length(wyniki_ok) == 0) {
    return(data.frame(lambda = numeric(0), max_root = numeric(0),
                      stable = logical(0), RMSE_val = numeric(0),
                      MAE_val = numeric(0)))
  }
  ranking <- data.frame(
    lambda   = sapply(wyniki_ok, function(x) x$lambda_val),
    max_root = round(sapply(wyniki_ok, function(x) x$max_root), 4),
    stable   = sapply(wyniki_ok, function(x) x$stable),
    RMSE_val = round(sapply(wyniki_ok, function(x) x$RMSE_val), 4),
    MAE_val  = round(sapply(wyniki_ok, function(x) x$MAE_val), 4)
  )
  ranking[order(ranking$RMSE_val), ]
}

# Reestymacja BVAR na train+val + ocena na teście
ocen_finalny_bvar <- function(zmienne, p, lambda_val = NULL, tryb,
                              train_diff, val_diff, test_diff,
                              n_draws = 15000, n_burn = 7500) {
  trainval_diff <- rbind(train_diff, val_diff)
  Y_tv_raw <- as.matrix(trainval_diff[, zmienne])
  Y_te_raw <- as.matrix(test_diff[, zmienne])
  colnames(Y_tv_raw) <- nazwy_skrocone[zmienne]
  colnames(Y_te_raw) <- nazwy_skrocone[zmienne]
  k_var <- ncol(Y_tv_raw)
  
  means_tv <- colMeans(Y_tv_raw); sds_tv <- apply(Y_tv_raw, 2, sd)
  sds_tv[sds_tv == 0] <- 1
  Y_tv <- scale(Y_tv_raw, center = means_tv, scale = sds_tv)
  Y_te <- scale(Y_te_raw, center = means_tv, scale = sds_tv)
  attr(Y_tv, "scaled:center") <- NULL; attr(Y_tv, "scaled:scale") <- NULL
  attr(Y_te, "scaled:center") <- NULL; attr(Y_te, "scaled:scale") <- NULL
  
  psi_vec <- compute_psi_vector(Y_tv, p)
  
  if (tryb == "hierarchical") {
    prior_minn <- bv_minnesota(
      lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
      alpha  = bv_alpha(mode = 2),
      psi    = bv_psi(scale = 0.004, shape = 0.004,
                      mode = psi_vec, min = psi_vec/100, max = psi_vec*100),
      var = 1e07)
    soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
    sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)
    priors_full <- bv_priors(hyper = c("lambda", "soc", "sur"),
                             mn = prior_minn, soc = soc, sur = sur)
  } else if (tryb == "minnesota_socsur") {
    prior_minn <- bv_minnesota(
      lambda = bv_lambda(mode = lambda_val, sd = 0.4, min = 0.0001, max = 5),
      alpha = bv_alpha(mode = 2),
      psi = bv_psi(scale = 0.004, shape = 0.004,
                   mode = psi_vec, min = psi_vec/100, max = psi_vec*100),
      var = 1e07)
    soc <- bv_soc(mode = 1, sd = 0.5, min = 1e-04, max = 50)
    sur <- bv_sur(mode = 1, sd = 0.5, min = 1e-04, max = 50)
    priors_full <- bv_priors(hyper = "lambda",
                             mn = prior_minn, soc = soc, sur = sur)
  } else {
    prior_minn <- bv_minnesota(
      lambda = bv_lambda(mode = lambda_val, sd = 0.4, min = 0.0001, max = 5),
      alpha = bv_alpha(mode = 2),
      psi = bv_psi(scale = 0.004, shape = 0.004,
                   mode = psi_vec, min = psi_vec/100, max = psi_vec*100),
      var = 1e07)
    priors_full <- bv_priors(hyper = "lambda", mn = prior_minn)
  }
  
  set.seed(456)
  bv_fit <- bvar(Y_tv, lags = p, n_draw = n_draws, n_burn = n_burn,
                 priors = priors_full, verbose = FALSE)
  
  beta_pt <- extract_beta_bvar(bv_fit, k_var, p)
  
  B <- beta_pt[, 2:(k_var*p + 1), drop = FALSE]
  C <- matrix(0, k_var*p, k_var*p); C[1:k_var, ] <- B
  if (p > 1) C[(k_var+1):(k_var*p), 1:(k_var*(p-1))] <- diag(k_var*(p-1))
  max_root <- max(Mod(eigen(C, only.values = TRUE)$values))
  
  Y_combined <- rbind(Y_tv, Y_te)
  n_tv <- nrow(Y_tv); n_te <- nrow(Y_te)
  preds_std <- matrix(NA_real_, n_te, k_var,
                      dimnames = list(NULL, colnames(Y_tv)))
  for (i in seq_len(n_te)) {
    preds_std[i, ] <- forecast_one_step(
      Y_combined[seq_len(n_tv + i - 1), , drop = FALSE], beta_pt, p)
  }
  preds_orig <- sweep(preds_std, 2, sds_tv, "*")
  preds_orig <- sweep(preds_orig, 2, means_tv, "+")
  
  infl_pred <- preds_orig[, "Inflacja"]
  infl_real <- Y_te_raw[, "Inflacja"]
  
  RMSE <- sqrt(mean((infl_pred - infl_real)^2))
  MAE  <- mean(abs(infl_pred - infl_real))
  MAPE <- mean(abs((infl_pred - infl_real) / infl_real)) * 100
  
  naive_pred <- rep(Y_tv_raw[n_tv, "Inflacja"], n_te)
  RMSE_naive <- sqrt(mean((naive_pred - infl_real)^2))
  TheilU <- RMSE / RMSE_naive
  
  lambda_eff <- if (tryb == "hierarchical") mean(bv_fit$hyper[, "lambda"]) else lambda_val
  
  list(zmienne = zmienne, p = p, lambda = lambda_eff, tryb = tryb,
       max_root = max_root, k_var = k_var,
       infl_pred = infl_pred, infl_real = infl_real,
       RMSE = RMSE, MAE = MAE, MAPE = MAPE, TheilU = TheilU,
       bv_fit = bv_fit, beta_pt = beta_pt,
       Y_tv = Y_tv, Y_te = Y_te,
       Y_tv_raw = Y_tv_raw, Y_te_raw = Y_te_raw)
}


# -------------------------------------------------------------
# 3.3 Diagnostyka wielkości modeli BVAR
# -------------------------------------------------------------
T_obs <- nrow(train_diff)
cat("==============================================================\n")
cat("DIAGNOSTYKA WIELKOŚCI MODELI BVAR\n")
cat("==============================================================\n")
for (nazwa_w in c("Mały (5 zm.)", "Średni (10 zm.)", "Duży (23 zm.)")) {
  if (grepl("Mały", nazwa_w))   { k <- 5;  p_use <- 4 }
  if (grepl("Średni", nazwa_w)) { k <- 10; p_use <- 2 }
  if (grepl("Duży", nazwa_w))   { k <- 23; p_use <- 4 }
  n_param <- k * p_use + 1
  ratio <- T_obs / n_param
  cat(sprintf("  %s, p=%d : %d parametrów/równanie, T/(k*p)=%.2f %s\n",
              nazwa_w, p_use, n_param, ratio,
              if (ratio > 1.5) "(OK)" else if (ratio > 1) "(napięte)" else "(za ciasne)"))
}
cat("\n")


# -------------------------------------------------------------
# 3.4 Selekcja hyperparametru lambda na walidacji - 3 warianty
# -------------------------------------------------------------
lambda_grid <- c(0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0)

# Wariant A
cat("==============================================================\n")
cat("WARIANT A: BVAR MAŁY (5 zm., p=4)\n")
cat("==============================================================\n\n")

cat("--- A.1: Siatka lambda + SOC + SUR ---\n")
set.seed(123)
wyniki_maly_grid <- list()
for (lam in lambda_grid) {
  cat(sprintf("  lambda = %.3f ... ", lam))
  o <- ocen_bvar(zmienne = zmienne_maly, p = 4, lambda_val = lam,
                 tryb = "minnesota_socsur",
                 train_diff = train_diff, val_diff = val_diff)
  if (is.null(o)) { cat("BLAD\n"); next }
  wyniki_maly_grid[[as.character(lam)]] <- o
  cat(sprintf("RMSE_val=%.4f, max_root=%.3f %s\n",
              o$RMSE_val, o$max_root,
              if (o$stable) "(stab)" else "(NIESTAB)"))
}
ranking_maly_grid <- build_ranking(wyniki_maly_grid)
cat("\nRanking:\n"); print(ranking_maly_grid, row.names = FALSE)
ranking_stab_m <- ranking_maly_grid[ranking_maly_grid$stable, ]
best_lam_maly_grid <- if (nrow(ranking_stab_m) > 0) ranking_stab_m$lambda[1] else ranking_maly_grid$lambda[1]

cat("\n--- A.2: Hierarchical lambda + SOC + SUR ---\n")
set.seed(123)
wynik_maly_hier <- ocen_bvar(zmienne = zmienne_maly, p = 4,
                             tryb = "hierarchical",
                             train_diff = train_diff, val_diff = val_diff,
                             n_draws = 10000, n_burn = 5000)
cat(sprintf("  lambda_posterior = %.4f, RMSE_val = %.4f, max_root = %.3f %s\n",
            wynik_maly_hier$lambda_eff, wynik_maly_hier$RMSE_val,
            wynik_maly_hier$max_root,
            if (wynik_maly_hier$stable) "(stab)" else "(NIESTAB)"))


# Wariant B
cat("\n==============================================================\n")
cat("WARIANT B: BVAR ŚREDNI (10 zm., p=2)\n")
cat("==============================================================\n\n")

cat("--- B.1: Siatka lambda + SOC + SUR ---\n")
set.seed(123)
wyniki_sredni_grid <- list()
for (lam in lambda_grid) {
  cat(sprintf("  lambda = %.3f ... ", lam))
  o <- ocen_bvar(zmienne = zmienne_sredni, p = 2, lambda_val = lam,
                 tryb = "minnesota_socsur",
                 train_diff = train_diff, val_diff = val_diff)
  if (is.null(o)) { cat("BLAD\n"); next }
  wyniki_sredni_grid[[as.character(lam)]] <- o
  cat(sprintf("RMSE_val=%.4f, max_root=%.3f %s\n",
              o$RMSE_val, o$max_root,
              if (o$stable) "(stab)" else "(NIESTAB)"))
}
ranking_sredni_grid <- build_ranking(wyniki_sredni_grid)
cat("\nRanking:\n"); print(ranking_sredni_grid, row.names = FALSE)
ranking_stab_s <- ranking_sredni_grid[ranking_sredni_grid$stable, ]
best_lam_sredni_grid <- if (nrow(ranking_stab_s) > 0) ranking_stab_s$lambda[1] else ranking_sredni_grid$lambda[1]

cat("\n--- B.2: Hierarchical lambda + SOC + SUR ---\n")
set.seed(123)
wynik_sredni_hier <- ocen_bvar(zmienne = zmienne_sredni, p = 2,
                               tryb = "hierarchical",
                               train_diff = train_diff, val_diff = val_diff,
                               n_draws = 10000, n_burn = 5000)
cat(sprintf("  lambda_posterior = %.4f, RMSE_val = %.4f, max_root = %.3f %s\n",
            wynik_sredni_hier$lambda_eff, wynik_sredni_hier$RMSE_val,
            wynik_sredni_hier$max_root,
            if (wynik_sredni_hier$stable) "(stab)" else "(NIESTAB)"))


# Wariant C
cat("\n==============================================================\n")
cat("WARIANT C: BVAR DUŻY (23 zm., p=4) - kontrolny\n")
cat("==============================================================\n\n")

set.seed(123)
wyniki_duzy <- list()
for (lam in lambda_grid) {
  cat(sprintf("  lambda = %.3f ... ", lam))
  o <- ocen_bvar(zmienne = zmienne_duzy, p = 4, lambda_val = lam,
                 tryb = "minnesota_grid",
                 train_diff = train_diff, val_diff = val_diff)
  if (is.null(o)) { cat("BLAD\n"); next }
  wyniki_duzy[[as.character(lam)]] <- o
  cat(sprintf("RMSE_val=%.4f, max_root=%.3f %s\n",
              o$RMSE_val, o$max_root,
              if (o$stable) "(stab)" else "(NIESTAB)"))
}
ranking_duzy <- build_ranking(wyniki_duzy)
cat("\nRanking:\n"); print(ranking_duzy, row.names = FALSE)
ranking_stab_d <- ranking_duzy[ranking_duzy$stable, ]
best_lam_duzy <- if (nrow(ranking_stab_d) > 0) ranking_stab_d$lambda[1] else ranking_duzy$lambda[1]


# -------------------------------------------------------------
# 3.5 Reestymacja BVAR na train+val + ocena na teście
# -------------------------------------------------------------
cat("\n==============================================================\n")
cat("REESTYMACJA BVAR NA TRAIN+VAL I OCENA NA TEŚCIE\n")
cat("==============================================================\n\n")

cat(">>> A.1: BVAR mały + grid + SOC/SUR ...\n")
final_A_grid <- ocen_finalny_bvar(zmienne = zmienne_maly, p = 4,
                                  lambda_val = best_lam_maly_grid,
                                  tryb = "minnesota_socsur",
                                  train_diff = train_diff,
                                  val_diff = val_diff,
                                  test_diff = test_diff)
cat(sprintf("  lambda=%.2f | RMSE=%.4f | MAE=%.4f | MAPE=%.4f%% | Theil U=%.4f | max_root=%.3f %s\n\n",
            final_A_grid$lambda, final_A_grid$RMSE, final_A_grid$MAE,
            final_A_grid$MAPE, final_A_grid$TheilU, final_A_grid$max_root,
            if (final_A_grid$max_root < 1) "(stab)" else "(NIESTAB)"))

cat(">>> A.2: BVAR mały + hierarchical + SOC/SUR ...\n")
final_A_hier <- ocen_finalny_bvar(zmienne = zmienne_maly, p = 4,
                                  tryb = "hierarchical",
                                  train_diff = train_diff,
                                  val_diff = val_diff,
                                  test_diff = test_diff)
cat(sprintf("  lambda_post=%.4f | RMSE=%.4f | MAE=%.4f | MAPE=%.4f%% | Theil U=%.4f | max_root=%.3f %s\n\n",
            final_A_hier$lambda, final_A_hier$RMSE, final_A_hier$MAE,
            final_A_hier$MAPE, final_A_hier$TheilU, final_A_hier$max_root,
            if (final_A_hier$max_root < 1) "(stab)" else "(NIESTAB)"))

cat(">>> B.1: BVAR średni + grid + SOC/SUR ...\n")
final_B_grid <- ocen_finalny_bvar(zmienne = zmienne_sredni, p = 2,
                                  lambda_val = best_lam_sredni_grid,
                                  tryb = "minnesota_socsur",
                                  train_diff = train_diff,
                                  val_diff = val_diff,
                                  test_diff = test_diff)
cat(sprintf("  lambda=%.2f | RMSE=%.4f | MAE=%.4f | MAPE=%.4f%% | Theil U=%.4f | max_root=%.3f %s\n\n",
            final_B_grid$lambda, final_B_grid$RMSE, final_B_grid$MAE,
            final_B_grid$MAPE, final_B_grid$TheilU, final_B_grid$max_root,
            if (final_B_grid$max_root < 1) "(stab)" else "(NIESTAB)"))

cat(">>> B.2: BVAR średni + hierarchical + SOC/SUR ...\n")
final_B_hier <- ocen_finalny_bvar(zmienne = zmienne_sredni, p = 2,
                                  tryb = "hierarchical",
                                  train_diff = train_diff,
                                  val_diff = val_diff,
                                  test_diff = test_diff)
cat(sprintf("  lambda_post=%.4f | RMSE=%.4f | MAE=%.4f | MAPE=%.4f%% | Theil U=%.4f | max_root=%.3f %s\n\n",
            final_B_hier$lambda, final_B_hier$RMSE, final_B_hier$MAE,
            final_B_hier$MAPE, final_B_hier$TheilU, final_B_hier$max_root,
            if (final_B_hier$max_root < 1) "(stab)" else "(NIESTAB)"))

cat(">>> C: BVAR duży (kontrolny) ...\n")
final_C <- ocen_finalny_bvar(zmienne = zmienne_duzy, p = 4,
                             lambda_val = best_lam_duzy,
                             tryb = "minnesota_grid",
                             train_diff = train_diff,
                             val_diff = val_diff,
                             test_diff = test_diff)
cat(sprintf("  lambda=%.2f | RMSE=%.4f | MAE=%.4f | MAPE=%.4f%% | Theil U=%.4f | max_root=%.3f %s\n\n",
            final_C$lambda, final_C$RMSE, final_C$MAE,
            final_C$MAPE, final_C$TheilU, final_C$max_root,
            if (final_C$max_root < 1) "(stab)" else "(NIESTAB)"))


# -------------------------------------------------------------
# 3.6 Podsumowanie wszystkich BVAR-ów + wybór najlepszego
# -------------------------------------------------------------
podsumowanie_bvar <- data.frame(
  Model = c("BVAR(4) maly + grid",
            "BVAR(4) maly + hierarch",
            "BVAR(2) sredni + grid",
            "BVAR(2) sredni + hierarch",
            "BVAR(4) duzy (kontrolny)"),
  k_zm  = c(5, 5, 10, 10, 23),
  p     = c(4, 4, 2, 2, 4),
  lambda = c(final_A_grid$lambda, final_A_hier$lambda,
             final_B_grid$lambda, final_B_hier$lambda, final_C$lambda),
  RMSE  = round(c(final_A_grid$RMSE, final_A_hier$RMSE,
                  final_B_grid$RMSE, final_B_hier$RMSE, final_C$RMSE), 4),
  MAE   = round(c(final_A_grid$MAE, final_A_hier$MAE,
                  final_B_grid$MAE, final_B_hier$MAE, final_C$MAE), 4),
  MAPE  = round(c(final_A_grid$MAPE, final_A_hier$MAPE,
                  final_B_grid$MAPE, final_B_hier$MAPE, final_C$MAPE), 4),
  Theil_U = round(c(final_A_grid$TheilU, final_A_hier$TheilU,
                    final_B_grid$TheilU, final_B_hier$TheilU, final_C$TheilU), 4),
  max_root = round(c(final_A_grid$max_root, final_A_hier$max_root,
                     final_B_grid$max_root, final_B_hier$max_root, final_C$max_root), 3),
  Stabil = c(if (final_A_grid$max_root < 1) "TAK" else "NIE",
             if (final_A_hier$max_root < 1) "TAK" else "NIE",
             if (final_B_grid$max_root < 1) "TAK" else "NIE",
             if (final_B_hier$max_root < 1) "TAK" else "NIE",
             if (final_C$max_root < 1) "TAK" else "NIE")
)

cat("==============================================================\n")
cat("PODSUMOWANIE WSZYSTKICH BVAR-ów\n")
cat("==============================================================\n")
print(podsumowanie_bvar, row.names = FALSE)


# Wybór najlepszego stabilnego BVAR
podsumowanie_stab <- podsumowanie_bvar[podsumowanie_bvar$Stabil == "TAK", ]
podsumowanie_stab_sorted <- podsumowanie_stab[order(podsumowanie_stab$RMSE), ]
cat("\n=== Najlepszy stabilny BVAR: ===\n")
print(podsumowanie_stab_sorted[1, ], row.names = FALSE)

best_label_bvar <- as.character(podsumowanie_stab_sorted$Model[1])
final_best_bvar <- switch(best_label_bvar,
                          "BVAR(4) maly + grid"      = final_A_grid,
                          "BVAR(4) maly + hierarch"  = final_A_hier,
                          "BVAR(2) sredni + grid"    = final_B_grid,
                          "BVAR(2) sredni + hierarch" = final_B_hier,
                          final_A_grid)


# -------------------------------------------------------------
# 3.7 Tabela prognoz, wykres, IRF, FEVD dla najlepszego BVAR
# -------------------------------------------------------------
n_te_bvar <- length(final_best_bvar$infl_pred)
test_dates_bvar <- as.yearqtr(time(test_data))[seq_len(n_te_bvar)]

tabela_bvar <- data.frame(
  Kwartal     = test_dates_bvar,
  Rzeczywista = round(final_best_bvar$infl_real, 3),
  Prognoza    = round(final_best_bvar$infl_pred, 3),
  Bład        = round(final_best_bvar$infl_pred - final_best_bvar$infl_real, 3)
)
cat("\n=== Prognozy najlepszego BVAR ===\n")
print(tabela_bvar, row.names = FALSE)

par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3.5, 1))
plot(test_dates_bvar, final_best_bvar$infl_real,
     type = "o", pch = 16, col = "black", lwd = 2,
     xlab = "Kwartal",
     ylab = "Inflacja kw/kw",
     main = sprintf("%s: prognoza vs rzeczywistosc\nRMSE=%.3f, Theil U=%.3f",
                    best_label_bvar, final_best_bvar$RMSE, final_best_bvar$TheilU),
     ylim = range(c(final_best_bvar$infl_real, final_best_bvar$infl_pred)) + c(-0.5, 0.5))
lines(test_dates_bvar, final_best_bvar$infl_pred, type = "o", pch = 17, col = "darkgreen", lwd = 2)
abline(h = 100, lty = 3, col = "grey60")
legend("topright", legend = c("Rzeczywista", "Prognoza BVAR"),
       col = c("black", "darkgreen"), pch = c(16, 17), lty = 1, lwd = 2, bty = "n")


cat("\n=== IRF dla najlepszego BVAR ===\n")
# Struktura BVAR irf$quants: [quantile, horizon, response_var, shock_var]
irf_bvar <- BVAR::irf(final_best_bvar$bv_fit, horizon = 12, conf_bands = c(0.05, 0.95))
cat("Wymiary irf$quants:", paste(dim(irf_bvar$quants), collapse=" x "), "\n")

n_horizon_irf <- dim(irf_bvar$quants)[2]
horizon_axis_irf <- 0:(n_horizon_irf - 1)
infl_idx <- 1
n_shocks_bvar <- final_best_bvar$k_var
ncol_plot_bvar <- min(3, n_shocks_bvar)
nrow_plot_bvar <- ceiling(n_shocks_bvar / ncol_plot_bvar)

par(mfrow = c(nrow_plot_bvar, ncol_plot_bvar), mar = c(3.5, 4, 2.5, 1))
for (sh_idx in seq_len(n_shocks_bvar)) {
  med <- irf_bvar$quants[2, , infl_idx, sh_idx]
  low <- irf_bvar$quants[1, , infl_idx, sh_idx]
  upp <- irf_bvar$quants[3, , infl_idx, sh_idx]
  
  plot(horizon_axis_irf, med, type = "l", lwd = 2.5, col = "darkgreen",
       xlab = "Kwartaly po szoku", ylab = "Reakcja inflacji",
       main = sprintf("BVAR - Szok: %s", colnames(final_best_bvar$Y_tv)[sh_idx]),
       ylim = range(c(low, upp, 0)))
  polygon(c(horizon_axis_irf, rev(horizon_axis_irf)), c(low, rev(upp)),
          col = adjustcolor("darkgreen", alpha.f = 0.15), border = NA)
  abline(h = 0, lty = 3, col = "grey40")
}
par(mfrow = c(1, 1))


cat("\n=== FEVD dla najlepszego BVAR ===\n")
# Struktura BVAR fevd$quants: [quantile, response_var, horizon, shock_var]
fevd_bvar <- BVAR::fevd(final_best_bvar$bv_fit, horizon = 13)
cat("Wymiary fevd$quants:", paste(dim(fevd_bvar$quants), collapse=" x "), "\n")

n_h_fevd <- dim(fevd_bvar$quants)[3]
fevd_med <- fevd_bvar$quants[2, infl_idx, , ]
colnames(fevd_med) <- colnames(final_best_bvar$Y_tv)
rownames(fevd_med) <- paste0("h=", 0:(n_h_fevd - 1))

cat("\nPełna FEVD inflacji:\n")
print(round(fevd_med, 3))

cat("\n=== Skrót: horyzonty 1, 4, 8, 12 ===\n")
h_to_show_idx <- c(2, 5, 9, 13)
h_to_show_idx <- h_to_show_idx[h_to_show_idx <= n_h_fevd]
cat("Horyzonty (kwartały):", paste(h_to_show_idx - 1, collapse=", "), "\n")
print(round(fevd_med[h_to_show_idx, , drop = FALSE], 3))


###############################################################
#                                                             #
#  SEKCJA 4: MODEL FAVAR                                      #
#                                                             #
#  Trzy warianty:                                             #
#    Wariant 0: klasyczny FAVAR (panel 22 zm., bez observable)#
#    Wariant A: z observable Stopa_ref                        #
#               (panel 21 zm., system [Infl, Stopa_ref, F])   #
#    Wariant B: targeted predictors                           #
#               (panel = zmienne z istotnym screeningiem)     #
#                                                             #
#  Selekcja (k, p): pełny grid search k x p                   #
#                                                             #
###############################################################

cat("\n\n###############################################################\n")
cat("# SEKCJA 4: MODEL FAVAR                                        #\n")
cat("###############################################################\n\n")

stopa_name <- "Stopa referencyjna"


# -------------------------------------------------------------
# 4.1 Funkcje pomocnicze
# -------------------------------------------------------------

# Targeted predictor screening (Bai-Ng 2008)
# Dla kazdej zmiennej x: regresja
#   inflacja_t = a + b*x_{t-1} + c*inflacja_{t-1} + e
target_screening <- function(infl_name, panel_vars, train_diff, alpha_thr = 0.20) {
  y <- as.numeric(train_diff[, infl_name]); n <- length(y)
  results <- data.frame(
    Zmienna_full = panel_vars,
    Zmienna = nazwy_skrocone[panel_vars],
    t_stat = NA_real_, p_value = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(panel_vars)) {
    x <- as.numeric(train_diff[, panel_vars[i]])
    df <- data.frame(y_t = y[2:n], x_l1 = x[1:(n-1)], y_l1 = y[1:(n-1)])
    fit <- tryCatch(lm(y_t ~ x_l1 + y_l1, data = df), error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)$coefficients
    if ("x_l1" %in% rownames(s)) {
      results$t_stat[i]  <- s["x_l1", "t value"]
      results$p_value[i] <- s["x_l1", "Pr(>|t|)"]
    }
  }
  results <- results[order(results$p_value), ]
  sel_full <- results$Zmienna_full[!is.na(results$p_value) & results$p_value < alpha_thr]
  list(all_results = results, selected_full = sel_full,
       selected_short = nazwy_skrocone[sel_full],
       n_selected = length(sel_full), alpha = alpha_thr)
}


# Estymacja FAVAR z USTALONYM (k, p) i ocena na walidacji
ocen_favar_fix <- function(k_factors, p, train_diff, val_diff, infl_name,
                           panel_vars, obs_vars = NULL) {
  if (k_factors > length(panel_vars)) return(NULL)
  
  X_tr_raw  <- as.matrix(train_diff[, panel_vars])
  X_val_raw <- as.matrix(val_diff[, panel_vars])
  
  m_tr <- colMeans(X_tr_raw); s_tr <- apply(X_tr_raw, 2, sd); s_tr[s_tr==0] <- 1
  X_tr_std  <- scale(X_tr_raw,  center = m_tr, scale = s_tr)
  X_val_std <- scale(X_val_raw, center = m_tr, scale = s_tr)
  attr(X_tr_std, "scaled:center") <- NULL; attr(X_tr_std, "scaled:scale") <- NULL
  attr(X_val_std, "scaled:center") <- NULL; attr(X_val_std, "scaled:scale") <- NULL
  
  pca_obj  <- prcomp(X_tr_std, center = FALSE, scale. = FALSE)
  cum_v    <- cumsum(pca_obj$sdev^2 / sum(pca_obj$sdev^2))
  loadings <- pca_obj$rotation[, 1:k_factors, drop = FALSE]
  
  F_tr <- X_tr_std %*% loadings; F_val <- X_val_std %*% loadings
  colnames(F_tr) <- paste0("F", 1:k_factors)
  colnames(F_val) <- paste0("F", 1:k_factors)
  
  Infl_tr  <- as.numeric(train_diff[, infl_name])
  Infl_val <- as.numeric(val_diff[, infl_name])
  
  if (!is.null(obs_vars) && length(obs_vars) > 0) {
    Y_obs_tr  <- as.matrix(train_diff[, obs_vars, drop = FALSE])
    Y_obs_val <- as.matrix(val_diff[, obs_vars, drop = FALSE])
    colnames(Y_obs_tr)  <- as.character(nazwy_skrocone[obs_vars])
    colnames(Y_obs_val) <- as.character(nazwy_skrocone[obs_vars])
    Y_tr  <- cbind(Inflacja = Infl_tr,  Y_obs_tr,  F_tr)
    Y_val <- cbind(Inflacja = Infl_val, Y_obs_val, F_val)
  } else {
    Y_tr  <- cbind(Inflacja = Infl_tr,  F_tr)
    Y_val <- cbind(Inflacja = Infl_val, F_val)
  }
  
  k_var <- ncol(Y_tr); n_tr <- nrow(Y_tr)
  n_param_eq <- k_var * p + 1; T_eff <- n_tr - p
  if (T_eff < n_param_eq + 5) return(NULL)
  
  var_obj <- tryCatch(VAR(Y_tr, p = p, type = "const"), error = function(e) NULL)
  if (is.null(var_obj)) return(NULL)
  
  rt <- roots(var_obj); stable <- max(rt) < 1
  s_test <- tryCatch(serial.test(var_obj, lags.pt = 16, type = "PT.asymptotic"),
                     error = function(e) NULL)
  p_serial <- if (is.null(s_test)) NA_real_ else s_test$serial$p.value
  
  if (!stable) {
    return(list(k_factors = k_factors, p = p, k_var = k_var,
                max_root = max(rt), stable = FALSE, p_serial = p_serial,
                var_kum = cum_v[k_factors],
                RMSE_val = NA_real_, MAE_val = NA_real_))
  }
  
  B    <- Bcoef(var_obj)
  beta <- cbind(B[, ncol(B)], B[, 1:(k_var * p)])
  Y_combined <- rbind(Y_tr, Y_val); n_v <- nrow(Y_val)
  preds <- matrix(NA_real_, n_v, k_var, dimnames = list(NULL, colnames(Y_tr)))
  for (i in seq_len(n_v)) {
    preds[i, ] <- forecast_one_step(
      Y_combined[seq_len(n_tr + i - 1), , drop = FALSE], beta, p)
  }
  
  pred_val <- preds[, "Inflacja"]; real_val <- Y_val[, "Inflacja"]
  list(k_factors = k_factors, p = p, k_var = k_var,
       max_root = max(rt), stable = TRUE, p_serial = p_serial,
       var_kum = cum_v[k_factors],
       RMSE_val = sqrt(mean((pred_val - real_val)^2)),
       MAE_val  = mean(abs(pred_val - real_val)))
}


# Reestymacja FAVAR na train+val + ocena na tescie
ocen_finalny_favar_g <- function(k_factors, p,
                                 train_diff, val_diff, test_diff,
                                 infl_name, panel_vars, obs_vars = NULL) {
  trainval_diff_loc <- rbind(train_diff, val_diff)
  X_tv_raw <- as.matrix(trainval_diff_loc[, panel_vars])
  X_te_raw <- as.matrix(test_diff[, panel_vars])
  
  m_tv <- colMeans(X_tv_raw); s_tv <- apply(X_tv_raw, 2, sd); s_tv[s_tv==0] <- 1
  X_tv_std <- scale(X_tv_raw, center = m_tv, scale = s_tv)
  X_te_std <- scale(X_te_raw, center = m_tv, scale = s_tv)
  attr(X_tv_std, "scaled:center") <- NULL; attr(X_tv_std, "scaled:scale") <- NULL
  attr(X_te_std, "scaled:center") <- NULL; attr(X_te_std, "scaled:scale") <- NULL
  
  pca_obj <- prcomp(X_tv_std, center = FALSE, scale. = FALSE)
  loadings <- pca_obj$rotation[, 1:k_factors, drop = FALSE]
  cum_v <- cumsum(pca_obj$sdev^2 / sum(pca_obj$sdev^2))
  
  F_tv <- X_tv_std %*% loadings; F_te <- X_te_std %*% loadings
  colnames(F_tv) <- paste0("F", 1:k_factors)
  colnames(F_te) <- paste0("F", 1:k_factors)
  
  Infl_tv <- as.numeric(trainval_diff_loc[, infl_name])
  Infl_te <- as.numeric(test_diff[, infl_name])
  
  if (!is.null(obs_vars) && length(obs_vars) > 0) {
    Y_obs_tv <- as.matrix(trainval_diff_loc[, obs_vars, drop = FALSE])
    Y_obs_te <- as.matrix(test_diff[, obs_vars, drop = FALSE])
    colnames(Y_obs_tv) <- as.character(nazwy_skrocone[obs_vars])
    colnames(Y_obs_te) <- as.character(nazwy_skrocone[obs_vars])
    Y_tv <- cbind(Inflacja = Infl_tv, Y_obs_tv, F_tv)
    Y_te <- cbind(Inflacja = Infl_te, Y_obs_te, F_te)
  } else {
    Y_tv <- cbind(Inflacja = Infl_tv, F_tv)
    Y_te <- cbind(Inflacja = Infl_te, F_te)
  }
  
  k_var <- ncol(Y_tv)
  var_obj <- tryCatch(VAR(Y_tv, p = p, type = "const"), error = function(e) NULL)
  if (is.null(var_obj)) return(NULL)
  rt <- roots(var_obj); stable <- max(rt) < 1
  s_test <- tryCatch(serial.test(var_obj, lags.pt = 16, type = "PT.asymptotic"),
                     error = function(e) NULL)
  p_ser <- if (is.null(s_test)) NA_real_ else s_test$serial$p.value
  
  B    <- Bcoef(var_obj)
  beta <- cbind(B[, ncol(B)], B[, 1:(k_var * p)])
  Y_combined <- rbind(Y_tv, Y_te); n_tv <- nrow(Y_tv); n_te <- nrow(Y_te)
  preds <- matrix(NA_real_, n_te, k_var, dimnames = list(NULL, colnames(Y_tv)))
  for (i in seq_len(n_te)) {
    preds[i, ] <- forecast_one_step(
      Y_combined[seq_len(n_tv + i - 1), , drop = FALSE], beta, p)
  }
  infl_pred <- preds[, "Inflacja"]; infl_real <- Y_te[, "Inflacja"]
  
  RMSE   <- sqrt(mean((infl_pred - infl_real)^2))
  MAE    <- mean(abs(infl_pred - infl_real))
  MAPE   <- mean(abs((infl_pred - infl_real) / infl_real)) * 100
  naive  <- rep(Y_tv[n_tv, "Inflacja"], n_te)
  TheilU <- RMSE / sqrt(mean((naive - infl_real)^2))
  
  list(k_factors = k_factors, p = p, k_var = k_var,
       max_root = max(rt), stable = stable, p_serial = p_ser,
       var_kum = cum_v[k_factors],
       RMSE = RMSE, MAE = MAE, MAPE = MAPE, TheilU = TheilU,
       infl_pred = infl_pred, infl_real = infl_real,
       var_obj = var_obj, Y_tv = Y_tv, Y_te = Y_te,
       loadings = loadings)
}


# Grid search po (k, p) z wyborem stabilnego modelu z dobrym Portmanteau
grid_favar <- function(label, train_diff, val_diff, test_diff,
                       infl_name, panel_vars, obs_vars = NULL,
                       k_grid = 2:6, p_grid = 1:4) {
  
  cat("\n--- WARIANT:", label, "---\n")
  cat("  Panel PCA:", length(panel_vars), "zmiennych\n")
  if (!is.null(obs_vars))
    cat("  Observable:", paste(nazwy_skrocone[obs_vars], collapse=", "), "\n")
  cat(sprintf("  Grid: k in {%s}, p in {%s}\n",
              paste(k_grid, collapse=","), paste(p_grid, collapse=",")))
  
  results <- list()
  for (k_f in k_grid) {
    for (p_f in p_grid) {
      o <- ocen_favar_fix(k_f, p_f, train_diff, val_diff, infl_name,
                          panel_vars, obs_vars)
      if (is.null(o)) next
      results[[paste0("k", k_f, "_p", p_f)]] <- o
    }
  }
  if (length(results) == 0) {
    cat("  >>> Zaden model nie zostal oszacowany\n"); return(NULL)
  }
  
  ranking <- do.call(rbind, lapply(results, function(x) {
    data.frame(k = x$k_factors, p = x$p,
               kum = round(100*x$var_kum, 2),
               max_root = round(x$max_root, 4),
               stable = x$stable,
               p_ser = round(x$p_serial, 4),
               RMSE_val = round(x$RMSE_val, 4))
  }))
  ranking <- ranking[order(ranking$RMSE_val, na.last = TRUE), ]
  cat("\n  Pelny ranking (k x p):\n")
  print(ranking, row.names = FALSE)
  
  rs <- ranking[which(ranking$stable & !is.na(ranking$RMSE_val)), ]
  if (nrow(rs) == 0) {
    cat("  >>> Brak STABILNEGO modelu w gridzie\n")
    return(list(label = label, ranking = ranking, best_fin = NULL,
                panel_vars = panel_vars, obs_vars = obs_vars))
  }
  
  cat("\n  Sprawdzanie po reestymacji na train+val:\n")
  best_fin <- NULL
  for (j in seq_len(nrow(rs))) {
    fin <- tryCatch(
      ocen_finalny_favar_g(rs$k[j], rs$p[j], train_diff, val_diff, test_diff,
                           infl_name, panel_vars, obs_vars),
      error = function(e) NULL)
    if (is.null(fin)) next
    cat(sprintf("  rank=%d, k=%d, p=%d: max_root_tv=%.3f, p_Portm_tv=%.4f, RMSE_test=%.3f %s\n",
                j, rs$k[j], rs$p[j], fin$max_root,
                ifelse(is.na(fin$p_serial), -1, fin$p_serial), fin$RMSE,
                if (fin$stable && !is.na(fin$p_serial) && fin$p_serial > 0.05)
                  "*** WYBRANY" else
                    if (fin$stable) "(stab, Portm zly)" else "(NIESTAB)"))
    if (is.null(best_fin) && fin$stable &&
        !is.na(fin$p_serial) && fin$p_serial > 0.05) {
      best_fin <- fin
    }
  }
  if (is.null(best_fin)) {
    cat("  Fallback: brak modelu z Portmanteau OK -> pierwszy stabilny po RMSE_val\n")
    best_fin <- ocen_finalny_favar_g(rs$k[1], rs$p[1],
                                     train_diff, val_diff, test_diff,
                                     infl_name, panel_vars, obs_vars)
  }
  cat(sprintf("\n  >>> WYBRANY: k=%d, p=%d, RMSE=%.4f, Theil=%.4f, max_root=%.3f %s\n",
              best_fin$k_factors, best_fin$p, best_fin$RMSE, best_fin$TheilU,
              best_fin$max_root,
              if (best_fin$stable) "(stab)" else "(NIESTAB)"))
  list(label = label, best_fin = best_fin, ranking = ranking,
       panel_vars = panel_vars, obs_vars = obs_vars)
}


# -------------------------------------------------------------
# 4.2 Wariant 0: klasyczny FAVAR (panel = 22 zmiennych)
# -------------------------------------------------------------
cat("==============================================================\n")
cat("WARIANT 0: KLASYCZNY (panel = 22 zm.)\n")
cat("==============================================================\n")
zmienne_panel_0 <- setdiff(colnames(train_diff), infl_name)
wynik_0 <- grid_favar("Wariant 0 (klasyczny)",
                      train_diff, val_diff, test_diff, infl_name,
                      panel_vars = zmienne_panel_0, obs_vars = NULL)


# -------------------------------------------------------------
# 4.3 Wariant A: FAVAR z observable Stopa_ref (panel = 21 zm.)
# -------------------------------------------------------------
cat("\n==============================================================\n")
cat("WARIANT A: Z OBSERVABLE Stopa_ref (panel = 21 zm.)\n")
cat("==============================================================\n")
zmienne_panel_A <- setdiff(zmienne_panel_0, stopa_name)
wynik_A <- grid_favar("Wariant A (z observable Stopa_ref)",
                      train_diff, val_diff, test_diff, infl_name,
                      panel_vars = zmienne_panel_A, obs_vars = stopa_name)


# -------------------------------------------------------------
# 4.4 Wariant B: targeted predictors
# -------------------------------------------------------------
cat("\n==============================================================\n")
cat("WARIANT B: TARGETED PREDICTORS (Bai-Ng 2008)\n")
cat("==============================================================\n")
screen <- target_screening(infl_name, zmienne_panel_0, train_diff, alpha_thr = 0.20)
cat("\n--- Screening top 10 (sortowane po p-value) ---\n")
print(head(screen$all_results[, c("Zmienna", "t_stat", "p_value")], 10),
      row.names = FALSE)
cat(sprintf("\nWybrane zmienne (p < %.2f): %d\n", screen$alpha, screen$n_selected))
cat("  ", paste(screen$selected_short, collapse = ", "), "\n")

zmienne_panel_B <- screen$selected_full
wynik_B <- grid_favar("Wariant B (targeted predictors)",
                      train_diff, val_diff, test_diff, infl_name,
                      panel_vars = zmienne_panel_B, obs_vars = NULL)


# -------------------------------------------------------------
# 4.5 Porownanie wariantow + wybor zwyciezcy
# -------------------------------------------------------------
cat("\n==============================================================\n")
cat("PORÓWNANIE WSZYSTKICH WARIANTÓW FAVAR\n")
cat("==============================================================\n\n")

zbierz_favar <- function(w) {
  if (is.null(w) || is.null(w$best_fin)) return(NULL)
  fin <- w$best_fin
  data.frame(
    Wariant = w$label, k = fin$k_factors, p = fin$p,
    k_panel = length(w$panel_vars),
    k_obs = if (is.null(w$obs_vars)) 0 else length(w$obs_vars),
    RMSE = round(fin$RMSE, 4), MAE = round(fin$MAE, 4),
    MAPE = round(fin$MAPE, 4), TheilU = round(fin$TheilU, 4),
    max_root = round(fin$max_root, 3),
    p_Portm = round(fin$p_serial, 4),
    Stabil = if (fin$stable) "TAK" else "NIE"
  )
}

porownanie_favar_list <- Filter(Negate(is.null),
                                list(zbierz_favar(wynik_0),
                                     zbierz_favar(wynik_A),
                                     zbierz_favar(wynik_B)))
porownanie_favar <- do.call(rbind, porownanie_favar_list)
print(porownanie_favar, row.names = FALSE)

porownanie_favar_stab <- porownanie_favar[which(porownanie_favar$Stabil == "TAK"), ]
if (nrow(porownanie_favar_stab) > 0) {
  porownanie_winner_favar <- porownanie_favar_stab[order(porownanie_favar_stab$RMSE), ][1, ]
  cat("\n>>> NAJLEPSZY STABILNY WARIANT FAVAR:\n")
} else {
  cat("\n>>> UWAGA: zaden wariant nie jest w pelni stabilny.\n")
  porownanie_filt_favar <- porownanie_favar[!is.na(porownanie_favar$RMSE), ]
  porownanie_winner_favar <- porownanie_filt_favar[order(porownanie_filt_favar$RMSE), ][1, ]
}
print(porownanie_winner_favar, row.names = FALSE)

zwyciezca_favar <- switch(as.character(porownanie_winner_favar$Wariant),
                          "Wariant 0 (klasyczny)" = wynik_0,
                          "Wariant A (z observable Stopa_ref)" = wynik_A,
                          "Wariant B (targeted predictors)" = wynik_B)


# -------------------------------------------------------------
# 4.6 Szczegoly zwyciezcy (diagnostyka, Granger, IRF, FEVD)
# -------------------------------------------------------------
cat("\n==============================================================\n")
cat("SZCZEGÓŁY NAJLEPSZEGO MODELU FAVAR\n")
cat("==============================================================\n\n")

best_favar <- zwyciezca_favar$best_fin
cat("Wariant:", zwyciezca_favar$label, "\n")
cat(sprintf("k=%d czynnikow, p=%d opoznien, k_var=%d (Inflacja + observable + czynniki)\n",
            best_favar$k_factors, best_favar$p, best_favar$k_var))
cat(sprintf("Wariancja kumulowana: %.2f%%\n", 100*best_favar$var_kum))
cat(sprintf("Liczba zmiennych w panelu PCA: %d\n", length(zwyciezca_favar$panel_vars)))

cat("\n=== Diagnostyka FAVAR ===\n")
cat(sprintf("Max |pierw.|: %.4f | stabilny: %s\n",
            best_favar$max_root, best_favar$stable))
if (!is.na(best_favar$p_serial))
  cat(sprintf("Portmanteau: p = %.4f %s\n", best_favar$p_serial,
              if (best_favar$p_serial > 0.05) "OK" else "PROBLEM"))

n_f_favar <- normality.test(best_favar$var_obj)
cat(sprintf("Jarque-Bera multi: p = %.4f %s\n",
            n_f_favar$jb.mul$JB$p.value,
            if (n_f_favar$jb.mul$JB$p.value > 0.05) "OK" else "nie-normalne"))
a_f_favar <- arch.test(best_favar$var_obj, lags.multi = 5)
cat(sprintf("ARCH multi (lags=5): p = %.4f %s\n",
            a_f_favar$arch.mul$p.value,
            if (a_f_favar$arch.mul$p.value > 0.05) "OK" else "heteroskedastyczne"))

# Granger - czy czynniki wnosza informacje o inflacji
factor_names_favar <- paste0("F", 1:best_favar$k_factors)
gr_favar <- tryCatch(causality(best_favar$var_obj, cause = factor_names_favar),
                     error = function(e) NULL)
if (!is.null(gr_favar)) {
  cat(sprintf("\nGranger (czynniki -> Inflacja): F=%.3f, p=%.4f %s\n",
              as.numeric(gr_favar$Granger$statistic),
              as.numeric(gr_favar$Granger$p.value),
              if (gr_favar$Granger$p.value < 0.05) "*** ISTOTNE" else "(nieistotne)"))
}

cat("\n=== Loadings (PC1, PC2) ===\n")
loadings_df <- as.data.frame(round(best_favar$loadings, 4))
loadings_df$Zmienna <- as.character(nazwy_skrocone[rownames(loadings_df)])
loadings_df <- loadings_df[, c("Zmienna", colnames(best_favar$loadings))]
print(loadings_df, row.names = FALSE)

# Tabela prognoz
cat("\n=== Prognozy FAVAR na zbiorze testowym ===\n")
test_dates_favar <- as.yearqtr(time(test_data))[seq_len(length(best_favar$infl_pred))]
tabela_favar <- data.frame(
  Kwartal = test_dates_favar,
  Rzeczywista = round(best_favar$infl_real, 3),
  Prognoza = round(best_favar$infl_pred, 3),
  Bład = round(best_favar$infl_pred - best_favar$infl_real, 3)
)
print(tabela_favar, row.names = FALSE)

# Wykres prognoz
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3.5, 1))
plot(test_dates_favar, best_favar$infl_real,
     type = "o", pch = 16, col = "black", lwd = 2,
     xlab = "Kwartal", ylab = "Inflacja kw/kw",
     main = sprintf("FAVAR (%s):\nRMSE=%.3f, Theil U=%.3f",
                    zwyciezca_favar$label, best_favar$RMSE, best_favar$TheilU),
     ylim = range(c(best_favar$infl_real, best_favar$infl_pred)) + c(-0.5, 0.5))
lines(test_dates_favar, best_favar$infl_pred, type = "o", pch = 17, col = "purple", lwd = 2)
abline(h = 100, lty = 3, col = "grey60")
legend("topright", legend = c("Rzeczywista", "Prognoza FAVAR"),
       col = c("black", "purple"), pch = c(16, 17), lty = 1, lwd = 2, bty = "n")

# IRF (vars::irf bo BVAR nadpisuje generic)
cat("\n=== IRF FAVAR (reakcja inflacji na szoki czynnikowe) ===\n")
irf_favar <- tryCatch(
  vars::irf(best_favar$var_obj, response = "Inflacja", n.ahead = 12,
            ortho = TRUE, boot = TRUE, ci = 0.95, runs = 200),
  error = function(e) { message("IRF error: ", conditionMessage(e)); NULL }
)
if (!is.null(irf_favar)) {
  n_shocks_favar  <- best_favar$k_var
  ncol_plot_favar <- min(3, n_shocks_favar)
  nrow_plot_favar <- ceiling(n_shocks_favar / ncol_plot_favar)
  par(mfrow = c(nrow_plot_favar, ncol_plot_favar), mar = c(3.5, 4, 2.5, 1))
  for (sh in colnames(best_favar$Y_tv)) {
    irf_vals <- irf_favar$irf[[sh]][, "Inflacja"]
    ci_low   <- irf_favar$Lower[[sh]][, "Inflacja"]
    ci_upp   <- irf_favar$Upper[[sh]][, "Inflacja"]
    plot(0:12, irf_vals, type = "l", lwd = 2.5, col = "purple",
         xlab = "Kwartaly po szoku", ylab = "Reakcja inflacji",
         main = sprintf("FAVAR - Szok: %s", sh),
         ylim = range(c(ci_low, ci_upp, 0)))
    polygon(c(0:12, 12:0), c(ci_low, rev(ci_upp)),
            col = adjustcolor("purple", alpha.f = 0.15), border = NA)
    abline(h = 0, lty = 3, col = "grey40")
  }
  par(mfrow = c(1, 1))
}

# FEVD
fevd_favar <- vars::fevd(best_favar$var_obj, n.ahead = 12)
cat("\n=== FEVD inflacji (h=1, 4, 8, 12) ===\n")
print(round(fevd_favar$Inflacja[c(1, 4, 8, 12), ], 3))


###############################################################
#                                                             #
#  SEKCJA 5: BENCHMARK ARIMA                                  #
#                                                             #
###############################################################

cat("\n\n###############################################################\n")
cat("# SEKCJA 5: BENCHMARK ARIMA                                    #\n")
cat("###############################################################\n\n")

# -------------------------------------------------------------
# 5.1 Wydzielenie szeregu inflacji + wyrownanie okresu
# -------------------------------------------------------------
infl_full <- as.numeric(full_ts[, infl_name])
n_test    <- nrow(test_data)

# Inflacja w okresie 2005Q2 - 2022Q4 (train+val) - 71 obs
infl_train_val <- infl_full[2:(n_train_orig + n_val)]
# Inflacja w okresie 2023Q1 - 2025Q3 (test) - 11 obs
infl_test      <- infl_full[(n_train_orig + n_val + 1):length(infl_full)]

cat("Liczba obserwacji po wyrownaniu:\n")
cat("  train+val (2005Q2-2022Q4):", length(infl_train_val), "\n")
cat("  test      (2023Q1-2025Q3):", length(infl_test), "\n\n")

y_tv_arima <- ts(infl_train_val, start = c(2005, 2), frequency = 4)


# -------------------------------------------------------------
# 5.2 Wybor modelu ARIMA na train+val
# -------------------------------------------------------------
# auto.arima z kryterium BIC

set.seed(123)
fit_arima <- auto.arima(
  y_tv_arima,
  ic            = "bic",
  stepwise      = FALSE,
  approximation = FALSE,
  seasonal      = TRUE,
  max.p = 5, max.q = 5, max.P = 2, max.Q = 2, max.d = 2, max.D = 1
)

cat("=== Wybrany model ARIMA ===\n")
print(fit_arima)
cat("\nAIC:", round(AIC(fit_arima), 3),
    " | BIC:", round(BIC(fit_arima), 3),
    " | logLik:", round(as.numeric(logLik(fit_arima)), 3), "\n")

# Diagnostyka reszt
lb_arima <- Box.test(residuals(fit_arima), lag = 16, type = "Ljung-Box",
                     fitdf = length(coef(fit_arima)))
cat(sprintf("Ljung-Box(lag=16): chi2=%.3f, p=%.4f %s\n",
            as.numeric(lb_arima$statistic), lb_arima$p.value,
            if (lb_arima$p.value > 0.05) "OK" else "PROBLEM"))


# -------------------------------------------------------------
# 5.3 Fixed-coefficient rolling 1-step-ahead
# -------------------------------------------------------------

forecasts_arima <- numeric(n_test)
for (i in seq_len(n_test)) {
  if (i == 1) {
    data_so_far <- y_tv_arima
  } else {
    data_so_far <- ts(
      c(as.numeric(y_tv_arima), infl_test[seq_len(i - 1)]),
      start = c(2005, 2), frequency = 4
    )
  }
  refit <- Arima(data_so_far, model = fit_arima)
  forecasts_arima[i] <- as.numeric(forecast(refit, h = 1)$mean[1])
}


# -------------------------------------------------------------
# 5.4 Miary bledow (zgodne z gl. skryptem)
# -------------------------------------------------------------
infl_real_arima <- infl_test
errors_arima    <- forecasts_arima - infl_real_arima

RMSE_arima  <- sqrt(mean(errors_arima^2))
MAE_arima   <- mean(abs(errors_arima))
MAPE_arima  <- mean(abs(errors_arima / infl_real_arima)) * 100
ME_arima    <- mean(errors_arima)

# Naive benchmark: constant forecast = inflacja w 2022Q4
y_T_arima         <- infl_train_val[length(infl_train_val)]
naive_pred_arima  <- rep(y_T_arima, n_test)
RMSE_naive_arima  <- sqrt(mean((naive_pred_arima - infl_real_arima)^2))
TheilU_arima      <- RMSE_arima / RMSE_naive_arima

cat("\n=== Statystyki bledow ARIMA na zbiorze testowym ===\n")
cat(sprintf("RMSE    : %.4f\n", RMSE_arima))
cat(sprintf("MAE     : %.4f\n", MAE_arima))
cat(sprintf("MAPE    : %.4f %%\n", MAPE_arima))
cat(sprintf("Theil U : %.4f %s\n", TheilU_arima,
            if (TheilU_arima < 1) "(model bije naiwna)" else "(naiwa lepsza)"))
cat(sprintf("ME      : %+.4f (obciazenie sredniego bledu)\n", ME_arima))


# -------------------------------------------------------------
# 5.5 Tabela prognoz + wykres ARIMA
# -------------------------------------------------------------
test_dates_arima <- as.yearqtr(time(test_data))[seq_len(n_test)]

tabela_arima <- data.frame(
  Kwartal     = test_dates_arima,
  Rzeczywista = round(infl_real_arima, 3),
  Prognoza    = round(forecasts_arima, 3),
  Bład        = round(errors_arima, 3)
)

cat("\n=== Tabela prognoz ARIMA ===\n")
print(tabela_arima, row.names = FALSE)

write.csv(tabela_arima, "prognozy_ARIMA_test.csv", row.names = FALSE)
write.csv(
  data.frame(Kwartal = test_dates_arima, error_ARIMA = errors_arima),
  "arima_errors.csv", row.names = FALSE
)

# Wykres prognozy ARIMA
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3.5, 1))
plot(test_dates_arima, infl_real_arima,
     type = "o", pch = 16, col = "black", lwd = 2,
     xlab = "Kwartal",
     ylab = "Inflacja kw/kw (poprz. okres = 100)",
     main = sprintf("Benchmark ARIMA: prognoza vs rzeczywistosc\nRMSE=%.3f, Theil U=%.3f",
                    RMSE_arima, TheilU_arima),
     ylim = range(c(infl_real_arima, forecasts_arima)) + c(-0.5, 0.5))
lines(test_dates_arima, forecasts_arima, type = "o", pch = 17, col = "orange", lwd = 2)
abline(h = 100, lty = 3, col = "grey60")
legend("topright",
       legend = c("Rzeczywista", "Prognoza ARIMA"),
       col = c("black", "orange"),
       pch = c(16, 17), lty = 1, lwd = 2, bty = "n")


###############################################################
#                                                             #
#  SEKCJA 7: PORÓWNANIE KOŃCOWE VAR / BVAR / FAVAR / ARIMA    #
#                                                             #
#  Celem jest zweryfikowanie hipotezy H1 (FAVAR > BVAR > VAR) #
#  z ARIMA jako benchmark univariate, oraz statystyczna       #
#  ocena roznic miedzy modelami testem Diebolda-Mariano       #
#                                                             #
###############################################################

cat("\n\n###############################################################\n")
cat("# SEKCJA 6: PORÓWNANIE KOŃCOWE VAR / BVAR / FAVAR / ARIMA      #\n")
cat("###############################################################\n\n")

# -------------------------------------------------------------
# 6.1 Tabela porownawcza wszystkich 4 modeli
# -------------------------------------------------------------
# Skrocony opis modelu ARIMA do tabeli
arima_order_txt <- paste0("ARIMA(", paste(arimaorder(fit_arima)[1:3], collapse=","), ")")
if (length(arimaorder(fit_arima)) > 3) {
  sea <- arimaorder(fit_arima)[4:6]
  if (any(sea > 0)) {
    arima_order_txt <- paste0(arima_order_txt, "(", paste(sea, collapse=","),
                              ")[", frequency(y_tv_arima), "]")
  }
}

porownanie_koncowe <- data.frame(
  Model = c("ARIMA (benchmark univariate)",
            "VAR (klasyczny)",
            sprintf("BVAR (%s)", best_label_bvar),
            sprintf("FAVAR (%s)", zwyciezca_favar$label)),
  Specyfikacja = c(arima_order_txt,
                   sprintf("VAR(%d), %d zm.", p_final_var, k_final_var),
                   sprintf("BVAR(%d), %d zm.", final_best_bvar$p, final_best_bvar$k_var),
                   sprintf("FAVAR(k=%d, p=%d)", best_favar$k_factors, best_favar$p)),
  RMSE = round(c(RMSE_arima, RMSE_var, final_best_bvar$RMSE, best_favar$RMSE), 4),
  MAE  = round(c(MAE_arima, MAE_var, final_best_bvar$MAE, best_favar$MAE), 4),
  MAPE = round(c(MAPE_arima, MAPE_var, final_best_bvar$MAPE, best_favar$MAPE), 4),
  Theil_U = round(c(TheilU_arima, TheilU_var, final_best_bvar$TheilU, best_favar$TheilU), 4)
)
porownanie_koncowe <- porownanie_koncowe[order(porownanie_koncowe$RMSE), ]
cat("=== TABELA PORÓWNAWCZA: ARIMA vs VAR vs BVAR vs FAVAR ===\n")
print(porownanie_koncowe, row.names = FALSE)


# -------------------------------------------------------------
# 6.2 Poprawa wzgledem dwoch benchmarkow:
#     (a) wzgledem ARIMA (najprostszy benchmark univariate)
#     (b) wzgledem VAR  (najprostszy benchmark multivariate)
# -------------------------------------------------------------
cat("\n=== Poprawa RMSE wzgledem benchmarkow ===\n")
poprawa <- data.frame(
  Model = porownanie_koncowe$Model,
  RMSE  = porownanie_koncowe$RMSE,
  Poprawa_vs_ARIMA_proc = round(100 * (porownanie_koncowe$RMSE - RMSE_arima) / RMSE_arima, 1),
  Poprawa_vs_VAR_proc   = round(100 * (porownanie_koncowe$RMSE - RMSE_var) / RMSE_var, 1)
)
print(poprawa, row.names = FALSE)
cat("\nUjemne wartosci = lepiej niz benchmark, dodatnie = gorzej.\n")


# -------------------------------------------------------------
# 6.3 Test Diebolda-Mariano (1995) parami - wszystkie 6 par
#     H0: rownia dokladnosc prognoz
#     Loss function: kwadratowa (power = 2), h = 1
# -------------------------------------------------------------
cat("\n=== TEST DIEBOLDA-MARIANO (parami, 4 modele -> 6 par) ===\n")
cat("H0: rownia dokladnosc prognoz | h=1 step | strata kwadratowa\n")
cat("Korekta na wielokrotne testowanie: metoda Holma (FWER <= 0.05)\n\n")

# Bledy prognoz dla 4 modeli
e_arima <- forecasts_arima - infl_real_arima
e_var   <- infl_pred_var - infl_real_var
e_bvar  <- final_best_bvar$infl_pred - final_best_bvar$infl_real
e_favar <- best_favar$infl_pred - best_favar$infl_real

# Sprawdzenie zgodnosci dlugosci
cat(sprintf("Dlugosc szeregu bledow: ARIMA=%d, VAR=%d, BVAR=%d, FAVAR=%d\n\n",
            length(e_arima), length(e_var), length(e_bvar), length(e_favar)))

# Pary do testowania - wszystkie 6 par dla 4 modeli
pary_dm <- list(
  list("ARIMA vs VAR",   e_arima, e_var),
  list("ARIMA vs BVAR",  e_arima, e_bvar),
  list("ARIMA vs FAVAR", e_arima, e_favar),
  list("VAR   vs BVAR",  e_var,   e_bvar),
  list("VAR   vs FAVAR", e_var,   e_favar),
  list("BVAR  vs FAVAR", e_bvar,  e_favar)
)

# Najpierw policzmy WSZYSTKIE testy i zebrac surowe p-wartosci
dm_raw <- data.frame(
  Para = character(0), DM_stat = numeric(0), p_raw = numeric(0),
  lepszy_idx = integer(0), stringsAsFactors = FALSE
)
for (par_dm in pary_dm) {
  nazwa <- par_dm[[1]]; e1 <- par_dm[[2]]; e2 <- par_dm[[3]]
  test_dm <- tryCatch(
    forecast::dm.test(e1, e2, alternative = "two.sided", h = 1, power = 2),
    error = function(e) { message("DM error (", nazwa, "): ", conditionMessage(e)); NULL }
  )
  if (is.null(test_dm)) next
  dm_raw <- rbind(dm_raw, data.frame(
    Para = nazwa,
    DM_stat = as.numeric(test_dm$statistic),
    p_raw = as.numeric(test_dm$p.value),
    lepszy_idx = if (mean(e1^2) < mean(e2^2)) 1L else 2L,
    stringsAsFactors = FALSE
  ))
}

# Korekta Holma na surowych p-wartosciach
dm_raw$p_holm <- p.adjust(dm_raw$p_raw, method = "holm")

# Wnioski oparte na p_holm
dm_raw$Wniosek <- ifelse(
  dm_raw$p_holm < 0.05,
  ifelse(dm_raw$lepszy_idx == 1, "1. lepszy (Holm p<0.05)", "2. lepszy (Holm p<0.05)"),
  ifelse(dm_raw$p_raw < 0.05,
         "nominalnie istotne, ale Holm odrzuca",
         "brak roznicy")
)

# Wydruk
for (i in seq_len(nrow(dm_raw))) {
  cat(sprintf("  %-18s : DM=%6.3f | p_raw=%.4f | p_holm=%.4f -> %s\n",
              dm_raw$Para[i], dm_raw$DM_stat[i],
              dm_raw$p_raw[i], dm_raw$p_holm[i], dm_raw$Wniosek[i]))
}

# Tabela do zapisu
dm_results <- data.frame(
  Para    = dm_raw$Para,
  DM_stat = round(dm_raw$DM_stat, 4),
  p_raw   = round(dm_raw$p_raw, 4),
  p_holm  = round(dm_raw$p_holm, 4),
  Wniosek = dm_raw$Wniosek,
  stringsAsFactors = FALSE
)

cat("\nUwaga: przy n_test=11 obs. test DM ma ograniczona moc.\n")
cat("Brak istotnosci (zwl. po Holmie) nie wyklucza realnej roznicy w wiekszej probie.\n")
cat("Liczba odrzucen H0 po korekcie: ", sum(dm_raw$p_holm < 0.05),
    " z ", nrow(dm_raw), " par.\n", sep = "")

# -------------------------------------------------------------
# 6.4 Wspolny wykres prognoz vs rzeczywistosc (4 modele)
# -------------------------------------------------------------
test_dates_all <- as.yearqtr(time(test_data))[seq_len(length(infl_real_var))]

par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3.5, 1))
plot(test_dates_all, infl_real_var,
     type = "o", pch = 16, col = "black", lwd = 2.5, cex = 1.2,
     xlab = "Kwartał", ylab = "Inflacja kw/kw (poprz. okres = 100)",
     main = "Porównanie prognoz: ARIMA vs VAR vs BVAR vs FAVAR",
     ylim = range(c(infl_real_var, forecasts_arima, infl_pred_var,
                    final_best_bvar$infl_pred,
                    best_favar$infl_pred)) + c(-0.5, 0.5))
lines(test_dates_all, forecasts_arima,
      type = "o", pch = 4, col = "orange", lwd = 2)
lines(test_dates_all, infl_pred_var,
      type = "o", pch = 17, col = "red", lwd = 2)
lines(test_dates_all, final_best_bvar$infl_pred,
      type = "o", pch = 15, col = "darkgreen", lwd = 2)
lines(test_dates_all, best_favar$infl_pred,
      type = "o", pch = 18, col = "purple", lwd = 2, cex = 1.3)
abline(h = 100, lty = 3, col = "grey60")
legend("topright",
       legend = c("Rzeczywista",
                  sprintf("ARIMA (RMSE=%.3f)", RMSE_arima),
                  sprintf("VAR (RMSE=%.3f)", RMSE_var),
                  sprintf("BVAR (RMSE=%.3f)", final_best_bvar$RMSE),
                  sprintf("FAVAR (RMSE=%.3f)", best_favar$RMSE)),
       col = c("black", "orange", "red", "darkgreen", "purple"),
       pch = c(16, 4, 17, 15, 18), lty = 1, lwd = 2, bty = "n", cex = 0.85)


# -------------------------------------------------------------
# 6.5 Wykres bledow kwadratowych (4 modele)
# -------------------------------------------------------------
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3.5, 1))
plot(test_dates_all, e_arima^2,
     type = "o", pch = 4, col = "orange", lwd = 2,
     xlab = "Kwartał", ylab = "Bład kwadratowy (poziom inflacji)",
     main = "Bledy kwadratowe prognoz w czasie",
     ylim = c(0, max(c(e_arima^2, e_var^2, e_bvar^2, e_favar^2)) * 1.05))
lines(test_dates_all, e_var^2,   type = "o", pch = 17, col = "red", lwd = 2)
lines(test_dates_all, e_bvar^2,  type = "o", pch = 15, col = "darkgreen", lwd = 2)
lines(test_dates_all, e_favar^2, type = "o", pch = 18, col = "purple", lwd = 2, cex = 1.3)
abline(h = 0, lty = 3, col = "grey60")
legend("topright",
       legend = c(sprintf("ARIMA (sr.=%.3f)", mean(e_arima^2)),
                  sprintf("VAR (sr.=%.3f)", mean(e_var^2)),
                  sprintf("BVAR (sr.=%.3f)", mean(e_bvar^2)),
                  sprintf("FAVAR (sr.=%.3f)", mean(e_favar^2))),
       col = c("orange", "red", "darkgreen", "purple"),
       pch = c(4, 17, 15, 18), lty = 1, lwd = 2, bty = "n", cex = 0.85)


# -------------------------------------------------------------
# 6.6 Podsumowanie hipotezy H1 i wnioski
# -------------------------------------------------------------
cat("\n==============================================================\n")
cat("WERYFIKACJA HIPOTEZY H1 (FAVAR > BVAR > VAR)\n")
cat("ARIMA traktowana jako benchmark univariate (nie nalezy do H1)\n")
cat("==============================================================\n\n")

cat("Hierarchia wedlug RMSE (rosnaco - od najlepszego):\n")
for (i in seq_len(nrow(porownanie_koncowe))) {
  cat(sprintf("  %d. %-35s : RMSE=%.4f, Theil U=%.4f\n",
              i, porownanie_koncowe$Model[i],
              porownanie_koncowe$RMSE[i], porownanie_koncowe$Theil_U[i]))
}

# Wszystkie 3 modele wielowymiarowe vs ARIMA (benchmark univariate)
cat("\nCzy modele wielowymiarowe bija benchmark ARIMA?\n")
var_bije_arima   <- RMSE_var          < RMSE_arima
bvar_bije_arima  <- final_best_bvar$RMSE < RMSE_arima
favar_bije_arima <- best_favar$RMSE   < RMSE_arima
cat(sprintf("  VAR   < ARIMA : %s (RMSE: %.3f vs %.3f)\n",
            if (var_bije_arima) "TAK" else "NIE", RMSE_var, RMSE_arima))
cat(sprintf("  BVAR  < ARIMA : %s (RMSE: %.3f vs %.3f)\n",
            if (bvar_bije_arima) "TAK" else "NIE",
            final_best_bvar$RMSE, RMSE_arima))
cat(sprintf("  FAVAR < ARIMA : %s (RMSE: %.3f vs %.3f)\n",
            if (favar_bije_arima) "TAK" else "NIE",
            best_favar$RMSE, RMSE_arima))

# H1: FAVAR > BVAR > VAR
cat("\nWeryfikacja hipotezy H1 (FAVAR > BVAR > VAR):\n")
favar_lepszy_niz_var  <- best_favar$RMSE < RMSE_var
favar_lepszy_niz_bvar <- best_favar$RMSE < final_best_bvar$RMSE
bvar_lepszy_niz_var   <- final_best_bvar$RMSE < RMSE_var

cat(sprintf("  FAVAR > VAR : %s (RMSE: %.3f vs %.3f)\n",
            if (favar_lepszy_niz_var) "TAK" else "NIE",
            best_favar$RMSE, RMSE_var))
cat(sprintf("  FAVAR > BVAR: %s (RMSE: %.3f vs %.3f)\n",
            if (favar_lepszy_niz_bvar) "TAK" else "NIE",
            best_favar$RMSE, final_best_bvar$RMSE))
cat(sprintf("  BVAR > VAR  : %s (RMSE: %.3f vs %.3f)\n",
            if (bvar_lepszy_niz_var) "TAK" else "NIE",
            final_best_bvar$RMSE, RMSE_var))

cat("\nWniosek:\n")
if (favar_lepszy_niz_var && favar_lepszy_niz_bvar) {
  cat("  H1 PEŁNIE POTWIERDZONA: FAVAR > BVAR > VAR\n")
} else if (favar_lepszy_niz_var && bvar_lepszy_niz_var) {
  cat("  H1 CZĘŚCIOWO POTWIERDZONA: oba modele (FAVAR i BVAR)\n")
  cat("  poprawiaja prognozy wzgledem VAR, ale BVAR > FAVAR.\n")
  cat("  Wynik spojny z literatura (Banbura-Giannone-Reichlin 2010,\n")
  cat("  Carriero-Clark-Marcellino 2015) - dla zmiennych nominalnych\n")
  cat("  maly dobrze wyspecyfikowany BVAR bije agregaty czynnikowe.\n")
} else {
  cat("  H1 NIE POTWIERDZONA - wymaga dalszej analizy.\n")
}

if (var_bije_arima && bvar_bije_arima && favar_bije_arima) {
  cat("\n  Dodatkowo: WSZYSTKIE modele wielowymiarowe bija benchmark ARIMA,\n")
  cat("  co potwierdza zasadnosc podejscia multivariate dla inflacji CPI.\n")
} else if (!var_bije_arima && !bvar_bije_arima && !favar_bije_arima) {
  cat("\n  UWAGA: zaden z modeli wielowymiarowych nie bije ARIMA.\n")
  cat("  Sugeruje to, ze dynamika inflacji jest dobrze opisana wlasnymi\n")
  cat("  opoznieniami, a dodawanie zmiennych nie wnosi istotnej informacji.\n")
} else {
  cat("\n  Mieszany wynik wzgledem ARIMA - czesc modeli pobija benchmark.\n")
}


cat("\n\n==============================================================\n")
cat("KONIEC ANALIZY: ARIMA + VAR + BVAR + FAVAR + porownanie\n")
cat("==============================================================\n")