rm(list = ls())

# Wczytanie bibliotek
library(readxl)
library(zoo)
library(tseries)
library(dplyr)
library(tidyr)
library(urca)
library(forecast)

# Wczytanie i przygotowanie danych
data_raw <- read_excel("C:\\Users\\przem\\OneDrive\\Pulpit\\dane_mgr.xlsx")
data_raw$Data <- as.yearqtr(data_raw$Data, format = "%Y Q%q")
ts_data <- ts(data_raw[, -1], start = c(2005, 1), frequency = 4)
ts_data[ts_data == 0] <- NA
ts_data_clean <- na.omit(ts_data)

# Podział na zbiór uczący i testowy
train_end <- c(2020, 4)
test_start <- c(2021, 1)
train_data <- window(ts_data_clean, end = train_end)
test_data  <- window(ts_data_clean, start = test_start)

# Lista zmiennych, które nie powinny być testowane z trendem (tylko stała)
zmienne_bez_trendu <- c(
  "Inflacja [ogółem - okres poprzedni = 100]",
  "Stopa referencyjna",
  "Stopa bezrobocia rejestrowanego (stan w końcu okresu)f [%]",
  "Stopa oprocentowania depozytów złotowych gospodarstw domowych i instytucji niekomercyjnych działających na rzecz gospodarstw domowych w bankach komercyjnych (na rachunkach bieżących) [%]",
  "Stopa oprocentowania depozytów złotowych gospodarstw domowych i instytucji niekomercyjnych działających na rzecz gospodarstw domowych w bankach komercyjnych (z terminem pierwotnym do 2 lat włącznie) [%]",
  "Wskaźnik ogólnego klimatu koniunktury w przetwórstwie przemysłowym (w miesiącu kończącym okres)"
  # Możesz dodać inne zmienne, które są stopami/procentami/indeksami bez trendu
)

# Funkcja testująca stacjonarność z wyborem specyfikacji
test_stacjonarnosc <- function(x, nazwa_zmiennej = "Zmienna", spec = "trend") {
  cat("\n=== Testy dla:", nazwa_zmiennej, "===\n")
  
  x_clean <- na.omit(x)
  if(length(x_clean) < 20) {
    warning("Za mało obserwacji dla: ", nazwa_zmiennej)
    return(NULL)
  }
  
  dx <- diff(x_clean)
  options(scipen = 4)
  
  # Ustal typ testu na poziomie
  if (spec == "trend") {
    adf_type <- "trend"
    pp_model <- "trend"
    kpss_type <- "tau"
  } else if (spec == "drift") {
    adf_type <- "drift"
    pp_model <- "constant"
    kpss_type <- "mu"
  } else {
    stop("spec musi być 'trend' lub 'drift'")
  }
  
  # Testy na poziomie
  adf_level <- ur.df(x_clean, type = adf_type, selectlags = "BIC")
  pp_level <- ur.pp(x_clean, type = "Z-tau", model = pp_model)
  kpss_level <- ur.kpss(x_clean, type = kpss_type)
  
  # Testy na pierwszych różnicach (zawsze ze stałą)
  adf_diff <- ur.df(dx, type = "drift", selectlags = "BIC")
  pp_diff  <- ur.pp(dx, type = "Z-tau", model = "constant")
  kpss_diff <- ur.kpss(dx, type = "mu")
  
  # Wyświetlanie wyników
  cat("\n[ADF] Poziom:\n"); print(summary(adf_level))
  cat("\n[ADF] Pierwsza różnica:\n"); print(summary(adf_diff))
  cat("\n[PP] Poziom:\n"); print(summary(pp_level))
  cat("\n[PP] Pierwsza różnica:\n"); print(summary(pp_diff))
  cat("\n[KPSS] Poziom:\n"); print(summary(kpss_level))
  cat("\n[KPSS] Pierwsza różnica:\n"); print(summary(kpss_diff))
  
  # Ocena stacjonarności (TRUE = stacjonarny)
  ADF_lvl_stac <- adf_level@teststat[1] < adf_level@cval[1,2]
  ADF_diff_stac <- adf_diff@teststat[1] < adf_diff@cval[1,2]
  
  PP_lvl_stac <- pp_level@teststat < pp_level@cval[2]
  PP_diff_stac <- pp_diff@teststat < pp_diff@cval[2]
  
  KPSS_lvl_stac <- kpss_level@teststat <= kpss_level@cval[2]
  KPSS_diff_stac <- kpss_diff@teststat <= kpss_diff@cval[2]
  
  stac_lvl_count <- sum(c(ADF_lvl_stac, PP_lvl_stac, KPSS_lvl_stac))
  stac_diff_count <- sum(c(ADF_diff_stac, PP_diff_stac, KPSS_diff_stac))
  
  return(list(
    ADF_level = ADF_lvl_stac,
    ADF_diff  = ADF_diff_stac,
    PP_level  = PP_lvl_stac,
    PP_diff   = PP_diff_stac,
    KPSS_level = KPSS_lvl_stac,
    KPSS_diff  = KPSS_diff_stac,
    Stacjonarnosc_lvl = stac_lvl_count,
    Stacjonarnosc_diff = stac_diff_count
  ))
}

# Przeprowadzenie testów dla każdej zmiennej z odpowiednią specyfikacją
wyniki_testow <- list()

for (nazwa in colnames(train_data)) {
  if (nazwa %in% zmienne_bez_trendu) {
    spec <- "drift"
  } else {
    spec <- "trend"
  }
  wyniki_testow[[nazwa]] <- test_stacjonarnosc(train_data[, nazwa], nazwa, spec)
}

# Tworzenie tabeli podsumowania
podsumowanie <- do.call(rbind, lapply(wyniki_testow, function(w) {
  if (is.null(w)) return(rep(NA, 8))
  unlist(w)
}))

rownames(podsumowanie) <- names(wyniki_testow)
colnames(podsumowanie) <- c("ADF_lvl", "ADF_diff", "PP_lvl", "PP_diff", 
                            "KPSS_lvl", "KPSS_diff", 
                            "Stacjonarnosc_lvl", "Stacjonarnosc_diff")

# Wyświetlenie wyników
print(podsumowanie)

zmienne_do_rozn <- c(
  "Stopa referencyjna",
  "Przeciętne miesięczne wynagrodzenie nominalne brutto w gospodarce narodoweja (zł)",
  "Stopa bezrobocia rejestrowanego (stan w końcu okresu)f [%]",
  "Wskaźnik ogólnego klimatu koniunktury w przetwórstwie przemysłowym (w miesiącu kończącym okres)",
  "Podaż pieniądza M3 (stan w końcu okresu) [mln zł]",
  "pieniądz gotówkowy w obiegu poza kasami banków [mln zł]",
  "Należności ogółem (stan w końcu okresu) [mln zł]",
  "Stopa oprocentowania depozytów złotowych gospodarstw domowych i instytucji niekomercyjnych działających na rzecz gospodarstw domowych w bankach komercyjnych (na rachunkach bieżących) [%]",
  "Stopa oprocentowania depozytów złotowych gospodarstw domowych i instytucji niekomercyjnych działających na rzecz gospodarstw domowych w bankach komercyjnych (z terminem pierwotnym do 2 lat włącznie) [%]",
  "Kurs oficjalny NBP (100 CHF)",
  "Zadłużenie krajowe Skarbu Państwa (stan w końcu okresu) [mln zł]",
  "Dług zagraniczny Skarbu Państwa (stan w końcu okresu) [mln zł]",
  "Popyt krajowy (ceny stale) [analogiczny okres roku poprzedniego=100]"  
)
# 1. Zbiór uczący – różnicowanie z zachowaniem długości (pierwsza wartość NA)
train_diff <- train_data
for (zm in zmienne_do_rozn) {
  if (zm %in% colnames(train_diff)) {
    train_diff[, zm] <- c(NA, diff(train_data[, zm]))
  }
}
train_diff <- na.omit(train_diff)  # usuwa pierwszy wiersz (NA dla wszystkich zmiennych)

# 2. Zbiór testowy – łączymy trening i test, różnicujemy całość, dzielimy przez indeksowanie
full_ts <- rbind(train_data, test_data)
full_diff <- full_ts
for (zm in zmienne_do_rozn) {
  if (zm %in% colnames(full_diff)) {
    full_diff[, zm] <- c(NA, diff(full_ts[, zm]))
  }
}

# Liczba obserwacji w treningu (oryginalnym)
n_train <- nrow(train_data)

# Dzielimy przez indeksy wierszy (unikamy problemów z window())
train_diff2 <- full_diff[2:(n_train), ]        # pomijamy pierwszy wiersz z NA
test_diff2  <- full_diff[(n_train+1):nrow(full_diff), ]

# Usuwamy ewentualne NA w pierwszych wierszach test_diff2 (powinno być już ok)
train_diff <- na.omit(train_diff2)
test_diff  <- na.omit(test_diff2)

# Sprawdzenie wymiarów
cat("Wymiar train_diff:", dim(train_diff), "\n")
cat("Wymiar test_diff :", dim(test_diff), "\n")