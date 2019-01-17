library("data.table")
library("pdftools")
library("purrr")
library("chron")
library("randomNames")
library("DT")
library("plotly")
library("fmsb")
library("ggthemes")

result_pdf <- "results/ywca-indoor-tri-2019-01-13.pdf"

text <- pdf_text(result_pdf)

all_text <- paste0(text, collapse = "") %>%
  gsub(" *Last Name", "LastName", .) %>%
  gsub(" +First Name", " FirstName", .) %>%
  gsub("Larson O'Neil", "Larson-O'Neil", .)


# read results and remove the repeated header row
results <- fread(text = all_text) %>% .[!LastName == "LastName"]

# drop the distance rank, we'll recreate later
results[, Rank := NULL]

# convert the times in the character columns to time objects
time_cols <- c("Swim", "TI", "Bike", "TII", "Run", "Total")
results[, (time_cols) := map(.SD, ~chron(times = .)), .SDcols = time_cols]

results[, Age := as.integer(Age)]

# adjust the swim and TI times in anticipation of their adjustments
results[LastName == "Legge", `:=`(
    Swim  = chron(time = "00:08:55")
  , TI    = chron(time = "00:02:25")
  , Total = chron(time = "00:51:32")
)]


# Genders -----------------------------------------------------------------

data("randomNamesData")

first_name_cats <- grep("^first_names", names(randomNamesData), value = TRUE) %>%
  grep("e6", ., invert = TRUE, value = TRUE)
first_names_list <- map(first_name_cats, get, envir = randomNamesData) %>%
  map(function(x) {
    data.table(
        FirstName  = names(x)
      , proportion = unname(x)
    )
  })

names(first_names_list) <- first_name_cats
first_names <- rbindlist(first_names_list, idcol = "meta")
first_names[, meta := gsub("first_names_", "", meta)]

first_names[, (c("ethnicity", "gender")) := tstrsplit(meta, split = "_", fixed = TRUE)]
first_names[, `:=`(meta = NULL, ethnicity = NULL)]
first_names[, `:=`(
  gender = factor(as.integer(gsub("g", "", gender)), levels = 0:1, labels = c("Male", "Female"))
)]

genders <- unique(first_names)

# collapse names by gender -- make sure there is only one
best_guess_gender <- genders[order(FirstName, -proportion)][, .SD[1], by = FirstName]

gender_results <- merge(results, best_guess_gender, by = "FirstName", all.x = TRUE)

gender_results[is.na(gender)]
gender_results[, .(n = length(unique(gender))), by = .(FirstName)][n > 1]

gender_results[FirstName == "Kimi", gender := "Female"]
gender_results[FirstName == "McClelland", gender := "Male"]

gender_results[, proportion := NULL]

# Age Groups --------------------------------------------------------------

age_groups <- list(
    '19 & under' = data.table(Age = 0:19)
  , '20 - 24'    = data.table(Age = 20:24)
  , '25 - 29'    = data.table(Age = 25:29)
  , '30 - 34'    = data.table(Age = 30:34)
  , '35 - 39'    = data.table(Age = 35:39)
  , '40 - 44'    = data.table(Age = 40:44)
  , '45 - 49'    = data.table(Age = 45:49)
  , '50 - 54'    = data.table(Age = 50:54)
  , '55 - 59'    = data.table(Age = 55:59)
  , '60 - 64'    = data.table(Age = 60:64)
  , '65 & Over'  = data.table(Age = 65:150)
) %>% rbindlist(idcol = "group")

all_results <- merge(gender_results, age_groups, by = "Age", all.x = TRUE)


# Additional Calculations -------------------------------------------------

all_results[, Name := paste0(substr(FirstName, 1, 1), ". ", LastName)]

all_results[, Distance := factor(Distance, levels = c("Mini", "Sprint", "Long"))]

all_results[, RunPercent  := as.numeric(Run / Total)]
all_results[, BikePercent := as.numeric(Bike / Total)]
all_results[, SwimPercent := as.numeric(Swim / Total)]


all_results[, ScaleDistance := as.numeric(Total / min(Total)), by = .(Distance, gender)]


# Average Times -----------------------------------------------------------

sprint_ranges <- expand.grid(Swim = c(14, 16), Bike = c(36, 45), Run = c(22, 26)) %>% as.data.table()

sprint_ranges[, Total := Swim + Bike + Run]

sprint_ranges[, `:=`(
    SwimPerc = Swim / Total
  , BikePerc = Bike / Total
  , RunPerc  = Run / Total
)]

sprint_ranges[, .(
    medSwim  = median(SwimPerc)
  , meanSwim = mean(SwimPerc)
  , medBike  = median(BikePerc)
  , meanBike = mean(BikePerc)
  , medRun   = median(RunPerc)
  , meanRun  = mean(RunPerc)
)]


# Analysis ----------------------------------------------------------------


all_results[, .(
  n = .N
  , Avg = mean(Total)
  , Min = min(Total)
), by = .(Distance, gender)][order(Distance, gender)]

all_results[, .(
  n = .N
  , Avg = mean(Total)
  , Min = min(Total)
), by = .(Distance, gender, group)][order(Distance, gender, group)]

# Run vs. Bike ------------------------------------------------------------


p <- ggplot(all_results, aes(x = BikePercent, y = RunPercent, label = Name, colour = gender)) + 
  facet_wrap(~Distance, scales = "free_x") + 
  scale_x_continuous(labels = scales::percent, limits = c(0.2, 0.6)) +
  scale_y_continuous(labels = scales::percent, limits = c(0.2, 0.6)) +
  # percentages according to an outdoor sprint distance
  geom_hline(yintercept = 0.32, alpha = 1/3, linetype = 2) +
  geom_vline(xintercept = 0.508, alpha = 1/3, linetype = 2) +
  geom_point(aes(alpha = 1 / (ScaleDistance^2))) + 
  theme(axis.ticks = element_blank())


ggplotly(p)


# Run vs. Swim ------------------------------------------------------------


p <- ggplot(all_results, aes(x = RunPercent, y = SwimPercent, label = Name, colour = gender)) + 
  facet_wrap(~Distance, scales = "free_x") + 
  scale_x_continuous(labels = scales::percent, limits = c(0.2, 0.6)) +
  scale_y_continuous(labels = scales::percent, limits = c(0.1, 0.3)) +
  # percentages according to an outdoor sprint distance
  geom_hline(yintercept = 0.188, alpha = 1/3, linetype = 2) +
  geom_vline(xintercept = 0.508, alpha = 1/3, linetype = 2) +
  geom_point(aes(alpha = 1 / (ScaleDistance^2))) + 
  theme(axis.ticks = element_blank())


ggplotly(p)


# Run vs. Swim ------------------------------------------------------------


p <- ggplot(all_results, aes(x = BikePercent, y = SwimPercent, label = Name, colour = gender)) + 
  facet_wrap(~Distance, scales = "free_x") + 
  scale_x_continuous(labels = scales::percent, limits = c(0.2, 0.5)) +
  scale_y_continuous(labels = scales::percent, limits = c(0.1, 0.3)) +
  # percentages according to an outdoor sprint distance
  geom_hline(yintercept = 0.188, alpha = 1/3, linetype = 2) +
  geom_vline(xintercept = 0.32, alpha = 1/3, linetype = 2) +
  geom_point(aes(alpha = 1 / (ScaleDistance^2))) + 
  theme(axis.ticks = element_blank())


ggplotly(p)

