install.packages("mice")
library(mice)
install.packages("readxl")
library(readxl)
wb <- read_excel("C://Aditi//UNI//Data Science//PX5902 (2024-25) Data Science Project//DataDirFP//Bothnian_Sea_masters_students.xlsx", sheet = 1)
head(wb)
summary(wb)
md.pattern(wb)

af <- c("SAL_win", "pH", "DIP", "DIN", "TP", "Si", "SST_su", "SST_spr",
        "TN", "DOC", "SAL_deep", "DIP_deep", "TP_deep", "SHT_su",
        "TN_deep", "Ox_deep", "DIN_deep", "N_load", "P_load", "Ice_max", "WBSI")

cc1 <- wb[, c(af, "Herr_Recruit")]
cc1
cm <- cor(cc1, use = "complete.obs", method = "spearman")
str(cc1)
md.pattern(cc1)

library(dplyr)
cc2 <- cc1 %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(where(is.character), as.numeric))
str(cc2)
summary(cc2)
logcc2 <- cc2 %>%
  mutate(across(everything(), ~ ifelse(. < 0 | is.na(.), NA, log(. + 1))))
summary(logcc2)

install.packages("ggplot2")
library(ggplot2)
library(tidyr)

logcc2.1 <- logcc2 %>%
  pivot_longer(
    cols = all_of(af),
    names_to = "abiotic_factor",
    values_to = "value"
  )

sp <- ggplot(logcc2.1, aes(x = Herr_Recruit, y = value)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth() +
  facet_wrap(~ abiotic_factor, scales = "free_y") +
  labs(title = "Abiotic factors vs Herring recruits",
       x = "Herr_Recruit",
       y = "Abiotic factor value"
  ) +
  theme_bw()
sp

cm <- cor(logcc2, use = "complete.obs", method = "spearman")
print(cm)

install.packages("reshape2")
library(reshape2)
herrcm <- cm["Herr_Recruit", af]
herrcm2 <- matrix(herrcm, nrow = 1)
rownames(herrcm2) <- "Herr_Recruit"
colnames(herrcm2) <- names(herrcm)

melt <- melt(herrcm2)

ggplot(data = melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color="black"),
        axis.text.y = element_text(color="black")) +
  labs(title = "Correlation with Herring Recruits", x = NULL, y = NULL)

logcc2
library(mice)
md.pattern(logcc2)
logcc3 <- mice(logcc2, m = 5, method = "pmm", seed = 123)
logcc4 <- complete(logcc3, 1)
logcc4

library(dplyr)
logcc5 <- logcc4 %>%
  as.data.frame() %>%
  mutate(across(
    where(is.numeric), 
    ~ if(all(is.na(.))) {
      NA
    } else {
      scale(.)[, 1]
    }
  ))
logcc5
sapply(logcc5, mean, na.rm = TRUE)
sapply(logcc5, sd, na.rm = TRUE)

install.packages("depmixS4")
library(depmixS4)

year <- wb[["Year"]]
year
logcc6 <- logcc5 %>%
  mutate(Year = year, .before = 1)
logcc6

str(logcc6)
multi_hmm <- depmix(list(SAL_win ~ 1, pH ~ 1, DIP ~ 1, DIN ~ 1, TP ~ 1, Si ~ 1, SST_su ~ 1, SST_spr ~ 1,
                         TN ~ 1, DOC ~ 1, SAL_deep ~ 1, DIP_deep ~ 1, TP_deep ~ 1, SHT_su ~ 1, TN_deep ~ 1, 
                         Ox_deep ~ 1, DIN_deep ~ 1, N_load ~ 1, P_load ~ 1, Ice_max ~ 1, WBSI ~ 1, 
                         Herr_Recruit ~ 1),
                    family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                                  gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                                  gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(),
                                  gaussian(), gaussian(), gaussian(), gaussian()),
                    nstates = 2,
                    data = logcc6)
multi_hmm
set.seed(123)
multi_hmm_fit <- fit(multi_hmm)
summary(multi_hmm_fit)

getpars(multi_hmm_fit)
transition_prob <- getpars(multi_hmm_fit)[3:6]
transition_prob2 <- matrix(transition_prob, nrow = 2, byrow = TRUE)
rownames(transition_prob2) <- c("From_R1", "From_R2")
colnames(transition_prob2) <- c("To_R1", "To_R2")
print(transition_prob2)

emission_prob <- getpars(multi_hmm_fit)[7:94]
emission_prob2 <- matrix(emission_prob, nrow = 2, byrow = TRUE)
rownames(emission_prob2) <- c("R1", "R2")
vars <- c("SAL_win", "pH", "DIP", "DIN", "TP", "Si", "SST_su", "SST_spr",
          "TN", "DOC", "SAL_deep", "DIP_deep", "TP_deep", "SHT_su",
          "TN_deep", "Ox_deep", "DIN_deep", "N_load", "P_load", "Ice_max", "WBSI", "Herr_Recruit")
param_labels <- rep(c("Intercept", "SD"), times = length(vars))
colnames(emission_prob2) <- paste0(rep(vars, each = 2), "_", param_labels)
print(emission_prob2)

hidden_states <- posterior(multi_hmm_fit)
print(hidden_states)

library(ggplot2)
logcc6.1 <- logcc6 %>% mutate(State = hidden_states$state)
logcc6.1
hp <- ggplot(logcc6.1, aes(x = Year, y = State)) +
  geom_step() +
  scale_y_continuous(breaks = c(1,2), labels = c("Regime 1", "Regime 2")) +
  labs(title = "Regime Shifts over time",
       x = "Year",
       y = "Hidden State") +
  theme_minimal()
hp

install.packages("bnlearn")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")
library(bnlearn)
library(dplyr)
library(Rgraphviz)

dbn_data <- logcc6.1[, c("State", "TN", "WBSI", "Herr_Recruit")]
dbn_data
dbn_data2 <- dbn_data %>%
  mutate(across(c(State, TN, WBSI, Herr_Recruit), 
                ~lag(.), .names = "{.col}_t")) %>%
  rename_with(~paste0(.x, "_t1"), c(State, TN, WBSI, Herr_Recruit)) %>%
  filter(!is.na(State_t))
dbn_data2

model_string <- "[State_t][State_t1|State_t][TN_t|State_t][WBSI_t|State_t][Herr_Recruit_t|State_t:TN_t:WBSI_t][TN_t1|TN_t:State_t1][WBSI_t1|WBSI_t:State_t1][Herr_Recruit_t1|Herr_Recruit_t:WBSI_t1:TN_t1]"
dbn_str <- model2network(model_string)

fitted_dbn <- bn.fit(dbn_str, data = dbn_data2)

dbn_plot <- graphviz.plot(dbn_str, main = "2-Slice DBN (Herring Recruitment)")
fitted_dbn

split_point <- floor(0.7 * nrow(dbn_data2))
split_point
train_data <- dbn_data2[1:split_point, ]
train_data
test_data <- dbn_data2[(split_point + 1): nrow(dbn_data2), ]
test_data

fitted_data <- bn.fit(dbn_str, data = train_data)
fitted_data

library(ggplot2)
years <- logcc6.1$Year[-1]
n_years <- length(years)
predicted_values <- numeric(n_years)

for (i in 1:n_years) {
  current_input <- dbn_data2[i, , drop = FALSE]
  predicted_values[i] <- predict(fitted_data, node = "Herr_Recruit_t1", data = current_input)
}

actual_values <- dbn_data2$Herr_Recruit_t1
plot_df <- data.frame(
  Year = rep(years, 2),
  Recruitment = c(actual_values, predicted_values),
  Type = rep(c("Actual", "Predicted"), each = n_years)
)
ggplot(plot_df, aes(x = Year, y = Recruitment, color = Type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Herring Recruitment: Actual vs Predicted",
       x = "Year",
       y = "Herring Recruitment") +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

install.packages("Metrics")
library(Metrics)

rmse_value <- rmse(actual_values, predicted_values)
rmse_value
