install.packages("mice") #Needed to impute missing values.
library(mice)
install.packages("readxl") #Reads excel files.
library(readxl)
wb <- read_excel("C://Aditi//UNI//Data Science//PX5902 (2024-25) Data Science Project//DataDirFP//Western_Baltic_Masters_students.xlsx", sheet = 1) #'Sheet = 1' tells R to only read the first excel sheet.
head(wb)
summary(wb)
md.pattern(wb) #Finds the missing value patterns

af <- c("NH4", "NO3", "NO2", "TP", "Si", "TN", "DIP", "DIN", "SST_win", "SST_spr", "SST_su", "SAL_win", "Ox_deep", "N_load", "P_load", "Ice_max", "WBSI")

cc1 <- wb[, c(af, "Herr_recruit")]
cc1
cm <- cor(cc1, use = "complete.obs", method = "spearman") #Calculates the correlation between fish recruit and abiotic variables.
str(cc1)
md.pattern(cc1)

library(dplyr)
cc2 <- cc1 %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%  #Changes missing values to 'NA'. 
  mutate(across(where(is.character), as.numeric)) #Changes all columns to a numeric data type.
str(cc2)
summary(cc2)
logcc2 <- cc2 %>%
  mutate(across(everything(), ~ ifelse(. < 0 | is.na(.), NA, log(. + 1)))) #Log-transforms the data.
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

sp <- ggplot(logcc2.1, aes(x = Herr_recruit, y = value)) + #Creates scatter plots showing each abiotic variable against herring recruits.
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth() +
  facet_wrap(~ abiotic_factor, scales = "free_y") +
  labs(title = "Abiotic factors vs Herring recruits",
       x = "Herr_recruit",
       y = "Abiotic factor value"
  ) +
  theme_bw()
sp

cm <- cor(logcc2, use = "complete.obs", method = "spearman")
print(cm)

install.packages("reshape2")
library(reshape2)
hercm <- cm["Herr_recruit", af]
hercm2 <- matrix(hercm, nrow = 1)
rownames(hercm2) <- "Herr_recruit"
colnames(hercm2) <- names(hercm)

melt <- melt(hercm2)

ggplot(data = melt, aes(x = Var2, y = Var1, fill = value)) + #Creates the correlation heatmap.
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
logcc3 <- mice(logcc2, m = 5, method = "pmm", seed = 123) #Imputes data.
logcc4 <- complete(logcc3, 1)
logcc4

library(dplyr) #Standardises all data values.
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

year <- wb[["year"]] #Extracts 'year' variable.
year
logcc6 <- logcc5 %>%
  mutate(Year = year, .before = 1) # The last section in line tells R to put the 'Year' column in the beginning.
logcc6

str(logcc6)
multi_hmm <- depmix(list(NH4 ~ 1, NO3 ~ 1, NO2 ~ 1, TP ~ 1, Si ~ 1, TN ~ 1, DIP ~ 1, DIN ~ 1, SST_win ~ 1,
                         SST_spr ~ 1, SST_su ~ 1, SAL_win ~ 1, Ox_deep ~ 1, N_load ~ 1, P_load ~ 1, 
                         Ice_max ~ 1, WBSI ~ 1, Herr_recruit ~ 1),                             # All observed outcomes you want to model which depend on the latent variable.
                    family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                                  gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                                  gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian()), #The distribution of each of those observed outcomes. 
                    nstates = 2,  #The number of states you want to model. 
                    data = logcc6)
multi_hmm
set.seed(123)
multi_hmm_fit <- fit(multi_hmm) #Fit the data.
summary(multi_hmm_fit)

getpars(multi_hmm_fit) #Outputs all parameter values of the model. 
transition_prob <- getpars(multi_hmm_fit)[3:6] #Outputs the transition probabilities. The first two values are the initial state probabilities. 
transition_prob2 <- matrix(transition_prob, nrow = 2, byrow = TRUE)
rownames(transition_prob2) <- c("From_R1", "From_R2")
colnames(transition_prob2) <- c("To_R1", "To_R2")
print(transition_prob2)

emission_prob <- getpars(multi_hmm_fit)[7:78] #Outputs the emission probabilities. This depends on the number of outcome variables you want to model.
emission_prob2 <- matrix(emission_prob, nrow = 2, byrow = TRUE)
rownames(emission_prob2) <- c("R1", "R2")
vars <- c("NH4", "NO3", "NO2", "TP", "Si", "TN", "DIP", "DIN", 
          "SST_win", "SST_spr", "SST_su", "SAL_win", "Ox_deep", 
          "N_load", "P_load", "Ice_max", "WBSI", "Herr_recruit")
param_labels <- rep(c("Intercept", "SD"), times = length(vars))
colnames(emission_prob2) <- paste0(rep(vars, each = 2), "_", param_labels)
print(emission_prob2)

hidden_states <- posterior(multi_hmm_fit) #Outputs the hidden state probabilities for each data value. 
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
  install.packages("BiocManager") #'Rgraphviz' package is not a CRAN package but is under the Bioconductor repository. 

BiocManager::install("Rgraphviz") 
library(bnlearn)
library(dplyr)
library(Rgraphviz)

dbn_data <- logcc6.1[, c("State", "N_load", "P_load", "Herr_recruit")]
dbn_data
dbn_data2 <- dbn_data %>%
  mutate(across(c(State, N_load, P_load, Herr_recruit), 
                ~lag(.), .names = "{.col}_t")) %>%  #'_t' = lagged variable showing the previous time slice
  rename_with(~paste0(.x, "_t1"), c(State, N_load, P_load, Herr_recruit)) %>%  #'_t1' = current observations or current time slice
  filter(!is.na(State_t))
dbn_data2

model_string <- "[State_t][State_t1|State_t][N_load_t|State_t][P_load_t|State_t][Herr_recruit_t|State_t:N_load_t:P_load_t][N_load_t1|N_load_t:State_t1]
                 [P_load_t1|P_load_t:State_t1][Herr_recruit_t1|Herr_recruit_t:P_load_t1:N_load_t1]"
dbn_str <- model2network(model_string) #Transforms the fitted model into a graphical model

fitted_dbn <- bn.fit(dbn_str, data = dbn_data2)

dbn_plot <- graphviz.plot(dbn_str, main = "2-Slice DBN (Herring Recruitment)") #Produces the DBN graph.
fitted_dbn

split_point <- floor(0.7 * nrow(dbn_data2)) #70% of the data is used as the training data.
split_point
train_data <- dbn_data2[1:split_point, ]
train_data
test_data <- dbn_data2[(split_point + 1): nrow(dbn_data2), ] #Remaining 30% is used as the test data. 
test_data

fitted_data <- bn.fit(dbn_str, data = train_data)
fitted_data

library(ggplot2)
years <- logcc6.1$Year[-1]#The first year is removed as there is no observation before this. This ensures that the number of actual and predicted values are equal for the RMSE. 
n_years <- length(years) # Tells how many times the loop should run.
predicted_values <- numeric(n_years) # Creates a vector with a length equal to the length of the 'years' variables to store the predictions. 

for (i in 1:n_years) {
  current_input <- dbn_data2[i, , drop = FALSE] #The last part in this line maintains the dimension of 'dbn_data2' variable.
  predicted_values[i] <- predict(fitted_data, node = "Herr_recruit_t1", data = current_input)
}

actual_values <- dbn_data2$Herr_recruit_t1
plot_df <- data.frame(
  Year = rep(years, 2), #Repeats each year two times for the actual and predicted values. 
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
