# 10 - Inflation ----
# URL: https://github.com/arubhardwaj/inflationfr

## data.R ----
library(tidyverse)

cpi <- read_csv("../data/FRACPIALLMINMEI.csv")
interest_rates <- read_csv("../data/IRSTCI01FRM156N.csv")
exchange_rate <- read_csv("../data/CCUSMA02FRM618N.csv")
unemployment_rate <- read_csv("../data/LRHUTTTTFRM156S.csv")
oil_prices <- read_csv("../data/MCOILWTICO.csv")


var_data <- cpi %>% left_join(interest_rates) %>% 
  left_join(exchange_rate) %>% 
  left_join(unemployment_rate) %>% 
  left_join(oil_prices)

names(var_data) <- c("DATE","CPI",
                     "Interest_Rates","Exchange_Rates",
                     "Unemployment_Rate","Oil_Prices")



# year on year log change
calculate_yoy_change_and_log <- function(series) {
  # Calculate year-on-year change
  yoy_change <- diff(series) / lag(series) * 100  # Percentage change
  
  # Take the logarithm of the year-on-year change
  log_yoy_change <- log(1 + yoy_change / 100)
  
  return(list(yoy_change = yoy_change, log_yoy_change = log_yoy_change))
}


yoy<-calculate_yoy_change_and_log(var_data$Oil_Prices)
var_data$Oil_Prices_log_yoy <- yoy$log_yoy_change


calculate_yoy_change_and_log(var_data$Oil_Prices)  


var_data_time <- var_data %>% filter(DATE >= '1986-01-01')


## svar.R ----
source("E:/french inflation/R/data.R")
library(vars)
library(svars)
library(kableExtra)


# VAR model ---------------------------------------------------------------


# Convert date to time series
var_data_time1 <- var_data_time[-6]


# Convert date to time series
var_data_time_ts <- ts(var_data_time1[, -1], start = c(1986, 1), frequency = 12)

# Remove rows with NAs
var_data_time_ts <- na.omit(var_data_time_ts)

# Create a VAR object
var_model <- VAR(var_data_time_ts, p = 1, type = "both")

var_model


a <- diag(1, 5)
a[lower.tri(a)] <- NA

# Create a VAR object
var_model <- VAR(var_data_time_ts, p = 1, type = "both")

# Estimate the SVAR model
svar_estimation <- SVAR(x = var_model, Amat = a, max.iter = 1000,estmethod = "direct")
summary(svar_estimation)



irf_result <- irf(svar_estimation, impulse = "Oil_Prices_log_yoy", response = c("CPI", "Interest_Rates", "Exchange_Rates", "Unemployment_Rate"), boot = FALSE)

# Plot IRF
plot(irf_result, main = "Impulse Response Functions", xlab = "Months", ylab = "Elasticity", col = c("blue", "red", "green", "purple"))
legend("topright", legend = c("CPI", "Interest Rates", "Exchange Rates", "Unemployment Rate"), col = c("blue", "red", "green", "purple"), lty = 1, cex = 0.8)

# Alternatively, you can plot the cumulative IRF to show cumulative effects over time
cum_irf_result <- irf(svar_estimation, impulse = "Oil_Prices_log_yoy", response = c("CPI", "Interest_Rates", "Exchange_Rates", "Unemployment_Rate"), boot = FALSE, cumulative = TRUE)

# Plot Cumulative IRF
plot(cum_irf_result, main = "Cumulative Impulse Response Functions", xlab = "Months", ylab = "Cumulative Elasticity", col = c("blue", "red", "green", "purple"))
legend("topright", legend = c("CPI", "Interest Rates", "Exchange Rates", "Unemployment Rate"), col = c("blue", "red", "green", "purple"), lty = 1, cex = 0.8)





# Ggplot ------------------------------------------------------------------


# Extract IRF data
irf_data <- irf(svar_estimation, impulse = "CPI", response = c("Oil_Prices_log_yoy", "Interest_Rates", "Exchange_Rates", "Unemployment_Rate"), boot = FALSE)

# Convert IRF data to data frame
irf_df <- as.data.frame(irf_data$irf)

# Add a column for the time points (months)
irf_df$Months <- seq_len(nrow(irf_df))

# Reshape data for ggplot
irf_df_long <- tidyr::gather(irf_df, key = "Variable", value = "Elasticity", -Months)

# Plot using ggplot2
ggplot(irf_df_long, aes(x = Months, y = Elasticity, color = Variable)) +
  geom_line() +
  labs(title = "Impulse Response Functions",
       x = "Months",
       y = "Elasticity") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green", "purple"))










# Extract IRF data
irf_data <- irf(svar_estimation, impulse = "CPI", response = c("Oil_Prices_log_yoy", "Interest_Rates", "Exchange_Rates", "Unemployment_Rate"), boot = FALSE)

# Convert IRF data to data frame
irf_df <- as.data.frame(irf_data$irf)

# Add a column for the time points (months)
irf_df$Months <- seq_len(nrow(irf_df))

# Reshape data for ggplot
irf_df_long <- tidyr::gather(irf_df, key = "Variable", value = "Elasticity", -Months)

# Filter the data for CPI only
ggplot(irf_df_long, aes(x = Months, y = Elasticity, color = Variable)) +
  geom_line() +
  labs(title = "Impulse Response Functions",
       x = "Months",
       y = "Elasticity") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green", "purple"))




# Extract IRF data
irf_data <- irf(svar_estimation, impulse = "CPI", response = c("Oil_Prices_log_yoy", "Interest_Rates", "Exchange_Rates", "Unemployment_Rate"), boot = FALSE)

# Convert IRF data to data frame
irf_df <- as.data.frame(irf_data$irf)

# Add a column for the time points (months)
irf_df$Months <- seq_len(nrow(irf_df))

# Reshape data for ggplot
irf_df_long <- tidyr::gather(irf_df, key = "Variable", value = "Elasticity", -Months)


# Plot using ggplot2 with facet_wrap

ggplot(irf_df_long, aes(x = Months, y = Elasticity, color = Variable)) +
  geom_line() +
  labs(title = "Effect of Variables on CPI",
       x = "Months",
       y = "Elasticity") +
  theme_minimal() +
  facet_grid(Variable ~ ., scales = "free_y")




ggplot(irf_df_long, aes(x = Months, y = Elasticity)) +
  geom_line() +
  labs(title = "Effect of Variables on CPI",
       x = "Months",
       y = "Elasticity") +
  theme_minimal() +
  facet_grid(Variable ~ ., scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a horizontal line at y = 0
  geom_hline(yintercept = c(-0.2, 0.2), linetype = "dashed", color = "red") +  # Add upper and lower threshold lines
  theme(legend.position = "none")




# Plots nicely done -------------------------------------------------------
library(devtools)
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")

irf_all <- irf(svar_estimation, impulse = "CPI", n.ahead = 40, ortho = T,
               cumulative = T, boot = TRUE, ci = 0.9, runs = 50)




single_varirf <- extract_varirf(irf_all)

single_varirf %>% 
  ggplot(aes(x=period, y=irf_cpi_interest_rates, ymin=lower_cpi_interest_rates, ymax=upper_cpi_interest_rates)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=.2, color="grey50", linetype="dashed") +
  geom_line() +
  theme_light() +
  ggtitle("")+
  ylab("Elasticity")+
  xlab("Months") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

ggsave( "../plot/cpi-interest-rates.pdf",device = "pdf",width = 10, height = 6)



single_varirf %>% 
  ggplot(aes(x=period, y=irf_cpi_exchange_rates, ymin=lower_cpi_exchange_rates, ymax=upper_cpi_exchange_rates)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=.2, color="grey50", linetype="dashed") +
  geom_line() +
  theme_light() +
  ggtitle("")+
  ylab("Elasticity")+
  xlab("Months") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

ggsave( "../plot/cpi-exchange-rates.pdf",device = "pdf",width = 10, height = 6)



single_varirf %>% 
  ggplot(aes(x=period, y=irf_cpi_unemployment_rate, ymin=lower_cpi_unemployment_rate, ymax=upper_cpi_unemployment_rate)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=.2, color="grey50", linetype="dashed") +
  geom_line() +
  theme_light() +
  ggtitle("")+
  ylab("Elasticity")+
  xlab("Months") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
ggsave( "../plot/cpi-unemployment-rates.pdf",device = "pdf",width = 10, height = 6)


single_varirf %>% 
  ggplot(aes(x=period, y=irf_cpi_oil_prices_log_yoy, ymin=lower_cpi_oil_prices_log_yoy, ymax=upper_cpi_oil_prices_log_yoy)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=.2, color="grey50", linetype="dashed") +
  geom_line() +
  theme_light() +
  ggtitle("")+
  ylab("Elasticity")+
  xlab("Months") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
ggsave( "../plot/cpi-global-oil-prices.pdf",device = "pdf",width = 10, height = 6)




# Forecast errror ---------------------------------------------------------


fevd_result <- fevd(svar_estimation, n.ahead = 60)

# Print FEVD
print(fevd_result)

# Plot FEVD
fevd_matrix <- fevd_result$CPI

# Convert percentages to character format
fevd_matrix_percent <- sprintf("%.1f%%", fevd_matrix * 100)



if (length(rownames(fevd_matrix)) == 0) {
  row_names <- paste("After", 1:nrow(fevd_matrix),"Month")
} else {
  row_names <- rownames(fevd_matrix)
}


num_months = 60

table_data <- data.frame(
  Contribution_of = row_names,
  Unemployment_Rate = fevd_matrix_percent[1:num_months],
  Inflation = fevd_matrix_percent[(num_months + 1):(2 * num_months)],
  Policy_Rate = fevd_matrix_percent[(2 * num_months + 1):(3 * num_months)],
  Exchange_Rate = fevd_matrix_percent[(3 * num_months + 1):(4 * num_months)],
  Global_Oil_Prices = fevd_matrix_percent[(4 * num_months + 1):(5 * num_months)]
)

names(table_data)[1] <- ""

table_data <- table_data[c(1,4,12,60),]
table_data <- table_data %>% t() %>% data.frame()
names(table_data)<-table_data[1,]
table_data <- table_data[-1,]

tab <- knitr::kable(table_data, format = "latex", col.names = c("After 1 Month", "After 1 Quarter", "After 1 Year", "After 5 Years"))

latex_table <- xtable::xtable(tab, align = c("l", "c", "c", "c", "c"))



# Save the table to a LaTeX file with caption
write.table(tab, "../tables/table1.tex", sep = "&", row.names = FALSE, col.names = FALSE, quote = FALSE)


# robustness --------------------------------------------------------------



# Dynamic specification of the VAR model
optimal_lag <- VARselect(var_data_time_ts, lag.max = 10, type = "both")$selection["AIC(n)"]
var_model <- VAR(var_data_time_ts, p = optimal_lag, type = "both")

# Increase bootstrap repetitions for IRF
irf_result <- irf(svar_estimation, impulse = "Oil_Prices_log_yoy", response = c("CPI", "Interest_Rates", "Exchange_Rates", "Unemployment_Rate"), boot = TRUE, runs = 1000)

# Enhanced diagnostic checks
serial_test <- serial.test(var_model, lags.pt = optimal_lag, type = "BG")
print(serial_test)

# Enhanced visualization
ggplot(irf_df_long, aes(x = Months, y = Elasticity, color = Variable)) +
  geom_line() +
  labs(title = "Enhanced Impulse Response Functions",
       x = "Months",
       y = "Elasticity") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

# Increase bootstrap repetitions for FEVD
fevd_result <- fevd(svar_estimation, n.ahead = 60, boot = TRUE, runs = 1000)

# Error handling (example)
tryCatch({
  # Code that might produce an error
  var_model <- VAR(var_data_time_ts, p = optimal_lag, type = "both")
}, error = function(e) {
  # Error handling code
  cat("Error in VAR model fitting: ", e$message, "\n")
})



# Extract FEVD results
fevd_matrix <- fevd_result$CPI

# Assuming 'fevd_matrix' is a 3-dimensional array (variables x horizons x response variables)
# Convert it into a long format data frame
fevd_df <- as.data.frame(fevd_matrix, responseName = "Variable")

# Add time horizon as a column
selected_horizons <- fevd_df %>%
  slice(c(1, 3, 6, 12, 60)) %>% 
  t() %>%  # Transpose the data frame
  as.data.frame() 

selected_horizons <- (round(selected_horizons*100,2))
# Use kable from knitr package to create a table
library(knitr)
colnames(fevd_df_selected) <- c("After 1 Month", "After 1 Quarter","After 6 Months", "After 1 Year", "After 5 Years")
rownames(fevd_df_selected) <- c("Unemployment_Rate", "Inflation", "Policy_Rate", "Exchange_Rate", "Global_Oil_Prices")

selected_horizons <- selected_horizons[,-1]
fevd_df_selected <- selected_horizons 

tab <- knitr::kable(fevd_df_selected, format = "latex", col.names = c("After 6 Months", "After 1 Quarter", "After 1 Year", "After 5 Years"))



write.table(tab, "../tables/table2-robust.tex", sep = "&", row.names = FALSE, col.names = FALSE, quote = FALSE)

