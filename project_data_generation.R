
library(tidyverse)
library(scales)
library(furrr)  # For parallel processing
library(lubridate)
library(tidyquant)  # For financial calculations

# Set seed for reproducibility
set.seed(580)

# Define constants
CONSTANTS <- list(
  INITIAL_REVENUE = 25e6,
  DISCOUNT_RATE = 0.08,
  YOY_CHANGES = c(0.0487, 0.0348, 0.0722, 0.0672, 0.0186, 
                  -0.1735, 0.4326, 0.2711, -0.0815, 0.0847)
)

#' Generate Historical Financial Data
#' @param start_year Numeric start year
#' @param end_year Numeric end year
#' @param initial_revenue Initial revenue value
#' @param yoy_changes Vector of year-over-year changes
#' @return tibble with historical financial data
generate_historical_data <- function(start_year, end_year, 
                                     initial_revenue, yoy_changes) {
  years <- start_year:end_year
  
  tibble(
    year = years,
    revenue = accumulate(
      yoy_changes, 
      ~ .x * (1 + .y), 
      .init = initial_revenue
    )[-1]
  ) %>%
    mutate(
      opex_ratio = runif(n(), 0.60, 0.70),
      capex_ratio = runif(n(), 0.10, 0.15),
      maintenance_ratio = runif(n(), 0.15, 0.25),
      
      opex = revenue * opex_ratio,
      capex = revenue * capex_ratio,
      maintenance = opex * maintenance_ratio,
      net_profit = revenue - (opex + capex)
    )
}

#' Generate Future Financial Projections
#' @param base_data Last row of historical data
#' @param num_years Number of years to project
#' @return tibble with projected financial data
generate_projections <- function(base_data, num_years) {
  projection_years <- (max(base_data$year) + 1):(max(base_data$year) + num_years)
  
  # Initialize first year
  initial_projection <- tibble(
    year = min(projection_years),
    revenue = tail(base_data$revenue, 1) * 1.06
  ) %>%
    mutate(
      opex_ratio = runif(1, 0.60, 0.70),
      capex_ratio = runif(1, 0.10, 0.15),
      maintenance_ratio = runif(1, 0.15, 0.25)
    )
  
  # Generate remaining years
  projections <- map_df(2:num_years, function(i) {
    growth_rate <- runif(1, 0.04, 0.08)
    
    tibble(
      year = projection_years[i],
      revenue = tail(initial_projection$revenue, 1) * (1 + growth_rate),
      opex_ratio = runif(1, 0.60, 0.70),
      capex_ratio = runif(1, 0.10, 0.15),
      maintenance_ratio = runif(1, 0.15, 0.25)
    )
  })
  
  bind_rows(initial_projection, projections) %>%
    mutate(
      opex = revenue * opex_ratio,
      capex = revenue * capex_ratio,
      maintenance = opex * maintenance_ratio,
      net_profit = revenue - (opex + capex)
    )
}

#' Calculate ROI Metrics
#' @param historical_data Historical financial data
#' @param projection_data Projected financial data
#' @param discount_rate Discount rate for NPV calculation
#' @return List of ROI metrics
calculate_roi_metrics <- function(historical_data, projection_data, discount_rate) {
  initial_investment <- sum(historical_data$capex)
  cash_flows <- projection_data$net_profit
  
  # Calculate NPV
  npv_value <- sum(c(-initial_investment, cash_flows) / 
                     (1 + discount_rate)^(0:(length(cash_flows))))
  
  # Calculate IRR
  irr_value <- IRR(
    cashflow = c(-initial_investment, cash_flows)
  )
  
  # Calculate Payback Period
  cumulative_flows <- cumsum(c(-initial_investment, cash_flows))
  payback_period <- which(cumulative_flows >= 0)[1] - 1
  
  list(
    npv = npv_value,
    irr = irr_value,
    payback_period = payback_period
  )
}

#' Create Financial Visualizations
#' @param data Combined historical and projection data
#' @return List of ggplot objects
create_visualizations <- function(data) {
  # Revenue Plot
  revenue_plot <- data %>%
    ggplot(aes(x = year, y = revenue)) +
    geom_line(aes(color = "Revenue"), size = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
    scale_color_manual(values = c("Revenue" = "blue")) +
    theme_minimal() +
    labs(
      title = "Revenue Trends",
      x = "Year",
      y = "Revenue ($ Millions)",
      color = NULL
    )
  
  # Profit Components Plot
  profit_components <- data %>%
    pivot_longer(
      cols = c(revenue, opex, capex, net_profit),
      names_to = "metric",
      values_to = "value"
    ) %>%
    ggplot(aes(x = year, y = value, color = metric)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    labs(
      title = "Financial Metrics Over Time",
      x = "Year",
      y = "Amount ($ Millions)",
      color = "Metric"
    )
  
  list(
    revenue_plot = revenue_plot,
    profit_components = profit_components
  )
}

# Main execution
historical_data <- generate_historical_data(
  2015, 2024,
  CONSTANTS$INITIAL_REVENUE,
  CONSTANTS$YOY_CHANGES
)

projection_data <- generate_projections(historical_data, 5)

# Combine data
full_data <- bind_rows(
  historical_data %>% mutate(type = "Historical"),
  projection_data %>% mutate(type = "Projected")
)

# Calculate ROI metrics
roi_metrics <- calculate_roi_metrics(
  historical_data,
  projection_data,
  CONSTANTS$DISCOUNT_RATE
)

# Generate visualizations
plots <- create_visualizations(full_data)

# Print ROI metrics
cat("\n=== ROI Metrics ===\n")
cat(sprintf("Net Present Value (NPV): $%s\n", 
            format(roi_metrics$npv, big.mark = ",", scientific = FALSE)))
cat(sprintf("Internal Rate of Return (IRR): %.2f%%\n", 
            roi_metrics$irr * 100))
cat(sprintf("Payback Period: %d years\n", 
            roi_metrics$payback_period))

# Display plots
plots$revenue_plot
plots$profit_components