calculate_financial_ratios <- function(data) {
  data %>%
    group_by(type) %>%
    summarise(
      operating_margin = mean((revenue - opex) / revenue),
      profit_margin = mean(net_profit / revenue),
      capex_to_revenue = mean(capex / revenue),
      maintenance_to_opex = mean(maintenance / opex),
      .groups = 'drop'
    )
}

# Function to calculate growth rates
calculate_growth_rates <- function(data) {
  data %>%
    arrange(year) %>%
    group_by(type) %>%
    summarise(
      revenue_cagr = (last(revenue) / first(revenue))^(1 / n()) - 1,
      profit_cagr = (last(net_profit) / first(net_profit))^(1 / n()) - 1,
      .groups = 'drop'
    )
}

# Function to generate risk metrics
calculate_risk_metrics <- function(data) {
  data %>%
    group_by(type) %>%
    summarise(
      revenue_volatility = sd(revenue) / mean(revenue),
      profit_volatility = sd(net_profit) / mean(net_profit),
      min_profit_margin = min(net_profit / revenue),
      max_profit_margin = max(net_profit / revenue),
      .groups = 'drop'
    )
}

# Generate comprehensive feasibility summary
generate_feasibility_summary <- function(data, roi_metrics) {
  # Calculate all metrics
  financial_ratios <- calculate_financial_ratios(data)
  growth_rates <- calculate_growth_rates(data)
  risk_metrics <- calculate_risk_metrics(data)
  
  # Prepare projected returns summary
  projected_returns <- data %>%
    filter(type == "Projected") %>%
    summarise(
      total_revenue = sum(revenue),
      total_profit = sum(net_profit),
      avg_annual_profit = mean(net_profit),
      profit_margin = mean(net_profit / revenue)
    )
  
  # Print comprehensive summary
  cat("\n=== ECONOMIC FEASIBILITY ASSESSMENT ===\n\n")
  
  # 1. Investment Overview
  cat("1. INVESTMENT METRICS\n")
  cat(sprintf("NPV: $%s\n", format(roi_metrics$npv, big.mark = ",", scientific = FALSE)))
  cat(sprintf("IRR: %.2f%%\n", roi_metrics$irr * 100))
  cat(sprintf("Payback Period: %d years\n\n", roi_metrics$payback_period))
  
  # 2. Financial Ratios
  cat("2. FINANCIAL RATIOS\n")
  print(kable(financial_ratios %>%
                mutate(across(where(is.numeric), ~scales::percent(., accuracy = 0.1))),
              format = "pipe"))
  cat("\n")
  
  # 3. Growth Analysis
  cat("3. GROWTH ANALYSIS\n")
  print(kable(growth_rates %>%
                mutate(across(where(is.numeric), ~scales::percent(., accuracy = 0.1))),
              format = "pipe"))
  cat("\n")
  
  # 4. Risk Assessment
  cat("4. RISK METRICS\n")
  print(kable(risk_metrics %>%
                mutate(across(where(is.numeric), ~scales::percent(., accuracy = 0.1))),
              format = "pipe"))
  cat("\n")
  
  # 5. Projected Returns Summary
  cat("5. PROJECTED RETURNS SUMMARY\n")
  cat(sprintf("Total Projected Revenue: $%s\n", 
              format(projected_returns$total_revenue, big.mark = ",", scientific = FALSE)))
  cat(sprintf("Total Projected Profit: $%s\n", 
              format(projected_returns$total_profit, big.mark = ",", scientific = FALSE)))
  cat(sprintf("Average Annual Profit: $%s\n", 
              format(projected_returns$avg_annual_profit, big.mark = ",", scientific = FALSE)))
  cat(sprintf("Average Profit Margin: %.1f%%\n\n", 
              projected_returns$profit_margin * 100))
  
  # 6. Generate recommendations based on metrics
  cat("6. FEASIBILITY ASSESSMENT\n")
  
  # NPV-based assessment
  if (roi_metrics$npv > 0) {
    cat("✓ Project has positive NPV, indicating value creation\n")
  } else {
    cat("⚠ Project has negative NPV, suggesting potential value destruction\n")
  }
  
  # IRR-based assessment
  if (roi_metrics$irr > 0.15) {
    cat("✓ IRR exceeds typical hurdle rate (15%)\n")
  } else {
    cat("⚠ IRR below typical hurdle rate (15%)\n")
  }
  
  # Payback period assessment
  if (roi_metrics$payback_period <= 5) {
    cat("✓ Payback period within acceptable range (≤5 years)\n")
  } else {
    cat("⚠ Extended payback period (>5 years)\n")
  }
  
  # Risk assessment
  projected_risk <- risk_metrics %>% 
    filter(type == "Projected")
  
  if (projected_risk$revenue_volatility < 0.2) {
    cat("✓ Projected revenue volatility within acceptable range\n")
  } else {
    cat("⚠ High projected revenue volatility\n")
  }
}

# Execute the analysis with your existing data
generate_feasibility_summary(full_data, roi_metrics)