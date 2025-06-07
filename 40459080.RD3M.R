#Load Required Libraries
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

# Model Parameters
num_factories <- c(11, 9, 5)  # Number of available factories of each class
min_rate <- c(1.00, 0.50, 1.50) # Minimum production rate per factory
max_rate <- c(2.00, 2.50, 4.00) # Maximum production rate per factory
weekly_cost_min <- c(10000, 25000, 30000) # Weekly cost at minimum production
weekly_cost_per_unit <- c(180, 130, 200) # Weekly cost per unit above minimum
setup_cost <- c(20000, 10000, 5000) # Setup cost for each factory class
weekly_demand <- rep(c(14, 33, 28, 20, 42, 24), c(6, 5, 3, 3, 4, 6)) # Demand per week
weeks <- length(weekly_demand)

# Function to solve the model with/without extra production provision
solve_factory_model <- function(extra_production = TRUE) {
  buffer_multiplier <- ifelse(extra_production, 1.2, 1.0)
  
  model <- MIPModel() %>%
    
    # Decision Variables
    add_variable(x1[j], type = "continuous", lb = 0, j = 1:weeks) %>%
    add_variable(x2[j], type = "continuous", lb = 0, j = 1:weeks) %>%
    add_variable(x3[j], type = "continuous", lb = 0, j = 1:weeks) %>%
    add_variable(y1[j], type = "integer", lb = 0, ub = num_factories[1], j = 1:weeks) %>%
    add_variable(y2[j], type = "integer", lb = 0, ub = num_factories[2], j = 1:weeks) %>%
    add_variable(y3[j], type = "integer", lb = 0, ub = num_factories[3], j = 1:weeks) %>%
    
    # Objective Function: Minimize Total Cost
    set_objective(
      sum_expr(
        setup_cost[1] * y1[j] + weekly_cost_min[1] * y1[j] + weekly_cost_per_unit[1] * (x1[j] - min_rate[1] * y1[j]) +
          setup_cost[2] * y2[j] + weekly_cost_min[2] * y2[j] + weekly_cost_per_unit[2] * (x2[j] - min_rate[2] * y2[j]) +
          setup_cost[3] * y3[j] + weekly_cost_min[3] * y3[j] + weekly_cost_per_unit[3] * (x3[j] - min_rate[3] * y3[j]),
        j = 1:weeks
      ), "min"
    ) %>%
    
    # Demand Constraint (With or Without 20% Buffer)
    add_constraint(x1[j] + x2[j] + x3[j] >= buffer_multiplier * weekly_demand[j], j = 1:weeks) %>%
    
    # Factory Constraints
    add_constraint(min_rate[1] * y1[j] <= x1[j], j = 1:weeks) %>%
    add_constraint(x1[j] <= max_rate[1] * y1[j], j = 1:weeks) %>%
    add_constraint(min_rate[2] * y2[j] <= x2[j], j = 1:weeks) %>%
    add_constraint(x2[j] <= max_rate[2] * y2[j], j = 1:weeks) %>%
    add_constraint(min_rate[3] * y3[j] <= x3[j], j = 1:weeks) %>%
    add_constraint(x3[j] <= max_rate[3] * y3[j], j = 1:weeks)
  
  # Solve the Model
  result <- solve_model(model, with_ROI(solver = "glpk",verbose = TRUE, control = list(tm_limit = 60000)))
  
  
  # Extract Results
  total_cost <- objective_value(result)
  
  return(list(result = result, cost = total_cost))
}

# Solve for both cases
solution_with_buffer <- solve_factory_model(extra_production = TRUE)
solution_without_buffer <- solve_factory_model(extra_production = FALSE)

# Compute the cost difference
cost_difference <- solution_with_buffer$cost - solution_without_buffer$cost

# Print Results
cat("\n--- Cost Analysis ---\n")
cat("Total Cost WITH 20% Extra Production Provision: ", solution_with_buffer$cost, "𐓌\n")
cat("Total Cost WITHOUT Extra Production Provision: ", solution_without_buffer$cost, "𐓌\n")
cat("Difference in Cost: ", cost_difference, "𐓌\n")

# Extract Factory Setup Decisions
solution_y1 <- get_solution(solution_with_buffer$result, y1[j])
solution_y2 <- get_solution(solution_with_buffer$result, y2[j])
solution_y3 <- get_solution(solution_with_buffer$result, y3[j])

# Print Number of Factories Active Each Week
cat("\n--- Number of Factories Active Each Week ---\n")
for (j in 1:weeks) {
  cat("Week", j, ": Class 1 =", solution_y1$value[j],
      ", Class 2 =", solution_y2$value[j],
      ", Class 3 =", solution_y3$value[j], "\n")
}

# Extract Production Decisions
solution_x1 <- get_solution(solution_with_buffer$result, x1[j]) %>%
  filter(value > 0)
solution_x2 <- get_solution(solution_with_buffer$result, x2[j]) %>%
  filter(value > 0)
solution_x3 <- get_solution(solution_with_buffer$result, x3[j]) %>%
  filter(value > 0)

# Print Production Decisions
cat("\n--- Production Decisions (Filtered) ---\n")
print(solution_x1)
print(solution_x2)
print(solution_x3)

# Extract Factory Setup Decisions (Filtered)
solution_y1 <- get_solution(solution_with_buffer$result, y1[j]) %>%
  filter(value > 0)
solution_y2 <- get_solution(solution_with_buffer$result, y2[j]) %>%
  filter(value > 0)
solution_y3 <- get_solution(solution_with_buffer$result, y3[j]) %>%
  filter(value > 0)

# Print Factory Setup Decisions
cat("\n--- Factory Setup Decisions (Filtered) ---\n")
print(solution_y1)
print(solution_y2)
print(solution_y3)

# 1. Prepare Data ----------------------------------------------------------
# Extract ALL production values (without filtering)
production_data <- data.frame(
  Week = 1:weeks,
  Class1 = get_solution(solution_with_buffer$result, x1[j])$value,
  Class2 = get_solution(solution_with_buffer$result, x2[j])$value,
  Class3 = get_solution(solution_with_buffer$result, x3[j])$value
) %>% 
  mutate(Total = Class1 + Class2 + Class3,
         Demand = weekly_demand,
         Buffered_Demand = weekly_demand * 1.2)

# Extract factory counts
factory_data <- data.frame(
  Week = 1:weeks,
  Class1 = get_solution(solution_with_buffer$result, y1[j])$value,
  Class2 = get_solution(solution_with_buffer$result, y2[j])$value,
  Class3 = get_solution(solution_with_buffer$result, y3[j])$value
)

# 2. Production vs Demand Plot --------------------------------------------
production_data %>% 
  pivot_longer(cols = c(Total, Demand, Buffered_Demand), 
               names_to = "Metric") %>%
  ggplot(aes(x = Week, y = value, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(title = "Production vs Demand (with 20% Buffer)",
       y = "Units",
       color = "Metric") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, weeks, 2))

# 3. Factory Utilization Plot ---------------------------------------------
factory_data %>% 
  pivot_longer(cols = -Week, names_to = "Class", values_to = "Count") %>%
  ggplot(aes(x = Week, y = Count, fill = Class)) +
  geom_col(position = "dodge") +
  labs(title = "Factory Utilization by Class",
       y = "Number of Factories") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(1, weeks, 2))

# 4. Production Breakdown Plot --------------------------------------------
production_data %>% 
  pivot_longer(cols = c(Class1, Class2, Class3), 
               names_to = "Class", values_to = "Production") %>%
  ggplot(aes(x = Week, y = Production, fill = Class)) +
  geom_area(position = "stack") +
  labs(title = "Production Breakdown by Factory Class",
       y = "Units Produced") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
  scale_x_continuous(breaks = seq(1, weeks, 2))

# 5. Cost Comparison Chart ------------------------------------------------
cost_data <- data.frame(
  Scenario = c("With Buffer", "Without Buffer"),
  Total_Cost = c(solution_with_buffer$cost, solution_without_buffer$cost)
)

ggplot(cost_data, aes(x = Scenario, y = Total_Cost, fill = Scenario)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = format(Total_Cost, big.mark = ",")), 
            vjust = -0.5) +
  labs(title = "Total Cost Comparison",
       y = "Total Cost (𐓌)") +
  theme_minimal()









