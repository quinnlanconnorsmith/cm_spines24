# Install and load required packages
library(ggplot2)
library(tidyverse)
library(minpack.lm)


# Example data 

library(readr)
example_spine_data <- read_csv("example_spine_data.csv")
View(example_spine_data)

example_spine_data <-subset(example_spine_data, select = -basin)
example_spine_data <- example_spine_data[example_spine_data$spp != "BLG", ]
example_spine_data <- na.omit(example_spine_data)

# Plotting the data
ggplot(example_spine_data, aes(x = age_1)) +
  geom_point(aes(y = length), color = "blue", size = 3) +
  geom_point(aes(y = weight), color = "red", size = 3) +
  labs(x = "Age", y = "Length/Weight") +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Weight (g)")) + # Adding secondary y-axis for weight
  theme_minimal()

# Fit von Bertalanffy growth curve
fit_vb_length <- nlsLM(length ~ L_inf * (1 - exp(-k * age_1)), data = example_spine_data, start = list(L_inf = 35, k = 0.2))
fit_vb_weight <- nlsLM(weight ~ W_inf * (1 - exp(-k * age_1)), data = example_spine_data, start = list(W_inf = 100, k = 0.2))

# Predicted values from the fitted model
predicted_lengths <- predict(fit_vb_length)
predicted_weights <- predict(fit_vb_weight)

# Adding fitted growth curves to the plot
plot_data <- data.frame(age = example_spine_data$age_1, predicted_lengths, predicted_weights)
ggplot() +
  geom_point(data = example_spine_data, aes(x = age_1, y = length), color = "blue", size = 3) +
  geom_point(data = example_spine_data, aes(x = age_1, y = weight), color = "red", size = 3) +
  geom_line(data = plot_data, aes(x = age, y = predicted_lengths), color = "blue", linetype = "dashed") +
  geom_line(data = plot_data, aes(x = age, y = predicted_weights), color = "red", linetype = "dashed") +
  labs(x = "Age", y = "Length/Weight") +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Weight (g)")) + # Adding secondary y-axis for weight
  theme_minimal()


#Remove weight bogus

plot_data <- data.frame(age = example_spine_data$age_1, predicted_lengths, predicted_weights)
ggplot() +
  geom_point(data = example_spine_data, aes(x = age_1, y = length), color = "blue", size = 3) +
  geom_line(data = plot_data, aes(x = age, y = predicted_lengths), color = "blue", linetype = "dashed") +
  labs(x = "Age", y = "Length") +
  ylim(100,500) +
  theme_minimal()


#Round 2 

# Initial parameter estimates
Linfinity_guess <- max(example_spine_data$length)
K_guess <- 0.23 #From Hesler and Lai
t0_guess <- min(example_spine_data$age_1)

# Fit the von Bertalanffy growth curve using nls
vb_fit <- nls(length ~ Linfinity * (1 - exp(-K * (age_1 - t0))), 
              data = example_spine_data, 
              start = list(Linfinity = Linfinity_guess, K = K_guess, t0 = t0_guess))

# View the summary of the fitted model
summary(vb_fit)

#Linfinity = 392.60329
#K = 0.23181
#t0 = 0.08702

# Visualize the fitted growth curve along with the data
plot(example_spine_data$age_1, example_spine_data$length, xlab = "Age", ylab = "Length", main = "Fish Growth Data")
curve(predict(vb_fit, newdata = data.frame(age_1 = x)), add = TRUE, col = "red")

#New attempt 
#Ggplot 

ggplot(example_spine_data, aes(x = age_1, y = length)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ Linfinity * (1 - exp(-K * (x - t0))),
              method.args = list(start = coef(vb_fit)),
              se = FALSE, 
              color = "red") +
  labs(x = "Age", y = "Length", title = "Fish Growth Data") +
  theme_minimal()

#Boxplot data! 

boxplot_data <- example_spine_data %>%
  mutate(age_class = cut(age_1, breaks = seq(min(age_1), max(age_1), by = 1)))  # You can adjust the breaks as per your age intervals

ggplot() +
  geom_boxplot(data = boxplot_data, aes(x = age_1, y = length, group=age_1), width = 0.5, fill = "lightblue") +
  geom_point(data = example_spine_data, aes(x = age_1, y = length)) +
  geom_smooth(data = example_spine_data, aes(x = age_1, y = length), method = "nls", 
              formula = y ~ Linfinity * (1 - exp(-K * (x - t0))),
              method.args = list(start = coef(vb_fit)),
              se = FALSE, 
              color = "red") +
  labs(x = "Age (years)", y = "Length (mm)") +
  scale_x_continuous(limits = c(2.5, 12.5), breaks = seq(3, 12, by = 1)) +
  theme_minimal()

# Calculate R-squared for the growth curve
model <- nls(length ~ Linfinity * (1 - exp(-K * (age_1 - t0))), 
             data = example_spine_data, 
             start = coef(vb_fit))

# Get the predicted values from the model
predicted_values <- predict(model)

# Calculate residuals
residuals <- residuals(model)

# Calculate total sum of squares (TSS)
TSS <- sum((example_spine_data$length - mean(example_spine_data$length))^2)

# Calculate residual sum of squares (RSS)
RSS <- sum(residuals^2)

# Calculate R-squared
r_squared <- 1 - (RSS / TSS)

# Print or use the R-squared value
print(r_squared)

