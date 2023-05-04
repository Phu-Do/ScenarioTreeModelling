mu = 0
variance = 0.004758
sigma = sqrt(variance)

df <- data.frame(x = seq(-4, 4, length.out = 1000))
df$y <- dnorm(df$x, mean = mu, sd = 1)

cv = 1.96

# Plot the normal distribution using ggplot2
ggplot(df, aes(x = x, y = y)) + 
  geom_line(size = 1) +
  labs(x = expression(paste(T)),
       y = "Density",
       title = expression(paste("Sampling distribution for T with ", mu, " = 0"," ", sigma, " = 1")),
       caption = "Author: Phu Do",
       fill = "Area greater than test statistic (T)") +
  theme_Publication() +
  geom_vline(xintercept = -1.96, colour = "red") +
  geom_vline(xintercept = 1.96, colour = "red") +
  geom_vline(xintercept = 0.869, colour = "lightblue") +
  geom_ribbon(aes(x = x, ymin = 0, ymax = y, fill = x > 0.869), alpha = 0.2) +
  scale_fill_manual(values = c("pink", "blue"),
                    labels = c("Less\nthan", "Greater\nthan")) +
  scale_y_continuous(expand = c(0,0)) 






SE = sqrt((0.36*0.64)/100 + (0.42*0.58)/100)
thetahat = 0.06


thetahat + 1.96*SE
thetahat - 1.96*SE
  

ggplot(df, aes(x = x, y = y)) + 
  geom_line(size = 1) +
  labs(x = expression(paste(T)),
       y = "Density",
       title = expression(paste("Sampling distribution for T with ", mu, " = 0"," ", sigma, " = 1")),
       caption = "Author: Phu Do",
       fill = "Area greater than\ntest statistic (T)") +
  theme_Publication() +
  geom_vline(xintercept = -1.96, colour = "blue") +
  geom_vline(xintercept = 1.96, colour = "blue") +
  geom_vline(xintercept = 0.869, colour = "purple") +
  geom_ribbon(aes(x = x, ymin = 0, ymax = y, fill = x > 0.869), alpha = 0.2) +
  scale_fill_manual(values = c("skyblue", "red"),
                    labels = c("Less\nthan", "Greater\nthan")) +
  scale_y_continuous(expand = c(0,0)) +
  geom_text(aes(x = 0.069, y = 0.2, label = "test statistic\n(T) = 0.869"), size = 4, vjust = -1)






# Shade the area above 0.06
df2 <- data.frame(x = seq(0.06, 0.2, length.out = 1000))
df2$y <- dnorm(df2$x, mean = mu, sd = sigma)


# Plot the normal distribution using ggplot2
ggplot(df, aes(x = x, y = y)) + 
  geom_line(size = 1) +
  geom_ribbon(data = df2, aes(x = x, ymax = y), ymin = 0, fill = "blue", alpha = 0.2) +
  labs(x = expression(paste(theta)),
       y = "Density",
       title = expression(paste("Sampling distribution with ", mu, " = 0", " ", sigma, " = 0.06")),
       caption = "Author: Phu Do") +
  theme_Publication() +
  geom_vline(xintercept = sigma)


alpha <- 0.05
beta <- 0.20
pro1 <- 0.40
d <- 0.10

(((-0.8416212*sqrt(0.98))-1.96)/(-0.2))^2



n1 <- ((1.96 + 0.8416212 * sqrt(0.98))^2) / 0.04
n1 <- ceiling(n1) # round up to nearest integer
n1


pach <- 0.4  # proportion achieving target SBP on current medication
pach1 <- 0.5  # assumed proportion achieving target SBP on new medication
es <- sqrt((pach*(1-pach) + pach1*(1-pach1)) / 2)  # calculate effect size

install.packages("pwr")
library(pwr)


library(pwr)
n <- pwr::pwr.p.test(h = es, n = NULL, sig.level = 0.05, power = 0.8, alternative = "two.sided")$n
n  # print sample size
pwr::
  
  
  
# Set parameters
alpha <- 0.05
power <- 0.8
p0 <- 0.4
delta <- 0.1

# Calculate required sample size
n <- pwr::pwr.2p.test(h = pwr::cohen.ES(p1 = p0, p2 = p0 + delta), n = NULL, sig.level = alpha, power = power)$n

n  # print the result




((0.5^2)/250) *2



log_likelihood <- function(gamma, p2, n1, x, n2, y) {
  # Calculate the log-likelihood using the given formula
  ll <- n1 * log(gamma * p2) +
    ((sum(x) - n1) * log(1 - (gamma * p2)) +
    n2 * log(p2) +
    ((sum(y) - n2) * log(1 - p2))
  return(ll)
}

gammahat = (78/207)/(51/212)
phat2 = 51/212
n1 = 78
x = 207
n2 = 51
y = 212

print(log_likelihood(gammahat, phat2, n1, x, n2, y))

print(log_likelihood(1, phat2, n1, x, n2, y))







# Compute log-likelihood for alternative hypothesis
ll_alternative = log_likelihood(gammahat, phat2, n1, x, n2, y)

# Compute log-likelihood for null hypothesis
ll_null = log_likelihood(1, phat2, n1, x, n2, y)

# Compute likelihood ratio test statistic
test_statistic = 2 * (ll_alternative - ll_null)

# The degree of freedom
degree_freedom = 1

#Computing the p value using the chi-squared distribution

p_value <- 1- pchisq(test_statistic, degree_freedom, 
                     lower.tail = TRUE)
#Printing the outputs
print("Likelihood Ratio Test Results:")
print(paste("Test Statistic: ", test_statistic))
print(paste("Degrees of Freedom: ", degree_freedom))
print(paste("P-value: ", p_value))




#Parameters for calculation
gammahat = (78/207)/(51/212)
gammapop = 1
phat2 = (51/212)
information = (78)/(gammahat^(2)*(1-gammahat*phat2))
infoinv = 0.056136
phat1 = 78/207
n1 = 207
n2 = 212

#Function for the wald statistic
approx_wald_statistic <- function(gamma_pop, gamma_hat, info_inv) {
  # Calculate the approximate Wald statistic
  wald_statistic <- ((gamma_hat - gamma_pop) / sqrt(info_inv))
  return(wald_statistic)
}

wald_stat <- approx_wald_statistic(gammapop, gammahat, infoinv)

# Print the result
cat("Approximate Wald statistic: ", wald_stat)


# Calculate the p-value using the cumulative distribution function (CDF) of the standard normal distribution
p_value <- 2 * (1 - pnorm(abs(wald_stat)))

# Print the result
cat("p-value:", p_value)







data = sensitivitydata
data$variable = as.factor(data$variable)
midpoint = (data$maximum + data$minimum)/2


# Reorder variable column based on maximum value
data <- data %>%
  mutate(variable = reorder(variable, maximum, FUN = function(x) max(x))) %>%
  arrange(variable)

print(data, n = 26)

data = data[-c(4,23),] 
print(data, n = 26)
 
ggplot(data, aes(x = variable, colour = "black")) +
  geom_segment(aes(x = variable, xend = variable, y = minimum, yend = maximum, 
                   color = variable), 
               size = 4, show.legend = FALSE) + 
  labs(x = "Variable", y = "Total system sensitivity (SSe)",
       caption = "Outputs from ModelRisk") +
  coord_flip() + theme_Publication() +
  geom_text(aes(label = round(maximum,3), y = maximum + 0.002, color = variable), 
            hjust = 0, vjust = 0.5, size = 3.5,
            family = "HelveticaNeueLT Pro 55 Roman") +
  geom_text(aes(label = round(minimum,3), y = minimum - 0.005, color = variable), 
            hjust = 0, vjust = 0.5, size = 3.5,
            family = "HelveticaNeueLT Pro 55 Roman") +
  theme(legend.position = "none") +
  scale_colour_manual(values = colors) +
  geom_hline(yintercept = midpoint, linetype = "dashed", size = 0.1,
             alpha = 0.25) 
  

  colors <- c("#00BFFF", "#FF4500", "#F44336", "#B22222", "#8A2BE2", "#8B4513", "#E9967A",
              "#808080", "#BDB76B", "#6495ED", "#228B22", "#D2691E", "#7B68EE", "#8B008B",
              "#228B3C", "#FFD700", "#A0522D", "#696969", "#AFEEEE", "#1E90FF", "#4CAF50",
              "#006400", "#DC143C", "#FFA500", "#E9967A", "#FF8C00", "#FFC107")

