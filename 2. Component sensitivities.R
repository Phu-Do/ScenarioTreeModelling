library(tidyverse)
library(scales)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)


component_calculations = component_sens %>% 
  group_by(sex, symptom, location) %>% 
  summarise(mean = mean(value))

component_calculations


#Component sensitivities (males)
symsemales <- component_calculations %>%
  filter(sex == "men", symptom == "symptom", location == "gp") %>%
  pull(mean) #GP


symsemales1 <- component_calculations %>%
  filter(sex == "men", symptom == "symptom", location == "community") %>%
  pull(mean) #Community health


symsemales2 <- component_calculations %>%
  filter(sex == "men", symptom == "symptom", location == "sexhealth") %>%
  pull(mean)  #Sexual Health Clinic

symsemales3 <- component_calculations %>%
  filter(sex == "men", symptom == "symptom", location == "tertiary") %>%
  pull(mean) #Tertiary Health Clinics

#Component senstivities asymptomatic males
asymsemales <- component_calculations %>%
  filter(sex == "men", symptom == "asymptom", location == "gp") %>%
  pull(mean) #gp

asymsemales1 <- component_calculations %>%
  filter(sex == "men", symptom == "asymptom", location == "community") %>%
  pull(mean)  ##Community Health
 
asymsemales2 <- component_calculations %>%
  filter(sex == "men", symptom == "asymptom", location == "sexhealth") %>%
  pull(mean) ## Sexual Health Clinic

asymsemales3 <- component_calculations %>%
  filter(sex == "men", symptom == "asymptom", location == "tertiary") %>%
  pull(mean) ## Tertiary Care

#Component sensitivities (females)
symsefemales <- component_calculations %>%
  filter(sex == "women", symptom == "symptom", location == "gp") %>%
  pull(mean) #GP women
  
symsefemales1 <- component_calculations %>%
  filter(sex == "women", symptom == "symptom", location == "community") %>%
  pull(mean) #Community women

symsefemales2 <- component_calculations %>%
  filter(sex == "women", symptom == "symptom", location == "sexhealth") %>%
  pull(mean) #Sexual health women
  
symsefemales3 <- component_calculations %>%
  filter(sex == "women", symptom == "symptom", location == "tertiary") %>%
  pull(mean)

#Component sensitivities asymptomatic females
asymsefemales <- component_calculations %>%
  filter(sex == "women", symptom == "asymptom", location == "gp") %>%
  pull(mean) #Asymptomatic women gp

asymsefemales1 <- component_calculations %>%
  filter(sex == "women", symptom == "asymptom", location == "community") %>%
  pull(mean) #Asymptomatic women community

asymsefemales2 <- component_calculations %>%
  filter(sex == "women", symptom == "asymptom", location == "sexhealth") %>%
  pull(mean) #Asymptomatic women sexhealth
  
asymsefemales3 <- component_calculations %>%
  filter(sex == "women", symptom == "asymptom", location == "tertiary") %>%
  pull(mean) #Asymptomatic women tertiary


#Font file

font = "HelveticaNeueLT Pro 55 Roman"

## Creating the data

x1 = seq(0, 55, by = 1)
y1 = 1 - (1 - symsemales) ^ (x1)
percentile1 <- (log(100) / (log(1 - symsemales)))
abs(percentile1)
df <- data.frame(x1, y1)

p1 <- df %>%
  ggplot(aes(x = x1, y = y1)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Symptomatic male GP") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = abs(percentile1),
    linetype = "dashed",
    colour = "Red",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile1), 0),
    x = 45,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p1

x2 = seq(0, 340, by = 1)
y2 = 1 - (1 - symsemales1) ^ (x2)
df2 <- data.frame(x2, y2)
percentile2 <- (log(100) / (log(1 - symsemales1)))

p2 <- df2 %>%
  ggplot(aes(x = x2, y = y2)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Symptomatic male\ncommunity health ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = abs(percentile2),
    linetype = "dashed",
    colour = "Red",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile2), 0),
    x = 250,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) + theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p2

x3 = seq(0, 340, by = 1)
y3 = 1 - (1 - symsemales2) ^ (x3)
percentile3 = abs((log(100) / (log(1 - symsemales2))))
df3 <- data.frame(x3, y3)

p3 <- df3 %>%
  ggplot(aes(x = x3, y = y3)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Symptomatic male\nsexual health clinic ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = percentile3,
    linetype = "dashed",
    colour = "Red",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile3), 0),
    x = 200,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p3


x4 = seq(0, 1600, by = 1)
y4 = 1 - (1 - symsemales3) ^ (x4)
df4 <- data.frame(x4, y4)
percentile4 = abs((log(100) / (log(1 - symsemales3))))

p4 <- df4 %>%
  ggplot(aes(x = x4, y = y4)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Symptomatic male\ntertiary care ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = percentile4,
    linetype = "dashed",
    colour = "Red",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile4), 0),
    x = 1000,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p4


x5 = seq(0, 17000, by = 1)
y5 = 1 - (1 - asymsemales) ^ (x5)
df5 <- data.frame(x5, y5)
percentile5 = abs((log(100) / (log(1 - asymsemales))))

p5 <- df5 %>%
  ggplot(aes(x = x5, y = y5)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolate",
       title = "Asymptomatic male\nGP") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma,
    limit = c(0, 17000)
  ) +
  geom_vline(
    xintercept = percentile5,
    linetype = "dashed",
    colour = "Orange",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile5), 0),
    x = 13000,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) + theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p5

x6 = seq(0, 170000, by = 1)
y6 = 1 - (1 - asymsemales1) ^ (x6)
df6 <- data.frame(x6, y6)
percentile6 = abs((log(100) / (log(1 - asymsemales1))))

p6 <- df6 %>%
  ggplot(aes(x = x6, y = y6)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Asymptomatic male\ncommunity care ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = percentile6,
    linetype = "dashed",
    colour = "Orange",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile6), 0),
    x = 110000,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p6


x7 = seq(0, 5550, by = 1)
y7 = 1 - (1 - asymsemales2) ^ (x7)
df7 <- data.frame(x7, y7)
percentile7 = abs((log(100) / (log(1 - asymsemales2))))

p7 <- df7 %>%
  ggplot(aes(x = x7, y = y7)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Asymptomatic male\nsexual health") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma,
    limit = c(0, 5550)
  ) +
  geom_vline(
    xintercept = percentile7,
    linetype = "dashed",
    colour = "Orange",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile7), 0),
    x = 3600,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p7

x8 = seq(0, 750000, by = 1)
y8 = 1 - (1 - asymsemales3) ^ (x8)
df8 <- data.frame(x8, y8)
percentile8 = abs((log(100) / (log(1 - asymsemales3))))

p8 <- df8 %>%
  ggplot(aes(x = x8, y = y8)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Asymptomatic male \n tertiary") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 3),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = percentile8,
    linetype = "dashed",
    colour = "Orange",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile8), 0),
    x = 500000,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) + 
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p8


pfinal = list(p1, p2, p3, p4, p5, p6, p7, p8) %>%
  map(~ .x + labs(x = NULL, y = NULL))



yleft <- textGrob(expression(bold("Probability of detection")), rot = 90, gp = gpar(fontfamily = font))
bottom <- textGrob(expression(bold("Number of isolates processed")), gp = gpar(fontfamily = font))
top <- textGrob(expression(bold("Detection probability by number of isolates tested using system component sensitivities (CSe)")),
                gp = gpar(fontfamily = font))

uni <- grid.arrange(grobs = pfinal,
                    nrow = 4)

# Female Components

x10 = seq(0, 750, by = 1)
y10 = 1 - (1 - symsefemales) ^ (x10)
fpercentile1 <- (log(100) / (log(1 - symsefemales)))
abs(fpercentile1)
df10 <- data.frame(x10, y10)


p10 <- df10 %>%
  ggplot(aes(x = x10, y = y10)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Symptomatic female GP \n") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = abs(fpercentile1),
    linetype = "dashed",
    colour = "purple",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(fpercentile1), 0),
    x = 500,
    y = 0.6,
    size = 5, family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p10

x11 = seq(0, 3550, by = 1)
y11 = 1 - (1 - symsefemales1) ^ (x11)
df11 <- data.frame(x11, y11)
fpercentile2 <- (log(100) / (log(1 - symsefemales1)))

p11 <- df11 %>%
  ggplot(aes(x = x11, y = y11)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Symptomatic female \n community health ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma,
    limits = c(0, 3550)
  ) +
  geom_vline(
    xintercept = abs(fpercentile2),
    linetype = "dashed",
    colour = "purple",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 4),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(fpercentile2), 0),
    x = 2900,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p11

x12 = seq(0, 3400, by = 1)
y12 = 1 - (1 - symsefemales2) ^ (x12)
fpercentile3 = abs((log(100) / (log(1 - symsefemales2))))
df12 <- data.frame(x12, y12)

p12 <- df12 %>%
  ggplot(aes(x = x12, y = y12)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Symptomatic female \n sexual health clinic ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma,
    limits = c(0, 3400)
  ) +
  geom_vline(
    xintercept = fpercentile3,
    linetype = "dashed",
    colour = "purple",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(fpercentile3), 0),
    x = 2500,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p12


x13 = seq(0, 16750, by = 1)
y13 = 1 - (1 - symsefemales3) ^ (x13)
df13 <- data.frame(x13, y13)
fpercentile4 = abs((log(100) / (log(1 - symsefemales3))))

p13 <- df13 %>%
  ggplot(aes(x = x13, y = y13)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Symptomatic female \n tertiary care ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = fpercentile4,
    linetype = "dashed",
    colour = "purple",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(fpercentile4), 0),
    x = 10000,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) + 
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p13


x14 = seq(0, 5000, by = 1)
y14 = 1 - (1 - asymsefemales) ^ (x14)
df14 <- data.frame(x14, y14)
fpercentile5 = abs((log(100) / (log(1 - asymsefemales))))

p14 <- df14 %>%
  ggplot(aes(x = x14, y = y14)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolate",
       title = "Asymptomatic female\nGP") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 3),
    labels = scales::comma,
  limit = c(0, 5000) ) +
  geom_vline(
    xintercept = fpercentile5,
    linetype = "dashed",
    colour = "blue",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(fpercentile5), 0),
    x = 3800,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p14

x15 = seq(0, 32500, by = 1)
y15 = 1 - (1 - asymsefemales1) ^ (x15)
df15 <- data.frame(x15, y15)
fpercentile6 = abs((log(100) / (log(1 - asymsefemales1))))

p15 <- df15 %>%
  ggplot(aes(x = x15, y = y15)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Asymptomatic female \n community care ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma,
    limits = c(0, 32500)
  ) +
  geom_vline(
    xintercept = fpercentile6,
    linetype = "dashed",
    colour = "blue",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 4),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(fpercentile6), 0),
    x = 25000,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p15


x16 = seq(0, 15000, by = 1)
y16 = 1 - (1 - asymsefemales2) ^ (x16)
df16 <- data.frame(x16, y16)
fpercentile7 = abs((log(100) / (log(1 - asymsefemales2))))

p16 <- df16 %>%
  ggplot(aes(x = x16, y = y16)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Asymptomatic female \n sexual health ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma,
    limits = c(0, 1250)
  ) +
  geom_vline(
    xintercept = fpercentile7,
    linetype = "dashed",
    colour = "blue",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(fpercentile7), 0),
    x = 850,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p16

x17 = seq(0, 160000, by = 1)
y17 = 1 - (1 - asymsefemales3) ^ (x17)
df17 <- data.frame(x17, y17)
fpercentile8 = abs((log(100) / (log(1 - asymsefemales3))))

p17 <- df17 %>%
  ggplot(aes(x = x17, y = y17)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Asymptomatic female\ntertiary") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma,
    limits = c(0, 160000)
  ) +
  geom_vline(
    xintercept = fpercentile8,
    linetype = "dashed",
    colour = "blue",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(fpercentile8), 0),
    x = 100000,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p17


pfinal1 <- list(p10, p11, p12, p13, p14, p15, p16, p17) %>%
  map(~ .x + labs(x = NULL, y = NULL, family = font))  # apply font family to the plots

# create grobs for the text labels
yleft <- textGrob(expression(bold("Probability of detection")), rot = 90, gp = gpar(fontfamily = font))
bottom <- textGrob(expression(bold("Number of isolates processed")), gp = gpar(fontfamily = font))
top <- textGrob(expression(bold("Detection probability by number of isolates tested using system component sensitivities (CSe)")),
                gp = gpar(fontfamily = font))


 
# pfinal1 = list(p10, p11, p12, p13, p14, p15, p16, p17) %>%
#   map(~ .x + labs(x = NULL, y = NULL))
# yleft <-
#   textGrob(expression(bold("Probability of detection")), rot = 90)
# bottom <- textGrob(expression(bold("Number of isolates processed")))
# top <-
#   textGrob(expression(
#     bold(
#       "Detection probability by number of isolates tested using system component sensitivities (CSe)"
#     )
#   ))


uni1 <- grid.arrange(grobs = pfinal1,
                     nrow = 4)
uni

uni1

grid.arrange(
  uni,
  uni1,
  ncol = 2,
  left = yleft,
  bottom = bottom,
  top = top
)

figure = ggarrange(uni, uni1, labels = c("A", "B"))
figure

grid.arrange(figure,
             left = yleft,
             bottom = bottom,
             top = top,
             labels = )

#Total
x_total = seq(0, 100, by = 1)
y_total = 1 - (1 - systemsens) ^ (x_total)
df_total <- data.frame(x_total, y_total)
percentile6 = abs((log(100) / (log(1 - systemsens))))

p6 <- df6 %>%
  ggplot(aes(x = x6, y = y6)) +
  geom_line(size = 1) + theme_pubr() +
  labs(y = "Probability of detection",
       x = "Number of isolates tested",
       title = "Asymptomatic male \n community care ") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::comma
  ) +
  geom_vline(
    xintercept = percentile6,
    linetype = "dashed",
    colour = "Orange",
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, 1)
  ) + annotate(
    "text",
    label = round(abs(percentile6), 0),
    x = 125000,
    y = 0.6,
    size = 5,
    family = "HelveticaNeueLT Pro 55 Roman"
  ) +
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman"))
p6


#-----------------------------------------------------
#Component sensitivity calculations using report data
#-----------------------------------------------------

nmales = 5598
nfemales = 1580

#Component sensitivities (males)
symsemales <- component_calculations %>%
  filter(sex == "men", symptom == "symptom", location == "gp") %>%
  pull(mean) #GP


symsemales1 <- component_calculations %>%
  filter(sex == "men", symptom == "symptom", location == "community") %>%
  pull(mean) #Community health


symsemales2 <- component_calculations %>%
  filter(sex == "men", symptom == "symptom", location == "sexhealth") %>%
  pull(mean)  #Sexual Health Clinic

symsemales3 <- component_calculations %>%
  filter(sex == "men", symptom == "symptom", location == "tertiary") %>%
  pull(mean) #Tertiary Health Clinics

#Component senstivities asymptomatic males
asymsemales <- component_calculations %>%
  filter(sex == "men", symptom == "asymptom", location == "gp") %>%
  pull(mean) #gp

asymsemales1 <- component_calculations %>%
  filter(sex == "men", symptom == "asymptom", location == "community") %>%
  pull(mean)  ##Community Health

asymsemales2 <- component_calculations %>%
  filter(sex == "men", symptom == "asymptom", location == "sexhealth") %>%
  pull(mean) ## Sexual Health Clinic

asymsemales3 <- component_calculations %>%
  filter(sex == "men", symptom == "asymptom", location == "tertiary") %>%
  pull(mean) ## Tertiary Care

#Component sensitivities (females)
symsefemales <- component_calculations %>%
  filter(sex == "women", symptom == "symptom", location == "gp") %>%
  pull(mean) #GP women

symsefemales1 <- component_calculations %>%
  filter(sex == "women", symptom == "symptom", location == "community") %>%
  pull(mean) #Community women

symsefemales2 <- component_calculations %>%
  filter(sex == "women", symptom == "symptom", location == "sexhealth") %>%
  pull(mean) #Sexual health women

symsefemales3 <- component_calculations %>%
  filter(sex == "women", symptom == "symptom", location == "tertiary") %>%
  pull(mean)

#Component sensitivities asymptomatic females
asymsefemales <- component_calculations %>%
  filter(sex == "women", symptom == "asymptom", location == "gp") %>%
  pull(mean) #Asymptomatic women gp

asymsefemales1 <- component_calculations %>%
  filter(sex == "women", symptom == "asymptom", location == "community") %>%
  pull(mean) #Asymptomatic women community

asymsefemales2 <- component_calculations %>%
  filter(sex == "women", symptom == "asymptom", location == "sexhealth") %>%
  pull(mean) #Asymptomatic women sexhealth

asymsefemales3 <- component_calculations %>%
  filter(sex == "women", symptom == "asymptom", location == "tertiary") %>%
  pull(mean) #Asymptomatic women tertiary


#-----------------------------------------------------
# Function for calculation
#-----------------------------------------------------

prob_detection <- function(CSe, n) {
  1 - ((1 - CSe)^n)
}


#-----------------------------------------------------
# Writing a loop
#-----------------------------------------------------
# Create an empty data frame to store the results
probability_df <- data.frame(
  Component = character(),
  CSe = numeric(),
  N = numeric(),
  Prob_Detection = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each component and calculate the probability of detection for males and females
for (component in c("GP", "Community Health", "Sexual Health Clinic", "Tertiary Care")) {
  # Calculate probability of detection for males
  prob_detect_symmales <- prob_detection(CSe = switch(
    component,
    "GP" = symsemales,
    "Community Health" = symsemales1,
    "Sexual Health Clinic" = symsemales2,
    "Tertiary Care" = symsemales3
  ),
  n = nmales)
  
  # Calculate probability of detection for males
  prob_detect_asymmales <- prob_detection(CSe = switch(
    component,
    "GP" = asymsemales,
    "Community Health" = asymsemales1,
    "Sexual Health Clinic" = asymsemales2,
    "Tertiary Care" = asymsemales3
  ),
  n = nmales)
  
  # Store the results in the data frame for males
  probability_df <- rbind(
    probability_df,
    data.frame(
      Component = paste0(component, " Symptomatic (Males)"),
      CSe = switch(
        component,
        "GP" = symsemales,
        "Community Health" = symsemales1,
        "Sexual Health Clinic" = symsemales2,
        "Tertiary Care" = symsemales3
      ),
      N = nmales,
      Prob_Detection = prob_detect_symmales
    )
  )
  
  probability_df <- rbind(
    probability_df,
    data.frame(
      Component = paste0(component, " Asymptomatic (Males)"),
      CSe = switch(
        component,
        "GP" = asymsemales,
        "Community Health" = asymsemales1,
        "Sexual Health Clinic" = asymsemales2,
        "Tertiary Care" = asymsemales3
      ),
      N = nmales,
      Prob_Detection = prob_detect_asymmales
    )
  )
  
  
  
  
  # Calculate probability of detection for females
  prob_detect_symfemales <- prob_detection(CSe = switch(
    component,
    "GP" = symsefemales,
    "Community Health" = symsefemales1,
    "Sexual Health Clinic" = symsefemales2,
    "Tertiary Care" = symsefemales3
  ),
  n = nfemales)
  
  
  prob_detect_asymfemales <- prob_detection(CSe = switch(
    component,
    "GP" = asymsefemales,
    "Community Health" = asymsefemales1,
    "Sexual Health Clinic" = asymsefemales2,
    "Tertiary Care" = asymsefemales3
  ),
  n = nfemales)
  
  # Store the results in the data frame for females
  probability_df <- rbind(
    probability_df,
    data.frame(
      Component = paste0(component, " Symptomatic (Females)"),
      CSe = switch(
        component,
        "GP" = symsefemales,
        "Community Health" = symsefemales1,
        "Sexual Health Clinic" = symsefemales2,
        "Tertiary Care" = symsefemales3
      ),
      N = nfemales,
      Prob_Detection = prob_detect_symfemales
    )
  )
  
  probability_df <- rbind(
    probability_df,
    data.frame(
      Component = paste0(component, " Asymptomatic (Females)"),
      CSe = switch(
        component,
        "GP" = asymsefemales,
        "Community Health" = asymsefemales1,
        "Sexual Health Clinic" = asymsefemales2,
        "Tertiary Care" = asymsefemales3
      ),
      N = nfemales,
      Prob_Detection = prob_detect_asymfemales
    )
  )
  
  
  #Storing results for asymptomatic females
  
  
}

# View the results
probability_df



data = probability_df

#----------------------------------
# Reformating the data for
# Calculation of probabilities
#----------------------------------

library(ggplot2)

ggplot(probability_df,
       aes(x = Component, y = Prob_Detection, fill = factor(N))) +
  geom_bar(stat = "identity",
           position = "dodge",
           alpha = 0.8) +
  scale_fill_manual(values = c("#FF6666", "#66CCFF"),
                    labels = c("Males", "Females")) +
  coord_flip() +
  labs(y = "Probability of detection", x = "", title = "Probability of detection by system component") +
  theme_pubr() + scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "right") + geom_text(aes(label = round(Prob_Detection, 2)),
                                               position = position_dodge(width = 1),
                                               hjust = 1)


# Extract healthcare facility
data$Healthcare_facility <-
  ifelse(
    grepl("Tertiary Care", data$Component),
    "Tertiary Care",
    ifelse(
      grepl("Sexual Health Clinic", data$Component),
      "Sexual Health Clinic",
      ifelse(
        grepl("GP", data$Component),
        "GP",
        ifelse(
          grepl("Community Health", data$Component),
          "Community Health",
          NA
        )
      )
    )
  )

# Extract symptom status and sex
data$Symptom_status <-
  ifelse(
    grepl("Symptomatic", data$Component),
    "Symptomatic",
    ifelse(grepl("Asymptomatic", data$Component), "Asymptomatic", NA)
  )

data$Sex <- ifelse(grepl("Male", data$Component),
                   "Male",
                   ifelse(grepl("Female", data$Component), "Female", NA))

# Drop the original Component column
data$Component <- NULL

# Reorder the columns
data <-
  data[, c("Healthcare_facility",
           "Symptom_status",
           "Sex",
           "CSe",
           "N",
           "Prob_Detection")]

# View the final result
data


#Graphing the results

data %>%
  ggplot(aes(
    x = reorder(Healthcare_facility, Prob_Detection),
    y = Prob_Detection,
    fill = Healthcare_facility
  )) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(Sex), row = vars(Symptom_status)) +
  theme_pubclean() +
  geom_text(aes(label = round(Prob_Detection * 100, 1)),
            hjust = 0.5) + coord_flip() +
  scale_fill_manual(
    values = c("#F44336", "#FFC107", "#4CAF50", "#2196F3"),
    labels = c(
      "Community\nhealth",
      "General\npractice",
      "Sexual health\nclinic",
      "Tertiary\ncare"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(
    x = "",
    y = "Probability of detection",
    caption = "Values based off number of samples processed within\nAustralian Gonococcal Surveillance Program 2020 report",
    fill = "Surveillance\nsystem component"
  )


data %>%
  ggplot(aes(
    x = reorder(Healthcare_facility, Prob_Detection),
    y = Prob_Detection,
    fill = Healthcare_facility
  )) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(Sex), row = vars(Symptom_status)) +
  theme_Publication() +
  geom_text(aes(label = paste0(round(
    Prob_Detection * 100, 1
  ), "%")),
  position = position_stack(vjust = 0.83)) +
  coord_flip() +
  scale_fill_manual(
    values = c("#F44336", "#FFC107", "#4CAF50", "#2196F3"),
    labels = c(
      "Community\nhealth",
      "General\npractice",
      "Sexual health\nclinic",
      "Tertiary\ncare"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0),
    breaks = scales::pretty_breaks(n = 4)
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(1, "cm")
  ) +
  labs(
    x = "",
    y = "Probability of detection",
    caption = "Values based off annual number of samples processed within\nAustralian Gonococcal Surveillance Program 2020 report",
    fill = "Surveillance\nsystem component"
  )
