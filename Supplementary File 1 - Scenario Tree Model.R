#-----------------------------------------------
# Scenario Tree Model for
# The Australian Gonococcal Surveillance Programme
#-----------------------------------------------

#-----------------------------------------------
# Author(s): Phu Cong Do, Yibeltal Assefa Alemu,
# Simon Andrew Reid
# University: The University of Queensland
# Affiliation: School of Public Health
# Address: 288 Herston Road, 4006, Queensland, 
# Australia
#-----------------------------------------------

#-----------------------------------------------
# Parameters given in the supplementary file
# set.seed(123456)
# Number of iterations 100,000
# Optional themes for graphs
# Please install fonts HelveticaNeue included 
# In supplementary file
#-----------------------------------------------

theme_Publication <- function(base_size=14, base_family="HelveticaNeueLT Pro 55 Roman") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="bold"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

theme_Publicationv <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "vertical",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}
#-----------------------------------------------
# Packages required
# library(mc2d) for PERT distributions
# tidyverse and reshape2 for data manipulation
#-----------------------------------------------
packages <- c("ggstance", "ggpubr", "reshape2", 
              "mc2d", "tidyverse", "extrafont",
              "grid", "gridExtra", "scales")
install.packages(packages)
lapply(packages, library, character.only = TRUE)

font_import(prompt = F) #Installing all the fonts for package
loadfonts(device = "win")

#-----------------------------------------------
# Building the physical model
#-----------------------------------------------

set.seed(123456)

#-----------------------------------------------
# Initialising the dataframe [Run this first]
# This must be run
#-----------------------------------------------

results = data.frame(
  iteration = numeric(0),
  systemsens = numeric(0),
  gonosens = numeric(0),
  women_symptom_gp = numeric(0),
  women_symptom_community = numeric(0),
  women_symptom_sexhealth = numeric(0),
  women_symptom_tertiary = numeric(0),
  women_asymptom_gp = numeric(0),
  women_asymptom_community = numeric(0),
  women_asymptom_sexhealth = numeric(0),
  women_asymptom_tertiary = numeric(0),
  en_symptom_gp = numeric(0),
  men_symptom_community = numeric(0),
  men_symptom_sexhealth = numeric(0),
  men_symptom_tertiary = numeric(0),
  men_asymptom_gp = numeric(0),
  men_asymptom_community = numeric(0),
  men_asymptom_sexhealth = numeric(0),
  men_asymptom_tertiary = numeric(0)
)

results_gono = data.frame(
  iteration = numeric(0),
  totalgonosens = numeric(0),
  gwomen_symptom_gp = numeric(0),
  gwomen_symptom_community = numeric(0),
  gwomen_symptom_sexhealth = numeric(0),
  gwomen_symptom_tertiary = numeric(0),
  gwomen_asymptom_gp = numeric(0),
  gwomen_asymptom_community = numeric(0),
  gwomen_asymptom_sexhealth = numeric(0),
  gwomen_asymptom_tertiary = numeric(0),
  gmen_symptom_gp = numeric(0),
  gmen_symptom_community = numeric(0),
  gmen_symptom_sexhealth = numeric(0),
  gmen_symptom_tertiary = numeric(0),
  gmen_asymptom_gp = numeric(0),
  gmen_asymptom_community = numeric(0),
  gmen_asymptom_sexhealth = numeric(0),
  gmen_asymptom_tertiary = numeric(0)
)

#-----------------------------------------------
# Initialising model with variables [Run after]
#-----------------------------------------------

options(scipen = 999) #Display all digits

niter <- 100000 #Number of iterations
progress_bar= txtProgressBar(min = 0, max = niter, style = 3)

for(i in 1:niter){
  #Initialising variables inside loop
  setTxtProgressBar(progress_bar, i)
  
  #-----------------------------------------------
  # initialising parameters with values
  # Differential risk parameters
  #-----------------------------------------------
  inf_women = 0.28
  inf_men = 0.72
  
  #-----------------------------------------------
  # initialising parameters with values
  # Clinical signs with healthcare access
  #-----------------------------------------------
  
  women_symptom = 0.20
  women_asymptom = 0.80
  
  wsym_healthaccess = runif(1, min = 0.8, max = 1)
  wasym_healthaccess = 0.65
  
  men_symptom = runif(1, min = 0.85, max = 0.95)
  men_asymptom = runif(1, min = 0.05, max = 0.15)
  
  msym_healthaccess = runif(1, min = 0.8, max = 1)
  masym_healthaccess = 0.52
  
  
  #-----------------------------------------------
  # initialising parameters with values
  # Proportions of testing at locations
  #-----------------------------------------------
  
  gp = rpert(1, 0.7, 0.75, 0.8)
  community = runif(1, min = 0.1, max = 0.175)
  sexhealth = runif(1, min = 0.1, max = 0.175)
  tertiary = runif(1, min = 0.01, max = 0.05)
  
  
  #-----------------------------------------------
  # initialising parameters with values
  # Proportions of testing at locations
  #-----------------------------------------------
  
  wscliniciantest_gp = rpert(1, 0.925, 0.9575, 0.99)
  wscliniciantest_community = rpert(1, 0.9, 0.95, 0.99)
  wscliniciantest_sexhealth = 0.99
  wscliniciantest_tertiary = rpert(1, 0.9, 0.95, 0.99)
  
  wasclinicicantest_gp = rpert(1, min = 0.046, 0.0465, 0.047)
  wasclinicicantest_community = runif(1, min = 0.02, max = 0.06)
  wascliniciantest_sexhealth = 0.99
  wascliniciantest_tertiary = runif(1, min = 0.02, max = 0.06)
  
  mscliniciantest_gp = rpert(1, 0.925, 0.9575, 0.99)
  mscliniciantest_community = rpert(1, 0.9, 0.95, 0.99)
  mscliniciantest_sexhealth = 0.99
  mscliniciantest_tertiary = rpert(1, 0.9, 0.95, 0.99)
  
  mascliniciantest_gp = rpert(1, 0.04, 0.0475, 0.055)
  mascliniciantest_community = runif(1, min = 0.01, max = 0.05)
  mascliniciantest_sexhealth = 0.99
  mascliniciantest_tertiary = runif(1, min = 0.01, max = 0.05)
  
  
  #-----------------------------------------------
  # initialising parameters with values
  # Diagnostic testing
  #-----------------------------------------------
  diagnostic_test = runif(1, min = 0.85, max = 1)
  sensitivity_culture = runif(1, min = 0.85, max = 1)
  
  #-----------------------------------------------
  # initialising parameters with values
  # Associated AST data
  #-----------------------------------------------
  
  ast_data = runif(1, 0.2, 0.3)
  
  #Symptomatic women
  
  women_symptom_gp <- inf_women * women_symptom * wsym_healthaccess * gp * wscliniciantest_gp * diagnostic_test * sensitivity_culture * ast_data
  women_symptom_community <- inf_women * women_symptom * wsym_healthaccess * community * wscliniciantest_community * diagnostic_test * sensitivity_culture * ast_data
  women_symptom_sexhealth <- inf_women * women_symptom * wsym_healthaccess * sexhealth * wscliniciantest_sexhealth * diagnostic_test * sensitivity_culture * ast_data
  women_symptom_tertiary <- inf_women * women_symptom * wsym_healthaccess * tertiary * wscliniciantest_tertiary * diagnostic_test * sensitivity_culture * ast_data
  
  #Asymptomatic women
  
  women_asymptom_gp <- inf_women * women_asymptom * wasym_healthaccess * gp * wasclinicicantest_gp * diagnostic_test * sensitivity_culture * ast_data
  women_asymptom_community <- inf_women * women_asymptom * wasym_healthaccess * community * wasclinicicantest_community * diagnostic_test * sensitivity_culture * ast_data
  women_asymptom_sexhealth <- inf_women * women_asymptom * wasym_healthaccess * sexhealth * wascliniciantest_sexhealth * diagnostic_test * sensitivity_culture * ast_data
  women_asymptom_tertiary <- inf_women * women_asymptom * wasym_healthaccess * tertiary * wascliniciantest_tertiary * diagnostic_test * sensitivity_culture * ast_data
  
  #Symptomatic men
  
  men_symptom_gp <- inf_men * men_symptom * msym_healthaccess * gp * mscliniciantest_gp * diagnostic_test * sensitivity_culture * ast_data
  men_symptom_community <- inf_men * men_symptom * msym_healthaccess * community * mscliniciantest_community * diagnostic_test * sensitivity_culture * ast_data
  men_symptom_sexhealth <- inf_men * men_symptom * msym_healthaccess * sexhealth * mscliniciantest_sexhealth * diagnostic_test * sensitivity_culture * ast_data
  men_symptom_tertiary <- inf_men * men_symptom * msym_healthaccess * tertiary * mscliniciantest_tertiary * diagnostic_test * sensitivity_culture * ast_data
  
  #Asymptomatic men
  
  men_asymptom_gp <- inf_men * men_asymptom * masym_healthaccess * gp * mascliniciantest_gp * diagnostic_test * sensitivity_culture * ast_data
  men_asymptom_community <-  inf_men * men_asymptom * masym_healthaccess * community * mascliniciantest_community * diagnostic_test * sensitivity_culture * ast_data
  men_asymptom_sexhealth <- inf_men * men_asymptom * masym_healthaccess * sexhealth * mascliniciantest_sexhealth * diagnostic_test * sensitivity_culture * ast_data
  men_asymptom_tertiary <- inf_men * men_asymptom * masym_healthaccess * tertiary * mascliniciantest_tertiary * diagnostic_test * sensitivity_culture * ast_data
  
  #-----------------------------------------------
  # Antibiotic resistance detection
  #-----------------------------------------------
  
  
  systemsens = women_symptom_gp + women_symptom_community + women_symptom_sexhealth +
    women_symptom_tertiary + women_asymptom_gp + women_asymptom_community +
    women_asymptom_sexhealth + women_asymptom_tertiary + men_symptom_gp +
    men_symptom_community + men_symptom_sexhealth + men_symptom_tertiary +
    men_asymptom_gp + men_asymptom_community + men_asymptom_sexhealth + men_asymptom_tertiary
  
  #-----------------------------------------------
  # Gonoccocal species detection
  #-----------------------------------------------
  
  #Symptomatic women
  
  gwomen_symptom_gp <- inf_women * women_symptom * wsym_healthaccess * gp * wscliniciantest_gp * diagnostic_test * sensitivity_culture 
  gwomen_symptom_community <- inf_women * women_symptom * wsym_healthaccess * community * wscliniciantest_community * diagnostic_test * sensitivity_culture 
  gwomen_symptom_sexhealth <- inf_women * women_symptom * wsym_healthaccess * sexhealth * wscliniciantest_sexhealth * diagnostic_test * sensitivity_culture 
  gwomen_symptom_tertiary <- inf_women * women_symptom * wsym_healthaccess * tertiary * wscliniciantest_tertiary * diagnostic_test * sensitivity_culture 
  
  #Asymptomatic women
  
  gwomen_asymptom_gp <- inf_women * women_asymptom * wasym_healthaccess * gp * wasclinicicantest_gp * diagnostic_test * sensitivity_culture 
  gwomen_asymptom_community <- inf_women * women_asymptom * wasym_healthaccess * community * wasclinicicantest_community * diagnostic_test * sensitivity_culture 
  gwomen_asymptom_sexhealth <- inf_women * women_asymptom * wasym_healthaccess * sexhealth * wascliniciantest_sexhealth * diagnostic_test * sensitivity_culture 
  gwomen_asymptom_tertiary <- inf_women * women_asymptom * wasym_healthaccess * tertiary * wascliniciantest_tertiary * diagnostic_test * sensitivity_culture 
  
  #Symptomatic men
  
  gmen_symptom_gp <- inf_men * men_symptom * msym_healthaccess * gp * mscliniciantest_gp * diagnostic_test * sensitivity_culture 
  gmen_symptom_community <- inf_men * men_symptom * msym_healthaccess * community * mscliniciantest_community * diagnostic_test * sensitivity_culture 
  gmen_symptom_sexhealth <- inf_men * men_symptom * msym_healthaccess * sexhealth * mscliniciantest_sexhealth * diagnostic_test * sensitivity_culture 
  gmen_symptom_tertiary <- inf_men * men_symptom * msym_healthaccess * tertiary * mscliniciantest_tertiary * diagnostic_test * sensitivity_culture 
  
  #Asymptomatic men
  
  gmen_asymptom_gp <- inf_men * men_asymptom * masym_healthaccess * gp * mascliniciantest_gp * diagnostic_test * sensitivity_culture 
  gmen_asymptom_community <-  inf_men * men_asymptom * masym_healthaccess * community * mascliniciantest_community * diagnostic_test * sensitivity_culture 
  gmen_asymptom_sexhealth <- inf_men * men_asymptom * masym_healthaccess * sexhealth * mascliniciantest_sexhealth * diagnostic_test * sensitivity_culture
  gmen_asymptom_tertiary <- inf_men * men_asymptom * masym_healthaccess * tertiary * mascliniciantest_tertiary * diagnostic_test * sensitivity_culture
  
  
  
  gonosens = (systemsens) / ((ast_data)*sensitivity_culture)
  
  results_gono[i, ] <- c(i,gonosens, gwomen_symptom_gp, gwomen_symptom_community, 
                         gwomen_symptom_sexhealth, gwomen_symptom_tertiary, gwomen_asymptom_gp, 
                         gwomen_asymptom_community, gwomen_asymptom_sexhealth, gwomen_asymptom_tertiary, 
                         gmen_symptom_gp, gmen_symptom_community, gmen_symptom_sexhealth, 
                         gmen_symptom_tertiary, gmen_asymptom_gp, gmen_asymptom_community, 
                         gmen_asymptom_sexhealth, gmen_asymptom_tertiary)

  # Binding the results
  results[i, ] <- c(i, systemsens, gonosens, women_symptom_gp, women_symptom_community, 
                    women_symptom_sexhealth, women_symptom_tertiary, women_asymptom_gp, 
                    women_asymptom_community, women_asymptom_sexhealth, women_asymptom_tertiary, 
                    men_symptom_gp, men_symptom_community, men_symptom_sexhealth, 
                    men_symptom_tertiary, men_asymptom_gp, men_asymptom_community, 
                    men_asymptom_sexhealth, men_asymptom_tertiary)
}
print(paste0("Simulation done with ", niter, " iterations"))
close(progress_bar)


# -----------------------------------------------
# Visualising results
#-----------------------------------------------



# -----------------------------------------------
# Splitting dataframe 
#------------------------------------------------

sensitivities = results %>% 
  select(gonosens,systemsens, iteration)

head(sensitivities)

# -----------------------------------------------
# Dual plotting
#------------------------------------------------

# -----------------------------------------------
# Reshaping data to dual plot
# library(ggstance)
#------------------------------------------------


#Melting the data

sens_melted = melt(sensitivities, id.vars = "iteration",
                   variable.name = "type",
                   value.name = "value")


#Final visualisation

p = sens_melted %>% 
  ggplot(aes(x = value, fill = type)) + 
  geom_density(alpha = 0.2, position = "identity") +
  theme_Publication() +
  labs(x = "Total system sensitivity (SSe)", y = "Proportion of simulated values") +
  scale_fill_manual(name = "Detection\ncomponent",
                    labels = c("Gonoccocal\ndetection", "Antibiotic\nresistance"),
                    values = c("#FCD116", "#D62728")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) +
  geom_boxploth(aes(x = value, y = -0.8, fill = type),
                width = 1) +
  stat_summaryh(aes(x = value, y = -0.8), geom = "point", shape = 21, size = 3) +
  stat_summaryh(aes(x = value, y = -0.8), geom = "errorbarh", width = 0.3, size = 1.5)

p

# -----------------------------------------------
# Reshaping data to dual plot
# Segment building with median displayed
#------------------------------------------------

sens_median = aggregate(data = sens_melted, value ~ type, median)
sens_mean = aggregate(data = sens_melted, value ~ type, mean)

p1 = p + geom_segment(
  data = sens_mean,
  aes(
    x = sens_mean$value[sens_mean$type == "systemsens"] ,
    xend = sens_mean$value[sens_mean$type == "systemsens"],
    y = 0,
    yend = 20
  ),
  linetype = "dashed",
  colour = "#B31B1B"
) + geom_segment(
  data = sens_mean,
  aes(
    x = sens_mean$value[sens_mean$type == "gonosens"],
    xend = sens_mean$value[sens_mean$type == "gonosens"],
    y = 0,
    yend = 20,
  ),
  linetype = "dashed",
  colour = "#E6B800"
) 


# -----------------------------------------------
# Reshaping data to dual plot
# Lines for median and confidence interval
#------------------------------------------------

p1 = p1 + geom_text(data = sens_mean, aes(
  x = sens_mean$value[sens_mean$type == "systemsens"],
  y = 21, fontface = "bold",
  label = paste0("Mean", "\n", round(sens_mean$value[sens_mean$type == "systemsens"], 4))
), size = 3.5) + geom_text(data = sens_mean, aes(
  x = sens_mean$value[sens_mean$type == "gonosens"],
  y = 21, fontface = "bold",
  label = paste0("Mean", "\n", round(sens_mean$value[sens_mean$type == "gonosens"], 4))
), size = 3.5) 

p1

# -----------------------------------------------
# Reshaping data to dual plot
# Lines for median and confidence interval
#------------------------------------------------

sens_pctl <- sens_melted %>% 
  group_by(type) %>% 
  summarise(pctl_up = quantile(value, 0.975),
            pctl_lower = quantile(value, 0.025))


p1 + 
  geom_segment(data = sens_pctl, aes(x = sens_pctl$pctl_up[sens_pctl$type == "systemsens"],
                                        xend = sens_pctl$pctl_up[sens_pctl$type == "systemsens"],
                                        y = 0,
                                        yend = 20),
               linetype = "dashed", colour = "#B31B1B") +
  geom_segment(data = sens_pctl, aes(x = sens_pctl$pctl_lower[sens_pctl$type == "systemsens"],
                                     xend = sens_pctl$pctl_lower[sens_pctl$type == "systemsens"],
                                     y = 0,
                                     yend = 20),
               linetype = "dashed", colour = "#B31B1B") +
  geom_segment(data = sens_pctl, aes(x = sens_pctl$pctl_lower[sens_pctl$type == "gonosens"],
                                     xend = sens_pctl$pctl_lower[sens_pctl$type == "gonosens"],
                                     y = 0,
                                     yend = 20),
               linetype = "dashed", colour = "#E6B800") +
  geom_segment(data = sens_pctl, aes(x = sens_pctl$pctl_up[sens_pctl$type == "gonosens"],
                                     xend = sens_pctl$pctl_up[sens_pctl$type == "gonosens"],
                                     y = 0,
                                     yend = 20),
               linetype = "dashed", colour = "#E6B800") +
  geom_text(data = sens_pctl, aes(
    x = sens_pctl$pctl_up[sens_pctl$type == "gonosens"] - 0.025,
    y = 7.5, fontface = "bold",
    label = paste0("95% CI", "\n", round(sens_pctl$pctl_up[sens_pctl$type == "gonosens"], 4))
  ), size = 3.5) +
  geom_text(data = sens_pctl, aes(
    x = sens_pctl$pctl_lower[sens_pctl$type == "gonosens"] + 0.025,
    y = 7.5, fontface = "bold",
    label = paste0("5% CI", "\n", round(sens_pctl$pctl_lower[sens_pctl$type == "gonosens"], 4))
  ), size = 3.5) +
  geom_text(data = sens_pctl, aes(
    x = sens_pctl$pctl_up[sens_pctl$type == "systemsens"] + 0.025,
    y = 9, fontface = "bold",
    label = paste0("95% CI", "\n", round(sens_pctl$pctl_up[sens_pctl$type == "systemsens"], 4))
  ), size = 3.5) +
  geom_text(data = sens_pctl, aes(
    x = sens_pctl$pctl_lower[sens_pctl$type == "systemsens"] - 0.03,
    y = 9, fontface = "bold",
    label = paste0("5% CI", "\n", round(sens_pctl$pctl_lower[sens_pctl$type == "systemsens"], 4))
  ), size = 3.5) +
  labs(caption = expression(paste(italic("n"), " = 100,000 iterations")))
  

# -----------------------------------------------
# System component senstivity
# Subsetting and redefining dataset
#------------------------------------------------

component_sens = results %>% 
  select(-gonosens, -systemsens, -iteration)

#Gathering into long
component_sens = component_sens %>% 
  gather(key = "variable", value = "value")

# -----------------------------------------------
# Component sensitivities
#------------------------------------------------
gono_component_sens = results_gono %>% 
  select(-iteration, -totalgonosens)

gono_component_sens = gono_component_sens %>% 
  gather(key = "variable", value = "value")

# -----------------------------------------------
# Manipulating dataset
#------------------------------------------------

#Converting to a long format
component_sens = component_sens %>% 
  separate(variable, into = c("sex", "symptom", sep = "_")) #Seperating antibiotic resistance

gono_component_sens = gono_component_sens %>% 
  separate(variable, into = c("sex", "symptom", sep = "_")) #Separating gono sens

names(component_sens)[3] = "location"
names(gono_component_sens)[3] = "location"
component_sens$sex = ifelse(component_sens$sex == "en", "men", component_sens$sex)
gono_component_sens$sex = ifelse(gono_component_sens$sex == "gwomen", 
                             "women", gono_component_sens$sex)

gono_component_sens$sex = ifelse(gono_component_sens$sex == "gmen",
                                 "men", gono_component_sens$sex)
unique(component_sens$sex)
unique(gono_component_sens$sex)

#-----------------------------------------------
# Flextable for antibiotic resistance
# Summary statistics
#------------------------------------------------

alpha = 0.05

component_table = component_sens %>% 
  group_by(sex, symptom, location) %>% 
  summarise(count = n(),
            mean= mean(value),
            lower95 = mean(value) - qt(1- alpha/2, (n() - 1))*sd(value)/sqrt(n()),
            upper95 = mean(value) + qt(1- alpha/2, (n() - 1))*sd(value/sqrt(n()))) %>% 
  flextable()

save_as_docx(component_table, 
             path = "C:\\Users\\uqpdo\\Documents\\Manuscripts\\Scenario Tree Model\\component_table.docx") 

results %>% 
  select(systemsens)
  mutate(min = min(systemsens),
         max = max(systemsens), 
         median = median(systemsens), 
         mean = mean(systemsems),
         lower95 = mean(systemsens) - qt(1- alpha/2, (n() - 1))*sd(systemsens)/sqrt(n()),
         upper95 = mean(systemsens) + qt(1- alpha/2, (n() - 1))*sd(systemsens)/sqrt(n())) %>% 
    View()


quantile(results$systemsens, c(0.025,0.5, 0.975))

gono_component_table = gono_component_sens %>% 
  group_by(sex, symptom, location) %>% 
  summarise(mean = mean(value),
            lower95 = mean(value) - qt(1- alpha/2, (n() - 1))*sd(value)/sqrt(n()),
            upper95 = mean(value) + qt(1- alpha/2, (n() - 1))*sd(value/sqrt(n()))) %>% 
  flextable()

gono_component_table

save_as_docx(gono_component_table, 
             path = "C:\\Users\\uqpdo\\Documents\\Manuscripts\\Scenario Tree Model\\gono_component_table.docx") 

# -----------------------------------------------
# Graphing results
#------------------------------------------------

#Making a colour palette
palette =  c("#F44336", "#FFC107", "#4CAF50", "#2196F3")
palette2 <- c("#FF8A80", "#FFD180", "#B9F6CA", "#81D4FA")



#Faceted boxplot for antibiotic resistance
p_comp = component_sens %>% 
  ggplot(aes(x = location, y = log(value), fill = location)) +
  geom_boxplot() +
  facet_grid(cols = vars(sex), rows = vars(symptom),
             labeller = labeller(sex = c(women = "Female", men = "Male"),
                                 symptom = c(asymptom = "Asymptomatic", symptom = "Symptomatic"))) + 
  theme_bw() + 
  coord_flip() +
  labs(x = "", 
       caption = "Antibiotic resistance status",
       y = expression(paste("Component sensitivity [",log[10]("CSe"),"]"))) +
  scale_fill_manual(name = "Surveillance system\ncomponent",
                      labels = c("Community\nhealth", "General\npractice", 
                                 "Sexual\nhealth clinic", "Tertiary\ncare"),
                      values = palette) +
  theme(axis.ticks.y= element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "HelveticaNeueLT Pro 55 Roman"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) 
p_comp

#Faceted boxplot for gonococcal resistance

p_gono = gono_component_sens %>% 
  ggplot(aes(x = location, y = log(value), fill = location)) +
  geom_boxplot() +
  facet_grid(cols = vars(sex), rows = vars(symptom),
             labeller = labeller(sex = c(women = "Female", men = "Male"),
                                 symptom = c(asymptom = "Asymptomatic", symptom = "Symptomatic"))) + 
  theme_bw() + 
  coord_flip() +
  labs(x = "", 
       caption = "Gonococcal detection",
       y = expression(paste("Component sensitivity [",log[10]("CSe"),"]"))) +
  scale_fill_manual(values = palette,
                    labels = c("Community\nhealth","General\npractice",
                               "Sexual\nhealth clinic", "Tertiary\ncare"),
                    name = "Surveillance system\ncomponent") +
  theme(axis.ticks.y= element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "HelveticaNeueLT Pro 55 Roman"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

p_gono


ggarrange(p_comp, p_gono, common.legend = TRUE, legend = "bottom",
          labels = "AUTO") 
#-----------------------------------------------
# Sensitivity Analysis
# Maximising individual values with each step
#-----------------------------------------------
#-----------------------------------------------
# Inserting in new dataset for sensitivity
# analysis
# Imported data from paramod
#-----------------------------------------------

parameter <- c("AST data", "Health Service Access (Symptomatic)", "Health Service Access (Asymptomatic)", 
               "Health Service Access (Inclusive)", "Clinician Testing (Symptomatic)", "Clinician Testing (Asymptomatic)", 
               "Clinician Testing (Inclusive)", "AST + Health Service Access (Symptomatic)", "AST + Health Service Access (Asymptomatic)", 
               "AST + Clinician Test (Symptomatic)", "AST + Clinician Test (Asymptomatic)", "AST + Clinician Test (Inclusive)", 
               "Health Service (Symptomatic) + Clinician Testing (Symptomatic)", "Health Service (Asymptomatic) + Clinician Testing (Asymptomatic)", 
               "Health Service (Symptomatic) + Clinician Test (Asymptomatic)", "Health Service (Asymptomatic) + Clinician Testing (Symptomatic)", 
               "Health Service (Inclusive) + Clinician Test (Inclusive)", "AST + Health Service (Inclusive) + Clinician Testing (Inclusive)")

mod_0.1 <- c(0.05, 0.02, 0.0133, 0.015, 0.02, 0.135, 0.018, 0.006, 0.053, 0.008, 0.054, 0.007, 0.0076, 0.133, 0.018, 0.015, 0.002, 0.0008)

mod_0.5 <- c(0.277, 0.079, 0.136, 0.07, 0.07, 0.15, 0.089, 0.157, 0.274, 0.15, 0.3, 0.18, 0.04, 0.149, 0.09, 0.07, 0.05, 0.011)

mod_0.99 <- c(0.549, 0.152, 0.14, 0.156, 0.146, 0.17, 0.17, 0.6, 0.56, 0.57, 0.68, 0.7, 0.158, 0.19, 0.186, 0.14, 0.196, 0.85)

variable <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3)

paramod <- data.frame(parameter, mod_0.1, mod_0.5, mod_0.99, variable)


new_data <- paramod %>%
  pivot_longer(cols = -c(parameter, variable), names_to = "Mod", 
               values_to = "Value")

new_data$Variable = as.factor(new_data$Variable)
new_data$Mod = as.factor(new_data$Mod)




                             
bright_palette <- c("#00AFBB", "#F8766D", "#00BA38", "#619CFF", "#F564E3", "#5D3A9B", 
                             "#00BFC4", "#F26D21", "#6A3D9A", "#87CEEB", "#FF7F00", "#FFC200", 
                             "#A0522D", "#00FF7F", "#FFD700", "#808080", "#008080", "#800000")
                             

#Sensitivity analysis plot

new_data %>% 
  ggplot(aes(x = Mod, y = Value, fill = parameter, label = round(Value, 2))) +
  geom_bar(stat = "identity", colour = "black", position = "dodge") +
  geom_text(aes(label = round(Value, 2), y = Value), 
            position = position_dodge(width = 1),
            vjust = -1,
            hjust = 0) +
  facet_wrap(~ variable, nrow = 3,
             labeller = labeller(variable = c(`1` = "One variable modified", 
                                              `2` = "Two variables modified", 
                                              `3` ="Three variables modified"))) + 
  theme_Publicationv() + #Custom theme
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"), strip.background = element_rect(fill = "grey"),
        plot.caption = element_text(hjust = 1, margin = margin(t = 10, r = 10))) + 
  scale_fill_manual(values = bright_palette) +
  labs(x = "Modified input values",
       y = "Total system sensitivity (SSe)",
       fill = "Parameters\npermutations",
       caption = "Manual calculation of total system sensitivity\nwith parameter inputs of 0.1, 0.5, and 0.99") +
  scale_x_discrete(expand = c(0,0), labels = c("0.1", "0.5", "0.99")) +
  scale_y_continuous(expand = c(0,0), limit = c(0,1.1))



new_data %>% 
  ggplot(aes(x = Mod, y = Value, fill = parameter, label = round(Value, 2))) +
  geom_bar(stat = "identity", colour = "black", position = "dodge") +
  geom_text_repel(aes(label = round(Value,2), y = Value), 
                  position = position_dodge(width = 1),
                  vjust = -1) +
  facet_wrap(~ variable, nrow = 3,
             labeller = labeller(variable = c(`1` = "One variable modified", 
                                              `2` = "Two variables modified", 
                                              `3` ="Three variables modified"))) + 
  theme_Publicationv() + #Custom theme
  theme(legend.position = "right",
        text = element_text(family = "HelveticaNeueLT Pro 55 Roman")
        strip.text = element_text(size = 12, face = "bold"), strip.background = element_rect(fill = "grey"),
        plot.caption = element_text(hjust = 1, margin = margin(t = 10, r = 10))) + 
  scale_fill_manual(values = bright_palette) +
  labs(x = "Modified input values",
       y = "Total system sensitivity (SSe)",
       fill = "Parameters\npermutations",
       caption = "Manual calculation of total system sensitivity\nwith parameter inputs of 0.1, 0.5, and 0.99") +
  scale_x_discrete(expand = c(0,0), labels = c("0.1", "0.5", "0.99")) +
  scale_y_continuous(expand = c(0,0), limit = c(0,1.1))




