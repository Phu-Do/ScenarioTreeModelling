#-----------------------------------------------
# Sensitivity Analysis
# Maximising individual values with each step
#-----------------------------------------------

#-----------------------------------------------
# Inserting in new dataset for sensitivity
# analysis
# Imported from maximum (ModelRisk)
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

#-----------------------------------------------
# Data manipulation
#-----------------------------------------------
new_data <- paramod %>%
  pivot_longer(cols = -c(parameter, variable), names_to = "Mod", 
               values_to = "Value")

new_data$Variable = as.factor(new_data$Variable)
new_data$Mod = as.factor(new_data$Mod)



#-----------------------------------------------
# Adding colour
#-----------------------------------------------
                             
bright_palette <- c("#00AFBB", "#F8766D", "#00BA38", "#619CFF", "#F564E3", "#5D3A9B", 
                             "#00BFC4", "#F26D21", "#6A3D9A", "#87CEEB", "#FF7F00", "#FFC200", 
                             "#A0522D", "#00FF7F", "#FFD700", "#808080", "#008080", "#800000")
                             

#-----------------------------------------------
# Sensitivity plot creation
#-----------------------------------------------

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



