library(ggstatsplot)
library(car)
library(tidyverse)
library(readxl)

# import data
data <- read_excel("dataset/ariba_vfdb_core_summary.xlsx", sheet = 2)
print(data)
names(data)


#let us set our coolour palette================================================
colours()[1:500]

my_colors <- c("chocolate",  "cadetblue","black", "gold","chocolate3",
               "wheat3","olivedrab4", "khaki3",  "goldenrod1" , "deepskyblue",
               "turquoise" , "green1", "magenta","khaki4", "darkred","azure4",
               "grey12","sienna1" , "peachpuff4"  , "tomato1" , "firebrick1",
               "violetred4" ,  "lawngreen", "blue4",   "mediumspringgreen",
               "darkcyan" ,  "chartreuse" , "gainsboro", "gray42"  )


# subset by phylogroup=======================================================
phylo_summary <- data %>% group_by(phylogroup, ST) %>%
  summarise(count = n()) 

#================================================
anova_result <- aov(count ~ phylogroup, data = phylo_summary)
print(summary(anova_result))

# checking normality of my data using anova result visually =================
par(mar =c(5, 4, 4, 2))
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(anova_result$residuals)

# QQ-plot
qqPlot(anova_result$residuals,
       id = FALSE # id = FALSE to remove point identification
)
# checking normality of my data using anova result statistically===============

shapiro.test(anova_result$residuals)


# since my dataset is not parametric: i need to use a non_paramteric kruskal-
#wallis test:

kruskal_wallis_result <- kruskal.test(count ~ phylogroup, data = phylo_summary)
summary(kruskal_wallis_result)

#let us visualise this as bar chart:====================================

ggbetweenstats(
  data = phylo_summary,
  x = phylogroup,
  y = count,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)+ theme_bw()
  
#- one-proportion test using ggpiestats-------------------------------------
phylo_summary_2 <- phylo_summary %>% filter(phylogroup == "A" | phylogroup =="B")
ggpiestats(
  data = phylo_summary_2,
  x = phylogroup,
  bf.message = FALSE
) +
labs(title = "One proportion test of phylogroup A and B") +
theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
#-=======================================================================
#goodness of fit test
probabilty <- 1 / 29

observed_and_expected_data <- phylo_summary %>% mutate(expected =  probabilty)

test <- chisq.test(observed_and_expected_data$count, # observed frequencies
                  p = observed_and_expected_data$expected # expected proportions
)

test

#===chi square for phylogroup A and B only====================================
probab <- 1/2
phylo_Aand_B_count <-  data %>% filter(phylogroup == "A" | phylogroup == "B") %>%
  count(phylogroup) %>% mutate(expected = probab) 

chi_test <- chisq.test(phylo_Aand_B_count$n, 
                       p = phylo_Aand_B_count$expected)
print(chi_test)

phylo_Aand_B_count %>% ggplot(aes(x = phylogroup, y = n, fill = phylogroup))+
  geom_col(colour = "black")+
  scale_fill_manual(values = my_colors)+ 
  theme_classic()+
  geom_text(aes(label = n, vjust = -0.5))+
  ggtitle("test of significance: p-calcualted = 0.4922")+
  scale_y_continuous("FREQUENCY")+
  scale_x_discrete("phylogroup")+
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.8, vjust = -2))





#===chi square for phylogroup A and B and others====================================
probab <- 1/3
phylo_Aand_B_count <-  data %>%
  count(phylogroup) %>% mutate(expected = probab)

chi_test <- chisq.test(phylo_Aand_B_count$n, 
                       p = phylo_Aand_B_count$expected)
print(chi_test)


phylo_Aand_B_count %>% ggplot(aes(x = phylogroup, y = n, fill = phylogroup))+
  geom_col(colour = "black")+
  scale_fill_manual(values = my_colors)+ 
  theme_classic()+
  geom_text(aes(label = n, vjust = -0.5))+
  ggtitle("test of significance: p-value = 0.000012951")+
  scale_y_continuous("FREQUENCY")+
  scale_x_discrete("phylogroup")+
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.8, vjust = -2))







