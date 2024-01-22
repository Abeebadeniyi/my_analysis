library(tidyverse)
# import data
data <- read_excel("dataset/ariba_vfdb_core_summary.xlsx", sheet = 2)
print(data)
names(data)
# using pivot_longer--------------------------------------------
data_long <- data %>%
  pivot_longer(cols = 4:331,
               names_to = "virulence_gene",
               values_to = "value")

#================================================
data_filter <- data_long %>% filter(value == "yes")
print(data_filter)


data_summary <- data_filter %>% group_by(ST,phylogroup) %>% 
  summarise(count = n()) 


# but the frequency of STs differ(e.g st773 = 14)), so we need to fix this by 
# dividing by the frequency of each STs

#1.

frequency_data <- data %>% group_by(ST) %>% summarise(n = n())
#2. 

data_summary$average_count <- data_summary$count / frequency_data$n 

#3. 

data_summary$average_count <- round(data_summary$average_count, 0)



#==============================================================================

correlatn_data <- tibble(VirulenceGeneCount = data_summary$average_count,
                             frequency_count = frequency_data$n,
                             ST = frequency_data$ST,
                             phylogroup = data_summary$phylogroup,
                             r = cor(VirulenceGeneCount, frequency_count))

#let us plot a correlation graph======================================
correlatn_data %>% 
    ggplot() +
   geom_point(aes(VirulenceGeneCount, frequency_count, color = ST)) +
   geom_text(aes(x = 200, y = 90, label = paste("r = ", r)), col = "blue") +
   theme_classic()

#let us set our coolour palette================================================
colours()[1:500]

my_colors <- c("chocolate",  "cadetblue","black", "gold","chocolate3",
               "wheat3","olivedrab4", "khaki3",  "goldenrod1" , "deepskyblue",
               "turquoise" , "green1", "magenta","khaki4", "darkred","azure4",
               "grey12","sienna1" , "peachpuff4"  , "tomato1" , "firebrick1",
               "violetred4" ,  "lawngreen", "blue4",   "mediumspringgreen",
               "darkcyan" ,  "chartreuse" , "gainsboro", "gray42"  )


# let us replot our graph again===============================================
correlatn_data %>% 
  ggplot() +
  geom_point(aes(VirulenceGeneCount, frequency_count, color = ST)) +
  geom_text(aes(x = 200, y = 70, label = paste("r = ", r)), col = "blue") +
  scale_color_manual(values = my_colors)+
  theme_classic()+
  ggtitle("Number of virulence genes Vs number of STs") +
  theme(plot.title = element_text(hjust = 0.5))












