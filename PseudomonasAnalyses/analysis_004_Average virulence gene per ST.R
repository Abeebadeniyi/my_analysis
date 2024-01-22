# load libraries
library("tidyverse")
library(readxl)

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
#4. arrange iin descending order.

data_summary <- data_summary %>% arrange(desc(average_count))
#=============================================================================
data_summary <- data_summary %>% 
  mutate(ST_f = factor( x = ST,
                        levels= c("1521", "2068", "277","12","554",
                        "983","244","Novel" ,"641*"  , "152",    "1743" ,
                        "389"   , "241"   , "455" ,   "1117"  , "2132*" ,"1748", 
                        "308"   , "782*" ,  "823" ,   "1756" ,  "217"   , "235",
                       "274" ,"1769"  , "Novel*", "357" ,   "773"   , "3043")))

data_summary %>% ggplot(aes(x = ST_f, y = average_count))+
  geom_col(fill = "blue", color = "black")+
  geom_text(aes(label = average_count), vjust = -0.5)+
  theme_classic()+
  ggtitle("Average virulence gene per ST")+
  theme(plot.title = element_text(hjust = 0.5))

# colouring it by phylogroup;------------------------------------------
data_summary %>% ggplot(aes(x = ST_f, y = average_count, fill = phylogroup))+
  geom_col(color = "black")+
  geom_text(aes(label = average_count), vjust = -0.5)+
  theme_classic()+
  ggtitle("Average virulence gene per ST")+
  theme(plot.title = element_text(hjust = 0.5))
