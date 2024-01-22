# load libraries
library("tidyverse")
library(readxl)

# import data
data <- read_excel("dataset/ariba_vfdb_core_summary.xlsx", sheet = 2)
print(data)
names(data)
# extract the name and and ST columns
ST <- data$ST
name <- data$name
phylogroup <- data$phylogroup
print(ST)
print(name)
print(phylogroup)

data %>% count(ST,phylogroup) %>%  mutate(y = n) %>% 
  ggplot(aes(ST, y, fill = ST), colour = "black")+ geom_col()+
  geom_text(aes(label = y, vjust = -0.5))+
  scale_fill_manual(values = my_colors)+ 
  theme_bw()+
  facet_wrap(~phylogroup)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(labels = abbreviate)

#let us set our coolour palette================================================
colours()[1:500]

my_colors <- c("chocolate",  "cadetblue","black", "gold","chocolate3",
               "wheat3","olivedrab4", "khaki3",  "goldenrod1" , "deepskyblue",
               "turquoise" , "green1", "magenta","khaki4", "darkred","azure4",
               "grey12","sienna1" , "peachpuff4"  , "tomato1" , "firebrick1",
               "violetred4" ,  "lawngreen", "blue4",   "mediumspringgreen",
               "darkcyan" ,  "chartreuse" , "gainsboro", "gray42"  )



#=============================================================================
#goodness of fit test
probabilty <- 1 / 29

observed_and_expected_data <- phylo_summary %>% mutate(expected =  probabilty)

test <- chisq.test(observed_and_expected_data$count, # observed frequencies
                   p = observed_and_expected_data$expected # expected proportions
)

test
#========================================================================

st_fact <- factor(x = data$ST, 
                  levels = c("1117","12", "152", "1521", 
                                          "1748", "1756", "2068" , "2132*",
                             "217", "241", "277", "389", "554","641*",
                                          "782*", "983", "Novel", "Novel*" ,
                             "244", "274", "1743","455", "235","357",
                                          "823","308", "773", "1769", "3043"))

 data.frame(ST = data$ST,
             phylogroup = data$phylogroup,
             st_factor = st_fact) %>% count(st_factor,phylogroup) %>% 
  
  ggplot(aes(x = st_factor, y = n , fill = phylogroup))+
  geom_col(position = position_dodge(), colour = "black")+
  geom_text(aes(label = n, vjust = -0.5))+
  scale_fill_manual(values = my_colors)+ 
   theme_classic()+
   ggtitle("Frequency Count of P. aeruginosa (N = 55 isolates) per ST
           p-value = 0.00000001356")+
   scale_y_continuous("FREQUENCY")+
   scale_x_discrete("SEQUENCE TYPE (ST)")+
   theme(axis.text.x = element_text(angle = 90, hjust = 1),
         plot.title = element_text(hjust = 0.5, vjust = -10))

   


