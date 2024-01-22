library("tidyverse")
library(readxl)

# import data__________________________________________________________________
data <- read_excel("/Users/Khabib/Downloads/pseudomonas_Reports/amr_genotypic_and_phenotypic_results.xlsx")
print(data)
names(data)
str(data)
phylo_count <- data %>% group_by(phylogroup,location) %>%
             summarise(count = n()) %>% 
            mutate(percentage = count /sum(count)*100)
phylo_count$percentage <- round(phylo_count$percentage, 0)


#===========================================================================

phylo_count %>% ggplot(aes(x = phylogroup, y = percentage))+
                geom_col(aes(fill = phylogroup), colour = "black", 
                         position = position_dodge())+
                 geom_text(aes(label = percentage,vjust = 1.5))+
                 facet_wrap(~location)+
                theme_classic()

#phylogroup B proportion by location============================================
phylogroupB_data <- data %>% filter(phylogroup == "B") %>% 
  group_by(location, ST, phylogroup) %>% 
  summarise(count = n()) %>%  mutate(percentage = count/24 * 100)

phylogroupB_data$percentage <- round(phylogroupB_data$percentage , 0)
#===========================================================================
phylogroupB_data %>% ggplot(aes(x = ST, y = percentage))+
  geom_col(aes(fill = phylogroup), colour = "black", 
           position = position_dodge())+
  scale_fill_manual(values = "green")+
  geom_text(aes(label = percentage,vjust = 1.5))+
  theme_gray()+
  facet_wrap(~location)+
  ggtitle("phylogroupB (n=24/55) distribution by location")+
  theme(plot.title = element_text(hjust = 0.5))

#phylogroup A proportion by location=========================================

phylogroupA_data <- data %>% filter(phylogroup == "A") %>% 
  group_by(location, ST, phylogroup) %>% 
  summarise(count = n()) %>%
  mutate(percentage = count/sum(phylogroupA_data$count) * 100)


phylogroupA_data$percentage <- round(phylogroupA_data$percentage , 0)
#===========================================================================
phylogroupA_data %>% ggplot(aes(x = ST, y = percentage))+
  geom_col(aes(fill = phylogroup), colour = "black", 
           position = position_dodge())+
  scale_fill_manual(values = "khaki1")+
  geom_text(aes(label = percentage,vjust = 1.5))+
  theme_bw()+
  facet_wrap(~location)+
  ggtitle("phylogroupA (n=29/55) percentage distribution by location")+
  theme(plot.title = element_text(hjust = 0.5), #this code poistion my title
                                                  #at the centre
        axis.text.x = element_text(angle = 90, hjust = 1))#this code slant my
         # x-axis text. 

#theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ST773 distribution across location========================================

st773_data <- data %>% filter(ST == "773") %>% 
         group_by(location, phylogroup) %>% 
         summarise(count = n()) %>%  mutate(percentage = count/14 * 100)

st773_data$percentage <- round(st773_data$percentage, 0)

# plotting st773 data ---------------------
st773_data %>% ggplot(aes(x = location, y = percentage))+
  geom_col(aes(fill = phylogroup), colour = "black", 
           position = position_dodge())+
  geom_text(aes(label = percentage,vjust = 1.5))+
  theme_classic()+
  ggtitle("ST773(n=14/55) distribution by location")+
  theme(plot.title = element_text(hjust = 0.5))




#---------------------------------------------------------------------------

#data %>%
#  ggplot(aes(x = phylogroup, fill = location)) +
#  geom_histogram(stat = "count", color = "black") +
##  geom_text(stat = "count", aes(label = after_stat(count), vjust = -5))+
#  theme_classic()


