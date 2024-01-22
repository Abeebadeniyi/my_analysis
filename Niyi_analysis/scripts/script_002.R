# load library
library(tidyverse)
library(ggthemes)
library(UpSetR)
library(readxl)
library(gridExtra)
# import data==================================================================
getwd()
data <- read_excel("Niyi_analysis/data/Niyi_tidy_data.xlsx", sheet = 1)

# view data====================================================================

data
glimpse(data)

# data visualisation============================================================

data <- data %>% mutate(month_ordered = reorder(month, month_numeric))
glimpse(data)

data %>% ggplot(aes(x = month_ordered, y = frequency)) +
  geom_point() +
  facet_grid(subject~group) +
  theme(axis.text.x = element_text(angle = 90))


#statistical analysis: Corr plot==============================================

data %>% mutate(r = cor(frequency, month_numeric)) %>% 
  ggplot(aes(x = month_ordered, y = frequency)) +
  geom_point() +
  facet_grid(subject~group) +
  geom_text(aes(x = 5, y = 20, label= paste("r = ", r)), col = "blue")+
  theme(axis.text.x = element_text(angle = 90))
#===============================================================================

corr_data <- data %>% 
  group_by(subject,group) %>% 
  mutate(r = cor(frequency, month_numeric)) %>% 
  ungroup() 

#==============================================================================
options(digits = 4)
corr_data %>% 
  ggplot(aes(x = month_ordered, y = frequency)) +
  geom_point() +
  facet_grid(subject~group) +
  geom_text(aes(x = 5, y = 20, label= paste("r = ", round(r, 4))), col = "red")+
  xlab("Month")+
  ylab("Number of subject recruited")+
  ggtitle("Relationship between month and subject recruitment") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust=0.5))
#==============================================================================

#========================Upsett Plot==============================================
#1.

parasite_detection_case <- read_excel("Niyi_analysis/data/HD dataset for plots.xlsx", sheet = 3)

glimpse(parasite_detection_case)

parasite_detection_case

input_1 <- c(C.parvum = 8,
            C.belli = 5,
            C.cayetanensis = 4,
            A.lumbricoides = 0,
            T.trichiura = 0,
            "A.lumbricoides&T.trichiura" = 1,
            "C.parvum&C.cayetanensis" = 2,
            "C.parvum&C.belli" = 3,
            "C.belli&C.cayetanensis" = 1,
            "C.parvum&C.belli&C.cayetanensis" = 1)


upset(fromExpression(input_1), 
      nintersects = 10, 
      nsets = 5, 
      order.by = "freq", 
      decreasing = T,
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 2, 
      point.size = 5.0, 
      line.size = 1,
      mainbar.y.label = "Frequency",
      sets.x.label = "count")

#2. 

parasite_detection_control <- read_excel("Niyi_analysis/data/HD dataset for plots.xlsx", sheet = 4)

glimpse(parasite_detection_control)

parasite_detection_control

input_2<- c(C.parvum = 15,
            S.stercoralis = 1,
            E.histolytica = 1,
             C.belli = 7,
             C.cayetanensis = 19,
             A.lumbricoides = 0,
             T.trichiura = 1,
            Entamoeba.coli = 0,
             "A.lumbricoides&T.trichiura" = 1,
             "C.parvum&C.cayetanensis" = 11,
             "C.parvum&C.belli" = 3,
             "C.belli&C.cayetanensis" = 3,
            "E.histolytica&Entamoeba.coli&C.cayetanensis" = 1,
             "C.parvum&C.belli&C.cayetanensis" = 4)


upset(fromExpression(input_2), 
      nintersects = 14, 
      nsets = 8, 
      order.by = "freq", 
      decreasing = T,
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 2, 
      point.size = 5.0, 
      line.size = 1,
      mainbar.y.label = "Frequency",
      sets.x.label = "count")

#3.

DEC_case <- read_excel("Niyi_analysis/data/HD dataset for plots.xlsx", sheet = 5)

glimpse(DEC_case)

DEC_case

input_3<- c(EAEC= 21,
            EIEC = 3,
            EPEC = 2,
            ETEC = 6,
            "EIEC&ETEC" = 1,
            "EIEC&EAEC" = 1,
            "EPEC&ETEC" = 3,
            "EPEC&EIEC" = 1,
            "ETEC&EAEC" = 3,
            "EPEC&ETEC&EAEC" = 4,
            "EAEC&EIEC&ETEC" = 1,
            "EAEC&EIEC&EPEC&ETEC" = 4)


upset(fromExpression(input_3), 
      nintersects = 12, 
      nsets = 4, 
      order.by = "freq", 
      decreasing = T,
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 2, 
      point.size = 5.0, 
      line.size = 1,
      mainbar.y.label = "Frequency",
      sets.x.label = "count")

#4. 

DEC_control <- read_excel("Niyi_analysis/data/HD dataset for plots.xlsx", sheet = 6)

glimpse(DEC_control)

DEC_control

input_4<- c(EAEC= 21,
            EIEC = 8,
            EPEC = 3,
            ETEC = 12,
            "EAEC&EIEC" = 2,
            "EAEC&EPEC" = 7,
            "EAEC&ETEC" = 6,
            "EIEC&STEC" = 7,
            "EPEC&ETEC" = 3,
            "EAEC&EIEC&ETEC" = 6,
            "EAEC&EPEC&ETEC" = 8,
            "EPEC&EIEC&ETEC" = 1)


upset(fromExpression(input_4), 
      nintersects = 12, 
      nsets = 4, 
      order.by = "freq", 
      decreasing = T,
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 2, 
      point.size = 5.0, 
      line.size = 1,
      mainbar.y.label = "Frequency",
      sets.x.label = "count")

#5.
bact_para_coinfection_case <- read_excel("Niyi_analysis/data/HD dataset for plots.xlsx", sheet = 7)

glimpse(bact_para_coinfection_case)

bact_para_coinfection_case


input_5<- c(EAEC= 0,
            EIEC = 0,
            EPEC = 0,
            ETEC = 0,
            C.parvum = 0,
            C.belli = 0,
            C.cayetanensis = 0,
            "C.belli&EAEC" = 1,
            "C.belli&ETEC"= 1,
            "C.belli&C.parvum&ETEC" = 1,
            "C.belli&C.parvum&EPEC" = 1,
            "C.belli&C.parvum&EAEC" = 1,
            "C.parvum&ETEC&EPEC" = 3,
            "C.parvum&EPEC&EIEC" = 1,
            "C.parvum&ETEC&EAEC" = 1,
            "C.parvum&ETEC&EIEC" = 1,
            "C.cayetanesis&ETEC&EAEC" = 1,
            "C.belli&EAEC&EPEC&EIEC" = 1,
            "C.parvum&EAEC&EIEC&EPEC&ETEC" = 1,
            "C.parvum&C.cayetanensis&EAEC&EPEC&ETEC" = 1,
            "C.belli&C.cayetanensis&EAEC&EPEC&EIEC&ETEC" = 1)


upset(fromExpression(input_5), 
      nintersects = 21, 
      nsets = 7, 
      order.by = "freq", 
      decreasing = T,
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 2, 
      point.size = 5.0, 
      line.size = 1,
      mainbar.y.label = "Frequency",
      sets.x.label = "count")

# 6.
bact_para_coinfection_control <- read_excel("Niyi_analysis/data/HD dataset for plots.xlsx", sheet = 8)

glimpse(bact_para_coinfection_control)

head(bact_para_coinfection_control, 20)


input_6<- c(EAEC= 0,
            EIEC = 0,
            EPEC = 0,
            ETEC = 0,
            C.parvum = 0,
            C.belli = 0,
            C.cayetanensis = 0,
            E.histolytica = 0,
            Entamoeba.coli = 0,
            "C.parvum&EAEC" = 3,
            "C.parvum&EIEC" = 1,
            "C.parvum&ETEC" = 1,
            "C.cayetanensis&ETEC" = 6,
            "C.cayetanensis&EPEC" = 1,
            "C.belli&EAEC" = 1,
            "C.parvum&EIEC&ETEC" = 2,
            "C.parvum&EAEC&ETEC" = 3,
            "C.parvum&C.cayetanensis&EIEC" = 1,
            "C.parvum&C.cayetanensis&EAEC" = 1,
            "C.parvum&C.belli&EAEC" = 1,
            "C.belli&EAEC&EPEC" = 1,
            "C.belli&EIEC&ETEC" = 1,
            "C.belli&C.cayetanensis&EPEC" = 1,
            "C.belli&C.cayetanensis&EAEC" = 1,
            "C.cayetanensis&EAEC&EIEC"  = 1,
            "C.cayetanensis&EIEC&ETEC"  = 1,
             "C.cayetanensis&EAEC&EPEC" = 1,
             "E.histolytica&EAEC&ETEC"   = 1,
            "C.parvum&C.belli&EAEC&EPEC"  = 1 )


upset(fromExpression(input_6), 
      nintersects = 30, 
      nsets = 9, 
      order.by = "freq", 
      decreasing = T,
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 2, 
      point.size = 5.0, 
      line.size = 1,
      mainbar.y.label = "Frequency",
      sets.x.label = "count")


##---------------------------------------------------------------------------


