install.packages("UpSetR")
install.packages("readxl")
library(tidyverse)
library(ggthemes)
library(UpSetR)
library(readxl)



data <- read_excel("niyi_dataset.xlsx", sheet = 3)
data
glimpse(data)




input <- c(C.parvum = 23,
           C.belli = 12,
           C.cayetanensis = 23,
           E.histolytica = 1,
           S.stercoralis = 1,
           Entamoeba.coli = 0,
           "A.lumbricoides&T.trichiura" = 1,
           "E.histolytica&Entamoeba.coli&C.cayetanensis" = 1,
           "C.parvum&C.cayetanensis" = 13,
           "C.parvum&C.belli" = 6,
           "C.belli&C.cayetanensis" = 4,
           "C.parvum&C.belli&C.cayetanensis" = 5)

upset(fromExpression(input), 
      nintersects = 11, 
      nsets = 6, 
      order.by = "freq", 
      decreasing = T,
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 1.1, 
      point.size = 2.8, 
      line.size = 1
)


#===========================================================================


input2 <- c(C.parvum = 7,
            C.belli = 3,
            C.cayetanensis = 1,
            E.histolytica = 0,
            S.stercoralis = 0,
            Entamoeba.coli = 0,
            "A.lumbricoides&T.trichiura" = 0,
            "E.histolytica&Entamoeba.coli&C.cayetanensis" = 0,
            "C.parvum&C.cayetanensis" = 2,
            "C.parvum&C.belli" = 3,
            "C.belli&C.cayetanensis" = 1,
            "C.parvum&C.belli&C.cayetanensis" = 0)


upset(fromExpression(input2), 
      nintersects = 11, 
      nsets = 6, 
      order.by = "freq", 
      decreasing = T,
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 1.1, 
      point.size = 2.8, 
      line.size = 1)
