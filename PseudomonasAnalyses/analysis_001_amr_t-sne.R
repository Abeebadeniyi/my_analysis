# load libraries
library("tidyverse")
install.packages("readxl")
library(readxl)

# import data
data <- read_excel("dataset/amr_genotypic_and_phenotypic_results.xlsx")
print(data)
names(data)
# extract the name and and ST columns
ST <- data$ST
name <- data$name
phylogroup <- data$phylogroup
print(ST)
print(name)
print(phylogroup)

# write a function to change yes and no obervation to 1 and 0 respectively
convert_yes_no_to_01 <- function(column) {
  return(ifelse(column == "yes", 1, 0))
}


# Apply the conversion function iteratively to all columns
for (col in names(data)) {
  data[[col]] <- convert_yes_no_to_01(data[[col]])
}


# Print the transformed data frame
print(data)

data$name <- name
data$ST <- ST
data$phylogroup <- phylogroup

print(data)

#============================================================================

# PCA analysis:-----------------------------------------------------------------


#packages:
install.packages("corrr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library(factoextra)
#check missing values---------------------------------------------------------
colSums(is.na(data))
#normalise data-----------------------------------
numerical_data <- data[,4:51]
print(numerical_data)


#data_normalized <- scale(numerical_data)
#head(data_normalized)

#NB:this doesnt work for me. so I am writing my own function

normalize_data <- function(data, method = "min-max", epsilon = 1e-8) {
  # Check if method is valid
  if (method != "min-max" && method != "standardize") {
    stop("Invalid normalization method. Use 'min-max' or 'standardize'.")
  }
  
  if (method == "min-max") {
    min_values <- apply(data, 2, min)
    max_values <- apply(data, 2, max)
    
    # Add epsilon to denominator to avoid division by zero
    normalized_data <- (data - min_values) / (max_values - min_values + epsilon)
  } else if (method == "standardize") {
    # Standardize the data (mean centering)
    normalized_data <- scale(data)
  }
  
  return(normalized_data)
}

# Normalize the data using the function
data_normalized <- normalize_data(numerical_data, method = "min-max")

# Print the normalized data
print(data_normalized)

#------------------------------------------------------------------------

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
# applying pca --------------------------------------------------------
data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]

#visualising the componenet---------------------------------------------

fviz_eig(data.pca, addlabels = TRUE)


# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

fviz_cos2(data.pca, choice = "var", axes = 1:2)
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
# running t-sne===========================================================
install.packages("Rtsne")
library(Rtsne)
?Rtsne
# Run t-SNE

tsne_result <- as.matrix(numerical_data) # You can specify the number of dimensions (e.g., dims = 2)
tsne_result <- Rtsne(tsne_result, dims = 2, perplexity = 5, check_duplicates = F,
                     max_iter = 5000)
# The result will contain the t-SNE coordinates in tsne_result$Y
tsne_coordinates <- tsne_result$Y
library(ggplot2)

# Create a scatter plot of the t-SNE results
tsne_dataFrame <- data.frame(X = tsne_coordinates[,1], Y = tsne_coordinates[,2])
is.data.frame(tsne_dataFrame)
tsne_dataFrame$ST <- ST
tsne_dataFrame$phylogroup <- phylogroup
#=========================================================================
#t-sne for phylogroup:
ggplot(tsne_dataFrame, aes(X, Y, color = phylogroup)) +
  geom_point() +  
  geom_text(aes(label = ST),hjust = 0, vjust = 0, nudge_x = 0.03, nudge_y = 0.03) +
  scale_color_manual(values = my_colors)+
  theme_grey()+
  ggtitle("AMR t-sne coloured by phylogroup, with perplexity = 5")+
  theme(plot.title = element_text(hjust = 0.5))


