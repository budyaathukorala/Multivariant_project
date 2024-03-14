#AABB Athukorala
#D/DBA/21/0043

setwd("D:/KDU/Third Year/Second Semester/Multivariate Data Analysis/Assignment/dataset")
getwd()

install.packages("readxl")
library(readxl)

##DATA PREPROCESSING##

# Load the dataset
data <- read_excel("project_data.xlsx")
data

# Checking for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

#Summary and Structure of the dataset
summary(data)
str(data)

##Boxplots
# Create boxplots for all 11 components
par(mfrow=c(4, 3)) 

# Loop through each component column
components <- colnames(data)[-1]  # Exclude the 'Sample_No' column
for (col in components) {
  boxplot(data[[col]], main = paste("Boxplot for", col), ylab = col)
}

# Restore the default layout
par(mfrow=c(1, 1))

##Correlation matrix
# Extract the 11 chemical components
components <- data[, 2:12]  # Assumes columns 2 to 12 contain the components

# Calculate the correlation matrix
correlation_matrix <- cor(components)

# Print the correlation matrix
print(correlation_matrix)

# Create a heatmap of the correlation matrix
library(ggplot2)
library(reshape2)

# Convert the correlation matrix to long format for plotting
correlation_melted <- melt(correlation_matrix)

# Create a heatmap
ggplot(data = correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "green") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")

# Calculate mean, deviation (standard deviation), and variance for each component
summary_stats <- sapply(data[, -1], function(x) c(Mean = mean(x), Deviation = sd(x), Variance = var(x)))

# Print the summary statistics for each component
print(summary_stats)

##MULTIVARIATE ANALYSIS##

#objective 01

library(stats)
library(graphics)

# Perform Principal Component Analysis (PCA)
pca_result <- prcomp(data[, -1], scale = TRUE)
pca_result

# Summarize the PCA results
summary(pca_result)

#Objective 02

# Calculate the Euclidean distance matrix
euclidean_dist <- dist(data[, 2:12], method = "euclidean")

# Euclidean distance matrix
euclidean_dist_matrix <- as.matrix(dist(data[, 2:12], method = "euclidean"))

# Print the Euclidean distance matrix
print(euclidean_dist_matrix)

# Calculate summary statistics for the distances
min_distance <- min(euclidean_dist_matrix)
max_distance <- max(euclidean_dist_matrix)
mean_distance <- mean(euclidean_dist_matrix)
sd_distance <- sd(euclidean_dist_matrix)

# Print the summary statistics
cat("Minimum Distance:", min_distance, "\n")
cat("Maximum Distance:", max_distance, "\n")
cat("Mean Distance:", mean_distance, "\n")
cat("Standard Deviation of Distance:", sd_distance, "\n")
 
#elbow method

library(cluster)
library(factoextra)

# Extract the relevant columns for clustering (e.g., columns 2 to 12)
data_for_clustering <- data[, 2:12]

# Calculate the WWCS for a range of K values
k_values <- 1:10  
wws_values <- numeric(length(k_values))

for (k in k_values) {
  kmeans_model <- kmeans(data_for_clustering, centers = k)
  wws_values[k] <- sum(kmeans_model$withinss)
}

# Create a plot to visualize the WWS values
plot(k_values, wws_values, type = "b", 
     xlab = "Number of Clusters (K)",
     ylab = "WWS (Within-Cluster Sum of Squares)",
     main = "Elbow Method for Optimal K")
#Elbow point is at 4

#Using NbClust library
install.packages("NbClust")
library(NbClust)

NbClust(data = data[-1], diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "ward.D2", index = "all", alphaBeale=0.1)


#Dendrogram Interpretation
# Perform hierarchical clustering using the Euclidean distance

hclust_result <- hclust(dist(data[, 2:12], method = "euclidean"), method = "complete")

# Create the dendrogram plot
plot(hclust_result, hang = -1, main = "Dendrogram for Hierarchical Clustering", xlab = "Well Samples")

# Perform hierarchical clustering and cut the dendrogram into 3 clusters
hclust_result <- hclust(dist(data[, 2:12], method = "euclidean"), method = "ward.D2")

cutree_result <- cutree(hclust_result, k = 3)

# Create a data frame to store cluster assignments and sample numbers
clustered_samples <- data.frame(Cluster = cutree_result, SampleNumber = data$`well water sample_No`)

# Display the samples in each cluster
for (cluster_id in unique(clustered_samples$Cluster)) {
  cat("Cluster", cluster_id, "-", paste(clustered_samples$SampleNumber[clustered_samples$Cluster == cluster_id], collapse = ", "), "\n")
}

##objective 3##

# Standard means
standard_means <- c(Be = 4, Cr = 100, Fe = 300, Ni = 20 , Cu = 1300, As = 10, Cd =5, Ba = 2000, Tl = 0.5, Pb = 15, U = 30)
standard_means

mean_values <- colMeans(data[, c("Be", "Cr", "Fe", "Ni", "Cu", "As", "Cd", "Ba", "Tl", "Pb", "U")])
mean_values


# Compare mean values of your dataset with standard means
n=92 #no of samples
p=11 #no of variables
new_dataframe <- cbind(data[-1])
cov_matrix <- cov(new_dataframe)  #calculate the co-variance matrix
cov_matrix

x_bar = matrix(mean_values,c(11,1)) #column vector of the sample mean values
x_bar

mu_note =matrix(standard_means,c(11,1))#column vector of the standard values
mu_note

#Test statistics 
T2_cal <- n*t(x_bar-mu_note)%*%solve(cov_matrix)%*% (x_bar-mu_note)
T2_cal

#Critical Value
Table_value =(n-1)*p/(n-p)*qf(0.95,p,n-p)
Table_value

#T2_cal>Table_value (Well water samples not in line with accepted standards)