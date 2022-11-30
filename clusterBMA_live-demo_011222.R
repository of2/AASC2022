###################################
######## Load Packages ############
###################################

library(clusterBMA) #v0.1.1 - for combining clusters

library(clusterGeneration) #v1.3.7 - for generating (Gaussian) simulated clusters
library(ClusterR) #v1.2.2 - for k-means++
library(plotly) #v4.9.3 - for plots





















###################################
######## Simulate Dataset #########
###################################

# 5 multivariate Gaussian clusters, 100 in each, 2 dimensions, moderate separation

test_data <- clusterGeneration::genRandomClust(numClust=5,sepVal=0.05,numNonNoisy=2,numNoisy=0,clustSizes=c(rep(100,5)),numReplicate = 1,clustszind = 3)

test_data <- test_data$datList$test_1

plot(test_data)















###################################
######## K-means++ solution #######
###################################

# k-means++, k=5, get allocation probabilities matrix
test_kmeans <- ClusterR::KMeans_rcpp(data=test_data,clusters = 5,initializer = "kmeans++")

km_labs <- test_kmeans$clusters


# Turn cluster labels into cluster allocation probability matrix

km_probs <- hard_to_prob_fn(km_labs,n_clust=5)

# Plot the k-means++ allocations
plot(test_data, col = km_labs)


















#################################################
######## Hierarchical clustering solution #######
#################################################

test_hclust <- hclust(d=dist(test_data), method = 'ward.D2')

hclust_labs <- cutree(test_hclust,k = 5)


# Turn cluster labels into cluster allocation probability matrix

hc_probs <- hard_to_prob_fn(hclust_labs,n_clust=5)

# Plot the hclust allocations
plot(test_data, col = hclust_labs)


















#############################################
######## Combine them with clusterBMA #######
#############################################


# Put cluster allocation probability matrices into list, format required for function clusterBMA::clusterBMA()
input_probs <- list(km_probs,hc_probs)

# Run clusterBMA function to combine results from k-means++ and hclust
# Need to specify input dataset, list of cluster allocation probability matrices, and the number of final clusters selected
test_bma_results <- clusterBMA(input_data = test_data, cluster_prob_matrices = input_probs, n_final_clust = 5)


# BMA results = list of outputs: 1) consensus matrix, 2) BMA allocation probabilities, 3) BMA labels, 4) N in each cluster, 5) Weights for each algorithm

test_bma_results[[3]] # Key outputs

test_bma_results[[4]] # N in each cluster

test_bma_results[[5]] #Weights for each algorithms


















###############################################################
######## Plot the BMA clusters + allocation uncertainty #######
###############################################################

# add BMA outputs as columns onto original dataframe
test_data <- cbind(test_data,test_bma_results[[3]])

# Example plot BMA cluster results - larger points have greater uncertainty

test_plot_BMA_uncertainty <- plot_ly(data=test_data,x=test_data[,1],y=test_data[,2],color=factor(test_data$alloc_vector),colors = RColorBrewer::brewer.pal(5,"Dark2"), size=test_data$alloc_uncertainty,marker=list(sizeref=0.3, sizemode="area"))
test_plot_BMA_uncertainty








