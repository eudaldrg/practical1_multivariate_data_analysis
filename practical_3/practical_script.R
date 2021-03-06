cereal_data <- as.data.frame(read.table("practical_3/Cereals.dat", header = TRUE))

## Standardized data
numerical_values <- cereal_data[,3:ncol(cereal_data)]
sd_numerical <- scale(numerical_values)

## Euclidean distance
eucl_dist <- as.matrix(dist(sd_numerical, upper = T))

## 2D approx
multidim_scale <- cmdscale(eucl_dist, k = 2, eig = T, x.ret = T)
multidim_scale$eig

## Closest points
approx_dist <- as.matrix(dist(multidim_scale$points, upper = T))
which(approx_dist == max(approx_dist), arr.ind = T)
cereal_data$Brand[41]
cereal_data$Brand[18]
cereal_data$Brand

## K dimension matrix
#Yes, 6

## Eigenvalues + GOF
multidim_scale$GOF
B <- multidim_scale$x
B.eigen <- eigen(B)
sum(B.eigen$values[1:2]) / sum(B.eigen$values)

B <- t(sd_numerical) %*% sd_numerical
B.eigen <- eigen(B)
B.eigen$values
sum(B.eigen$values[1:2]) / sum(B.eigen$values)

## Are there 0 Eigen? Why not?
# No.

## Plot distances and do regression.
eucl_dist_no_matrix <- dist(sd_numerical)
approx_dist_no_matrix <- dist(multidim_scale$points)
helper_df <- data.frame(cbind(approx_dist_no_matrix, eucl_dist_no_matrix))

plot(helper_df$approx_dist_no_matrix, helper_df$eucl_dist_no_matrix)

linearMod <- lm(approx_dist_no_matrix ~ eucl_dist_no_matrix, data = helper_df)
summary(linearMod)

## Non-metric
non_metric <- MASS::isoMDS(eucl_dist)


# todo make plot nicer
non_metric
plot(non_metric$points)

## Check two closest
approx_dist_non_metric <- dist(non_metric$points)
helper_df_non_metric <- data.frame(cbind(approx_dist_non_metric, eucl_dist_no_matrix))

## Regression
plot(helper_df_non_metric$approx_dist_non_metric, helper_df_non_metric$eucl_dist_no_matrix)

linearMod_non_metric <- lm(approx_dist_non_metric ~ eucl_dist_no_matrix, data = helper_df)
summary(linearMod_non_metric)

## Stress

## Scatterplot

helper_scatter <- data.frame(cbind(multidim_scale$points, non_metric$points))
colnames(helper_scatter) <- c("MetricDim1","MetricDim2","NonMetricDim1","NonMetricDim2")
plot(helper_scatter)
round(cor(helper_scatter), digits = 3)

## Why sd?