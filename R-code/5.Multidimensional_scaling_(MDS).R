#================================================
#Multidimensional scaling (MDS) using R
     #---------Part(a)-----------#
#================================================

#Step 1: Create an empty matrix and define its dimensions ---
# The matrix will hold the distance data between 10 U.S. cities.
Air_line_data_2 <- matrix(ncol = 10, nrow = 10)

# Assign city names to the columns and rows
colnames(Air_line_data_2) <- c("Atlanta", "Chicago", "Denver", "Houston", "Los_Angeles", "Miami", "New_York", "San_Francisco", "Seattle", "Washington_D.C")
rownames(Air_line_data_2) <- c("Atlanta", "Chicago", "Denver", "Houston", "Los_Angeles", "Miami", "New_York", "San_Francisco", "Seattle", "Washington_D.C")

#Step 2: Populate the matrix with distance data ---
# The distances are fed into the lower triangle of the matrix.
Air_line_data_2[lower.tri(Air_line_data_2)] <- c(587, 1212, 701, 1936, 604, 748, 2139, 2182, 543, 920, 940, 1745, 1188, 713, 1858, 1737, 597, 879, 831, 1726, 1631, 949, 1021, 1494, 1374, 968, 1420, 1645, 1891, 1220, 2339, 2451, 347, 959, 2300, 1092, 2594, 2734, 923, 2571, 2408, 205, 678, 2442, 2329)

# Set the diagonal to zero (distance from a city to itself is 0)
diag(Air_line_data_2) <- 0

#Step 3: Convert the matrix to a distance object ---
# The cmdscale function works with distance objects.
# We also reflect the lower triangle to the upper triangle to make it a full symmetric matrix.
dist_data <- as.dist(Air_line_data_2)

#Step 4: Perform Classical (Metric) MDS ---
# cmdscale() performs the scaling, reducing the data to k=2 dimensions.
MMDS_1 <- cmdscale(dist_data, k = 2)

#Step 5: Plot the results ---
# 'type="n"' creates an empty plot.
plot(MMDS_1[, 1], MMDS_1[, 2], type = "n", xlab = "", ylab = "", axes = FALSE, main = "cmdscale (stats)")

# 'text()' adds the city names to the plot at their new 2D coordinates.
text(MMDS_1[, 1], MMDS_1[, 2], labels = rownames(MMDS_1), cex = 0.9, xpd = TRUE)


#================================================
#Multidimensional scaling (MDS) using R
     #---------Part(b)-----------#
#================================================

#Step 1: Load the MASS library for the isoMDS function ---
library(MASS)

#Step 2: Create an empty matrix for the dissimilarity data ---
World_war_Politicians_data_1 <- matrix(ncol = 12, nrow = 12)

# Assign names to the columns and rows
politician_names <- c("Hitler", "Mussolini", "Churchill", "Eisenhower", "Stalin", "Attlee", "Franco", "De_Gaulle", "Mao_Tse", "Truman", "Chamberlain", "Tito")
colnames(World_war_Politicians_data_1) <- politician_names
rownames(World_war_Politicians_data_1) <- politician_names

#Step 3: Populate the matrix with dissimilarity data ---
# Fill the lower triangle of the matrix with the provided dissimilarity scores.
World_war_Politicians_data_1[lower.tri(World_war_Politicians_data_1)] <- c(5, 11, 15, 8, 17, 5, 10, 16, 17, 12, 16, 14, 16, 13, 18, 3, 11, 18, 18, 14, 17, 7, 11, 11, 12, 5, 16, 8, 10, 8, 16, 16, 14, 8, 17, 6, 7, 12, 15, 13, 11, 12, 14, 16, 12, 16, 12, 16, 12, 9, 13, 9, 17, 16, 10, 12, 13, 9, 11, 7, 12, 17, 10, 9, 11, 15)

# Set the diagonal to zero
diag(World_war_Politicians_data_1) <- 0

#Step 4: Convert the matrix to a distance object ---
dist_politicians <- as.dist(World_war_Politicians_data_1)

#Step 5: Perform Non-Metric MDS ---
# The isoMDS() function is used for non-metric scaling.
NMMDS_1 <- isoMDS(dist_politicians, k = 2)

#Step 6: Plot the results ---
# Create an empty plot to place the labels on.
plot(NMMDS_1$points, type = "n", xlab = "", ylab = "", axes = FALSE)

# Add the politician names at their scaled coordinates.
text(NMMDS_1$points, labels = politician_names, cex = 0.9, xpd = TRUE)
