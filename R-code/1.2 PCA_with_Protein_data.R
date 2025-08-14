
# Principal Component Analysis (PCA) with Protein data 

# --- 1. Load Required Libraries ---
# Install packages if not already installed
required_packages <- c("corrr", "ggcorrplot", "FactoMineR", "factoextra", "ibawds")
lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# --- 2. Load and Inspect Data ---
data(protein)  # From 'ibawds' package
protein_data <- protein
cat("Dataset Dimensions:", dim(protein_data), "\n")
str(protein_data)
head(protein_data)

# --- 3. Check for Missing Values ---
missing_counts <- colSums(is.na(protein_data))
print(missing_counts)

# --- 4. Select Numerical Variables ---
numerical_data <- protein_data[, 2:10]
head(numerical_data)

# --- 5. PCA Computation ---
pca_model <- princomp(numerical_data, cor = TRUE) # cor=TRUE standardizes the data
summary(pca_model)  # Proportion of variance explained
pca_model$loadings[, 1:2]  # Loadings for first two PCs

# --- 6. PCA Visualization ---
# Scree plot (Eigenvalues)
fviz_eig(pca_model, addlabels = TRUE)

# Variable correlation plot (PCA Biplot)
fviz_pca_var(pca_model, col.var = "black")

# Cos²: Quality of representation on PC1 & PC2
fviz_cos2(pca_model, choice = "var", axes = 1:2)

# PCA variable plot with Cos² coloring
fviz_pca_var(
  pca_model,
  col.var = "cos2",
  gradient.cols = c("black", "orange", "green"),
  repel = TRUE
)
