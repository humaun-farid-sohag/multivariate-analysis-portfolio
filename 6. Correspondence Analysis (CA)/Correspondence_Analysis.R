# ===============================================================
# Correspondence Analysis on USArrests Dataset
# ===============================================================

#Step 1. Load Required Packages and Load Dataset
#install.packages(c("FactoMineR", "factoextra", "ca"))

library(FactoMineR)   # For CA function
library(factoextra)   # For visualization
library(ca)           # For correspondence analysis

data("USArrests")  # Built-in dataset
head(USArrests)    # Preview first few rows

#Step  2. Correspondence Analysis using FactoMineR
ca_result_facto <- CA(USArrests)  # Perform CA

# Visualization: Biplot
fviz_ca_biplot(ca_result_facto, repel = TRUE)

#Step  3. Correspondence Analysis using 'ca' package
ca_result <- ca(USArrests, graph = FALSE)  # Perform CA

#Step  4. Eigenvalues & Scree Plot
eig_values <- get_eigenvalue(ca_result)
print(eig_values)         # Display eigenvalues
fviz_eig(ca_result)       # Scree plot

#Step  5. Row & Column Profiles
row_profiles <- get_ca_row(ca_result)
print(row_profiles)

col_profiles <- get_ca_col(ca_result)
print(col_profiles)

#Step  6. Visualize Row & Column Profiles
fviz_ca_row(ca_result)    # Row profiles
fviz_ca_col(ca_result)    # Column profiles

#Step  7. Biplot (Rows & Columns)
fviz_ca_biplot(ca_result, repel = TRUE)
