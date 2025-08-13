# =========================================
# FACTOR ANALYSIS ####
# =========================================

#Load Required Libraries
# Install the 'psych' package if not already installed
if(!require(psych)) install.packages("psych", dependencies = TRUE)
library(psych)

#Factor Analysis on the Iris Dataset
# Load iris dataset
data(iris)
# View the first few rows
head(iris)
# Scale numeric variables (first 4 columns)
iris_scaled <- scale(iris[, 1:4])
# Determine the number of factors using factor analysis
# Using 4 factors and varimax rotation
fa_iris <- fa(r = iris_scaled, 
              nfactors = 4, 
              rotate = "varimax")

# Summarize results
summary(fa_iris)
# View factor loadings
fa_iris$loadings
# Optional: Factor analysis on a subset
subset1 <- subset(iris[, 1:4], iris$Sepal.Length < mean(iris$Sepal.Length))
fa_subset <- fa(subset1, nfactors = 4, rotate = "varimax")
print(fa_subset)


# Factor Analysis on the mtcars Dataset
# Load mtcars dataset
data(mtcars)
# Perform factor analysis using 'factanal' with 3 factors and varimax rotation
factor_analysis_mtcars <- factanal(x = mtcars, 
                                   factors = 3, 
                                   rotation = "varimax")
# Print results
print(factor_analysis_mtcars)
# Notes:
# factanal() arguments:
#   x        : numeric data matrix or data frame
#   factors  : number of factors to extract
#   rotation : rotation method (e.g., "varimax", "promax")
#   scores   : type of factor scores ("none", "regression", "Bartlett")
#   covmat   : covariance matrix if x is not provided



# =========================================
# MULTIPLE FACTOR ANALYSIS (MFA) IN R####
# =========================================

# Step 1: Install and load required packages
if(!require(FactoMineR)) install.packages("FactoMineR", dependencies = TRUE)
library(FactoMineR)

if(!require(factoextra)) install.packages("factoextra", dependencies = TRUE)
library(factoextra)

# Step 2: Load the dataset
data("iris")
head(iris)

# Step 3: Define groups for MFA
iris_data <- iris[, 1:4]          # Only numeric columns
group_definitions <- c(2, 2)      # Sepal: 2 variables, Petal: 2 variables

# Step 4: Perform Multiple Factor Analysis
res_mfa <- MFA(
  iris_data,
  group = group_definitions,
  type = c("s", "s"),              # Quantitative variables
  name.group = c("Sepal", "Petal"),
  graph = FALSE
)

# View MFA summary
summary(res_mfa)

# Step 5: Visualize MFA results
# Individuals colored by species with ellipses
fviz_mfa_ind(
  res_mfa, 
  label = "var", 
  habillage = iris$Species, 
  addEllipses = TRUE, 
  ellipse.level = 0.95
)

# Contributions of quantitative variables to Dimension 1
fviz_contrib(res_mfa, choice = "quanti.var", axes = 1)


# =========================================
# CONFIRMATORY FACTOR ANALYSIS (CFA) ####
# =========================================

# Step 1: Install and load required package
if(!require(lavaan)) install.packages("lavaan", dependencies = TRUE)
library(lavaan)

# Step 2: Load and check the structure of the dataset
data("HolzingerSwineford1939")  # Load dataset from lavaan package
head(HolzingerSwineford1939)    # View first few rows
str(HolzingerSwineford1939)     # Check structure

# Step 3: Specify the CFA model
# Three latent variables:
#   visual   -> x1, x2, x3
#   textual  -> x4, x5, x6
#   speed    -> x7, x8, x9
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

# Step 4: Run CFA and check results
cfa_result <- cfa(model, data = HolzingerSwineford1939)

# Display summary with fit measures and standardized estimates
summary(cfa_result, fit.measures = TRUE, standardized = TRUE)



# =========================================
# # Exploratory Factor Analysis (EFA) in R ####
# =========================================

# Step 1: Install and Load Required Packages
if(!require(psych)) install.packages("psych", dependencies = TRUE)
library(psych)

if(!require(factoextra)) install.packages("factoextra", dependencies = TRUE)
library(factoextra)

if(!require(lavaan)) install.packages("lavaan", dependencies = TRUE)
library(lavaan)

# Step 2: Load and Inspect the Dataset
data(mtcars)
head(mtcars)   # View first few rows

# Step 3: Perform Exploratory Data Analysis (EDA)
# Check for missing values
sum(is.na(mtcars))

# Check for outliers
boxplot(mtcars, main = "Boxplot of mtcars Variables")

# Step 4: Conduct EFA
# Perform EFA with 2 factors and Varimax rotation
efa_result <- fa(r = mtcars, nfactors = 2, rotate = "varimax")

# Step 5: View EFA Results
print(efa_result)

