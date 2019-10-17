
# Intro to CCA

# Install and load packages
required_packages <- c(
  "ggplot2",
  "GGally",
  "CCA",
  "CCP"
)

for (package in required_packages) {
  if (!(package %in% rownames(installed.packages()))) {
    install.packages(package, dep = T)
  }
}

require(ggplot2)
require(GGally)
require(CCA)
require(CCP)
require(pheatmap)
require(magrittr)

# Load data
mm <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
summary(mm)

# === Canonical correlation analysis ===========================================
# Below we use the canon command to conduct a canonical correlation analysis. It
# requires two sets of variables enclosed with a pair of parentheses. We specify
# our psychological variables as the first set of variables and our academic 
# variables plus gender as the second set. For convenience, the variables in the
# first set are called “u” variables and the variables in the second set are 
# called “v” variables.

# Let’s look at the data.

xtabs(~Sex, data = mm)

psych <- mm[, 1:3]
acad <- mm[, 4:8]

ggpairs(psych)
ggpairs(acad)

# correlations
cor_xy <- matcor(psych, acad)

# Heatmap to visualise this
pheatmap(cor_xy$XYcor, cluster_rows = F, cluster_cols = F)

cc1 <- cc(psych, acad)

# display the canonical correlations
cc1$cor
cc1[3:4]

# compute canonical loadings
cc2 <- comput(psych, acad, cc1)

# display canonical loadings
cc2[3:6]

# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(psych)[1]
p <- length(psych)
q <- length(acad)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")

# standardized psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(psych))))
s1 %*% cc1$xcoef

# standardized acad canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(acad))))
s2 %*% cc1$ycoef

