# Using dplyr, filter the dataset so it only contains patients who died.
filter(VA, status == 1)
deadVA_df <- filter(VA, status == 1)

# Change cell level to "Squamous","Small Cell","Adeno","Big Cell".
deadVA_df$cell <- factor(deadVA_df$cell, 
                         levels = c(1,2,3,4), 
                         labels = c("Squamous","Small Cell","Adeno","Big Cell"))

# Produce a barplot for the cell types of tumors.
CellTypes <- table(deadVA_df$cell)
barplot(CellTypes, main = "Tumor Cell Type", xlab = "Cell Type", ylab = "Cell Frequency")

# Change treat level to "Standard","Treated"
deadVA_df$treat <- factor(deadVA_df$treat, 
                         levels = c(1,2), 
                         labels = c("Standard","Test"))

# Produce a barplot for the treatment types.
treatment_groups <- table(deadVA_df$treat)
barplot(treatment_groups, main = "Standard Vs. Tested Treatment", xlab = "Treatment", ylab = "Trement Frequency")