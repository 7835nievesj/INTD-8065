source("http://bioconductor.org/biocLite.R")
biocLite("GenomicRanges")
vignette("GenomicRangesIntroduction")

biocLite("airway")

library(airway)
?airway

data(airway)
airway

head(assays(airway)$counts)
dim(assays(airway)$counts)

head(rowRanges(airway))

head(colData(airway))

head(rownames(airway))

rownames(airway)[5]

# biomaRt package can get data related to the gene

biocLite("biomaRt")
library(biomaRt)
mart <- useEnsembl('ENSEMBL_MART_ENSEMBL')
dim(listDatasets(mart))

head(listDatasets(mart))

mart <- useEnsembl(biomart = "ensembl", dataset = "hsapiens_gene_ensembl")
head(listAttributes(mart))

dim(listAttributes(mart))

# Visualize data with ggbio
biocLite("ggbio")
library(ggbio)

biocLite("Homo.sapiens")
library(Homo.sapiens)

wh <- unlist(rowRanges(airway)[5])
p.g <- autoplot(Homo.sapiens, which = wh2)
# Error we need to rename the chromosomes

wh2 <- renameSeqlevels(wh, sub("1","chr1", seqlevels(wh)))
p.g2 <- autoplot(Homo.sapiens, which = wh2)
p.g2

# Analysis with limma
biocLite("limma")
library(limma)

design <- model.matrix(~ dex, data = colData(airway))
v <- voom(assays(airway)$counts, design = design)
fit <- lmFit(v, design)
fit <- eBayes(fit)
summary(decideTests(fit))

topTable(fit, coef = "dexuntrt")

plotMA(fit)

