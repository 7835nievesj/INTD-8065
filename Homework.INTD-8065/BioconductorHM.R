# set working directory
setwd("/Users/jose_nieves/Documents/School/INTD_8065/GitHub.INTD-8065/
      Homework.INTD-8065")

# install bioconductor package
source("https://bioconductor.org/biocLite.R")
biocLite("caOmicsV")

# install caOmicsV package
library(caOmicsV)

# read vignettes for the caOmicsVR package
browseVignettes("caOmicsV")

# generate a bioMatrix layout image with the build-in demo data
data(biomatrixPlotDemoData)

plotBioMatrix(biomatrixPlotDemoData, summaryType="text")
bioMatrixLegend(heatmapNames=c("RNASeq", "miRNASeq"), 
                categoryNames=c("Methyl H", "Methyl L"), 
                binaryNames=c("CN LOSS", "CN Gain"),   
                heatmapMin=-3, heatmapMax=3, colorType="BlueWhiteRed")

# bioNetCircos layout image with the build-in demo data
data(bionetPlotDemoData)

plotBioNetCircos(bionetPlotDemoData)
dataNames <- c("Tissue Type", "RNASeq", "miRNASeq", "Methylation", "CNV")
bioNetLegend(dataNames, heatmapMin=-3, heatmapMax=3)

# making plot data set (list of function's arguments)
args(getPlotDataSet) 

# plot biomatrix layout

# 1. Demo data
dataSet <- biomatrixPlotDemoData
names(dataSet)

# 2. initialize biomatrix layout
numOfGenes <- length(dataSet$geneNames);
numOfSamples <- length(dataSet$sampleNames); 
numOfPhenotypes <- nrow(dataSet$sampleInfo)-1;

numOfHeatmap <- length(dataSet$heatmapData);
numOfSummary <- length(dataSet$summaryData);
phenotypes   <- rownames(dataSet$sampleInfo)[-1];

sampleHeight <- 0.4;
sampleWidth <- 0.1; 
samplePadding <- 0.025;
geneNameWidth <- 1;
sampleNameHeight <- 2.5;
remarkWidth <- 2; 
summaryWidth <- 1;
rowPadding <- 0.1;

initializeBioMatrixPlot(numOfGenes, numOfSamples, numOfPhenotypes, 
                        sampleHeight, sampleWidth, samplePadding, rowPadding, 
                        geneNameWidth, remarkWidth, summaryWidth, sampleNameHeight)
caOmicsVColors <- getCaOmicsVColors()

png("caOmicsVbioMatrixLayoutDemo.png", height=8, width=12, 
    unit="in", res=300, type="cairo")
par(cex=0.75)
showBioMatrixPlotLayout(dataSet$geneNames,dataSet$sampleNames, phenotypes)

# 3. plot tissue types on phenotype area
head(dataSet$sampleInfo)[,1:3]
rowIndex <- 2;

sampleGroup <- as.character(dataSet$sampleInfo[rowIndex,])
sampleTypes <- unique(sampleGroup)
sampleColors <- rep("blue", length(sampleGroup));
sampleColors[grep("Tumor", sampleGroup)] <- "red"

rowNumber <- 1
areaName <- "phenotype"
plotBioMatrixSampleData(rowNumber, areaName, sampleColors);

geneLabelX <- getBioMatrixGeneLabelWidth()
maxAreaX <- getBioMatrixDataAreaWidth()
legendH <- getBioMatrixLegendHeight()
plotAreaH <- getBioMatrixPlotAreaHeigth()
sampleH<- getBioMatrixSampleHeight()

sampleLegendX <- geneLabelX + maxAreaX
sampleLegendY <- plotAreaH + legendH - length(sampleTypes)*sampleH
colors <- c("blue", "red")
legend(sampleLegendX, sampleLegendY, legend=sampleTypes, 
       fill=colors,  bty="n", xjust=0)

# 4. heatmap plot
heatmapData <- as.matrix(dataSet$heatmapData[[1]][,]);
plotBioMatrixHeatmap(heatmapData, maxValue=3, minValue=-3)

heatmapData <- as.matrix(dataSet$heatmapData[[2]][,])
plotBioMatrixHeatmap(heatmapData, topAdjust=sampleH/2,  
                     maxValue=3, minValue=-3);

secondNames <- as.character(dataSet$secondGeneNames)
textColors <- rep(caOmicsVColors[3], length(secondNames));
plotBioMatrixRowNames(secondNames, "omicsData", textColors, 
                      side="right", skipPlotColumns=0);

# 5. draw outlines for each samples to show methylation status
categoryData <- dataSet$categoryData[[1]]
totalCategory <- length(unique(as.numeric(dataSet$categoryData[[1]])))

plotColors <- rev(getCaOmicsVColors())
plotBioMatrixCategoryData(categoryData, areaName="omicsData", 
                          sampleColors=plotColors[1:totalCategory])

# 6. binary data plot
binaryData <- dataSet$binaryData[[1]];
plotBioMatrixBinaryData(binaryData, sampleColor=caOmicsVColors[4]);

binaryData <- dataSet$binaryData[[2]];
plotBioMatrixBinaryData(binaryData, sampleColor=caOmicsVColors[3])

# 7. plot summary data on right side of plot area
summaryData  <- dataSet$summaryInfo[[1]][, 2];
summaryTitle <- colnames(dataSet$summaryInfo[[1]])[2];

remarkWidth <- getBioMatrixRemarkWidth();
sampleWidth <- getBioMatrixSampleWidth();
col2skip <- remarkWidth/2/sampleWidth + 2;

plotBioMatrixRowNames(summaryTitle, areaName="phenotype", 
                      colors="black", side="right", skipPlotColumns=col2skip);

plotBioMatrixRowNames(summaryData, "omicsData", 
                      colors=caOmicsVColors[3], side="right", 
                      skipPlotColumns=col2skip)

# 8. add legend
bioMatrixLegend(heatmapNames=c("RNASeq", "miRNASeq"), 
                categoryNames=c("Methyl H", "Methyl L"), 
                binaryNames=c("CN LOSS", "CN Gain"),   
                heatmapMin=-3, heatmapMax=3, 
                colorType="BlueWhiteRed")
dev.off()

# plot bionetcircos layout

# 1. Demo data
data(bionetPlotDemoData)
dataSet <- bionetPlotDemoData

sampleNames  <- dataSet$sampleNames
geneNames    <- dataSet$geneNames
numOfSamples <- length(sampleNames)

numOfSampleInfo <- nrow(dataSet$sampleInfo) - 1
numOfSummary <- ifelse(dataSet$summaryByRow, 0, col(dataSet$summaryInfo)-1)
numOfHeatmap <- length(dataSet$heatmapData)
numOfCategory <- length(dataSet$categoryData)
numOfBinary <- length(dataSet$binaryData)

expr <- dataSet$heatmapData[[1]]
bioNet <- bc3net(expr) 

# 2. initialize bionetcircos layout
widthOfSample    <- 100
widthBetweenNode <- 3
lengthOfRadius   <- 10

dataNum <- sum(numOfSampleInfo, numOfSummary, numOfHeatmap, 
               numOfCategory, numOfBinary)
trackheight <- 1.5
widthOfPlotArea  <- dataNum*2*trackheight

initializeBioNetCircos(bioNet, numOfSamples, widthOfSample, 
                       lengthOfRadius, widthBetweenNode, widthOfPlotArea)
caOmicsVColors <- getCaOmicsVColors()
supportedType <- getCaOmicsVPlotTypes()

par(cex=0.75)
showBioNetNodesLayout()

# 3. label each node
par(cex=0.6)
onTop <- c(14, 15, 16, 9, 7, 20, 8, 24, 10, 25)
labelBioNetNodeNames(nodeList=onTop,labelColor="blue", 
                     labelLocation="top", labelOffset = 0.7)

onBottom <- c(26, 22, 23, 18, 19, 3, 5)
labelBioNetNodeNames(nodeList=onBottom,labelColor="black", 
                     labelLocation="bottom", labelOffset = 0.7)

onLeft <- c(2, 11, 21, 17)
labelBioNetNodeNames(nodeList=onLeft,labelColor="red", 
                     labelLocation="left", labelOffset = 0.7)

onRight <- c(13, 12, 4, 1, 6)
labelBioNetNodeNames(nodeList=onRight,labelColor="brown", 
                     labelLocation="right", labelOffset = 0.7)
eraseBioNetNode() # once nodes are labelled plot area erased

# 4. plot each data set
inner <- lengthOfRadius/2
outer <- inner +  trackheight

# plot tissue type for each node
groupInfo <- as.character(dataSet$sampleInfo[2, ])
sampleColors <- rep("blue", numOfSamples);
sampleColors[grep("Tumor", groupInfo)] <- "red"

plotType=supportedType[1]
groupInfo <- matrix(groupInfo, nrow=1)
bioNetCircosPlot(dataValues=groupInfo, 
                 plotType, outer, inner, sampleColors)

inner <- outer + 0.5
outer <- inner +  trackheight  

# heatmap plot for each node
exprData <- dataSet$heatmapData[[1]]
plotType <- supportedType[4] 
bioNetCircosPlot(exprData, plotType, outer, inner, 
                 plotColors="BlueWhiteRed", maxValue=3, minValue=-3)

inner <- outer + 0.5 
outer <- inner +  trackheight

# category data plot for each node
categoryData <- dataSet$categoryData[[1]]
plotType <- supportedType[2];
bioNetCircosPlot(categoryData, plotType, 
                 outer, inner, plotColors="red")
inner <- outer + 0.5
outer <- inner +  trackheight

# binary data plot for each node
binaryData <- dataSet$binaryData[[1]]
plotType <- supportedType[3]
plotColors <- rep(caOmicsVColors[1], ncol(binaryData))
bioNetCircosPlot(binaryData, plotType, 
                 outer, inner, plotColors)

inner <- outer + 0.5
outer <- inner +  trackheight 

# link samples on a node
outer <- 2.5
bioNetGraph <- getBioNetGraph()
nodeIndex <- which(V(bioNetGraph)$name=="PLVAP")

fromSample <- 10 
toSample <- 50 
plotColors <- "red"
linkBioNetSamples(nodeIndex, fromSample, 
                  toSample, outer, plotColors)

fromSample <- 40 
toSample <- 20 
plotColors <- "blue"
linkBioNetSamples(nodeIndex, fromSample, 
                  toSample, outer, plotColors)

# 5. add legend        
dataNames <- c("Tissue Type", "RNASeq", "Methylation", "CNV")
bioNetLegend(dataNames, heatmapMin=-3, heatmapMax=3)

dev.off()
