#library(qtl)
setwd("/Users/Cody_2/git.repos/NAM_CAM_FIELD/raw_data/phenotypic_data/")
pheno <- read.csv("Amanjot_image_data_clean.csv", na.strings= "NA")

setwd("/Users/Cody_2/git.repos/NAM_CAM_FIELD/experimental_notebook/")
location <- read.csv("AtCol_Sha2014_plantlocation_clean.csv", na.strings = "NA")

str(pheno)
str(location)

pheno$Image
test <- gsub("(BL)(\\d)(_)(PL)(\\d)(_)(C)(\\d+)(-)(\\d+)(.tif)",
                    "\\1\\2\\3\\4", pheno$Image)

pheno$Block <- as.numeric(gsub("(BL)(\\d)(_)(PL)(\\d)(_)(C)(\\d+)(-)(\\d+)(.tif)",
                             "\\2", pheno$Image))
pheno$Block
pheno$Plot <- as.numeric(gsub("(BL)(\\d)(_)(PL)(\\d)(_)(C)(\\d+)(-)(\\d+)(.tif)",
                                 "\\5", pheno$Image))
pheno$Row <- as.numeric(gsub("(R)(\\d)(C)(\\d+)", "\\2",
                               pheno$Location))
pheno$Coltemp <- as.numeric(gsub("(BL)(\\d)(_)(PL)(\\d)(_)(C)(\\d+)(-)(\\d+)(.tif)",
                                   "\\8", pheno$Image))
pheno$Column <- as.numeric(gsub("(R)(\\d)(C)(\\d+)", "\\4", pheno$Location))
pheno$Column
head(pheno)
str(pheno)
tail(pheno)

# Amanjot and James split the images into three, so the Column is not correct
# this corrects the column to match the large image
pheno$Column <- (pheno$Coltemp + pheno$Column) - 1
pheno$Column
# looks good
head(pheno)
# reorganize dataframe
phenotypes <- pheno[,c(1:3,7:11,4:6)]
head(phenotypes)
phenotypes

?subset
phenotypes <- subset(phenotypes, Placeholder != "No Pot" )
phenotypes <- subset(phenotypes, Placeholder != "Reference" )
phenotypes
phenotypes2
dim(phenotypes)

head(location)
location

as.numeric(pheno$Row)
pheno$Row

head(location)
location$Row2 <- location$Row
location$Row <- as.numeric(location$Row2)
head(location, 50)
dim(location)
dim(phenotypes)


phenotypes$mergecol <-  paste(phenotypes$Block, phenotypes$Plot, phenotypes$Column, phenotypes$Row, sep="_")
location$mergecol   <- paste(location$Block, location$Plot, location$Column, location$Row, sep="_")






?merge
merged <- merge(phenotypes, location, all.x = TRUE)
head(merged, 30)

head(merged)
dim(merged)
tail(merged)
merged


merged <- merged[,c(5,12:13,1:4,14:15,6:11)]
head(merged)
dim(merged)
?sort

sorted <- merged[with(merged, order(Line)), ]
sorted
sorted$Plant
unique(sorted$Plant)