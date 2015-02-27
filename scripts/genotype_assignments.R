#library(qtl)
setwd("/Users/Cody_2/git.repos/NAM_CAM_FIELD/raw_data/phenotypic_data/")
pheno <- read.csv("Amanjot_image_data_clean.csv", na.strings= "NA")
pheno$Placeholder
pheno

setwd("/Users/Cody_2/git.repos/NAM_CAM_FIELD/experimental_notebook/")
location <- read.csv("AtCol_Sha2014_plantlocation_clean.csv", na.strings = "NA")
dim(pheno)
str(pheno)
str(location)
dim(location)
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
dim(phenotypes)
?subset
phenotypes <- subset(phenotypes, Placeholder != "No Pot" )
phenotypes <- subset(phenotypes, Placeholder != "Reference" )
dim(phenotypes)
dim(location)
################
# I really hesitate to do this line by line, but I think that it is necessary not to get 
# confused
################
head(location)
head(phenotypes)

# block and plot are the same between the dataframes
# column and row are defined differently in the two datasets
# I will use an X, Y grid to convert between the two
# The phenotypes data frame already has an X, Y grid of columns (1-14) and rows (1-6)
# this is starting at column 1, row 1 in the upper left hand corner of an image
# this corresponds to the Southwest corner of each plot in AtSha_col2014plotdesign.png
# first I will deal with the rows
# I thought initially of changing 2 things at once, hence the ifelse, but thought it 
# also confusing without looking at AtSha_col2014plotdesign.png, so here is my attempt

#copy column to change in place
# make numeric so the ifelse will change correctly
location$Row2 <- as.numeric(location$Row)
head(location)

location$Row2  <- with(location, ifelse(Row == 'A', 4, Row2))
location$Row2
location$Row2  <- with(location, ifelse(Row == 'B', 5, Row2))
location$Row2  <- with(location, ifelse(Row == 'C', 6, Row2))
location$Row2  <- with(location, ifelse(Row == 'D', 3, Row2))
location$Row2  <- with(location, ifelse(Row == 'E', 2, Row2))
location$Row2  <- with(location, ifelse(Row == 'F', 1, Row2))
location$Row2
################
################

################
################
# columns
################
################
location$Column2 <- location$Column
head(location)
location$Column2
str(location)

location$Column2  <- with(location, ifelse(Column == 15, 1, Column2))
location$Column2
location$Column2  <- with(location, ifelse(Column == 16, 2, Column2))
location$Column2  <- with(location, ifelse(Column == 17, 3, Column2))
location$Column2  <- with(location, ifelse(Column == 18, 4, Column2))
location$Column2  <- with(location, ifelse(Column == 19, 5, Column2))
location$Column2  <- with(location, ifelse(Column == 20, 6, Column2))
location$Column2  <- with(location, ifelse(Column == 21, 7, Column2))
location$Column2  <- with(location, ifelse(Column == 22, 8, Column2))
location$Column2  <- with(location, ifelse(Column == 23, 9, Column2))
location$Column2  <- with(location, ifelse(Column == 24, 10, Column2))
location$Column2  <- with(location, ifelse(Column == 25, 11, Column2))
location$Column2  <- with(location, ifelse(Column == 26, 12, Column2))
location$Column2  <- with(location, ifelse(Column == 27, 13, Column2))
location$Column2  <- with(location, ifelse(Column == 28, 14, Column2))
location$Column2
head(location)
head(phenotypes)


phenotypes$mergecol <-  paste(phenotypes$Block, phenotypes$Plot, phenotypes$Column, phenotypes$Row, sep="_")
location$mergecol   <- paste(location$Block, location$Plot,location$Column2, location$Row2 , sep="_")

dim(phenotypes)
dim(location)
head(location)
head(phenotypes)




?merge
merged <- merge(phenotypes, location, by = "mergecol", all.x = TRUE)
head(merged)

head(merged, 100)
tail(merged, 50)
merged$Line
dim(merged)
tail(merged)
merged

setwd("/Users/Cody_2/git.repos/NAM_CAM_FIELD/raw_data/phenotypic_data/")
write.table(merged, "genotype_phenotype_merged2.csv", sep = ",", row.names = FALSE)












merged <- merged[,c(5,12:13,1:4,14:15,6:11)]
head(merged)
dim(merged)
?sort

sorted <- merged[with(merged, order(Line)), ]
sorted
sorted$Plant
unique(sorted$Plant)