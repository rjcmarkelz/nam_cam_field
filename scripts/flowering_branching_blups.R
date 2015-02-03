# Cody Markelz
# August 6 2014
# mixed models to get blups for developmental traits on col/sha field data
# 
setwd("~/git.repos/NAM_field/input/")
flowering <- read.table("Summer2014_NAM_RNA_FINAL_CLEAN_RNA_removed.csv", 
	                    header=TRUE, sep = ",", na.strings = "NA")
head(flowering)
str(flowering)

# get dataframe ready for analysis
flowering$Plant <- as.factor(flowering$Plant)
flowering$Block <- as.factor(flowering$Block)
flowering$GermD <- as.Date(flowering$GermD, format = "%m/%d/%Y")
flowering$BoltD <- as.Date(flowering$BoltD, format = "%m/%d/%Y")
flowering$FlrD <- as.Date(flowering$FlrD, format = "%m/%d/%Y")
flowering$Apical_Aborted <- as.character(flowering$Apical_Aborted)

str(flowering)

# make some date values
flowering$germ2bolt <- as.numeric(flowering$BoltD - flowering$GermD)
flowering$germ2flr <- as.numeric(flowering$FlrD - flowering$GermD)
flowering$bolt2flr <- as.numeric(flowering$FlrD - flowering$BoltD)
flowering$branch_tot <- flowering$AM_Branches + flowering$Basal
head(flowering)

hist(flowering$germ2bolt)
hist(flowering$germ2flr)
hist(flowering$bolt2flr)
hist(flowering$AM_Branches)
hist(flowering$Basal)
hist(flowering$branch_tot)


flr_lm <- lm(germ2flr ~ Line + Block, data = flowering)
summary(flr_lm)
branch_lm <- lm(AM_Branches ~ Line + Block, data = flowering)
summary(branch_lm)

#mixed models


library(lme4)
germ2flower_1 <- lmer(germ2flr ~ Line + (1|Block), 
	                  data = flowering, REML = FALSE)
germ2flower_2 <- lmer(germ2flr ~ (1|Block), 
	                  data = flowering, REML = FALSE)


anova(germ2flower_1, germ2flower_2) # no crowding by nutrient interaction
anova(germ2flower_1, germ2flower_3) # crowding by RIL interaction, yeah!
anova(germ2flower_1, germ2flower_4) # marginal nutrient by genotype interaction

library(lsmeans)
library(ggplot2)
# use model 2
?.old.lsmeans
?lsmeans
germ2flower_lsmeans <- .old.lsmeans(germ2flower_1, "Line")
germ2flower_lsmeans
germ2flower_lsmeans_1 <- germ2flower_lsmeans[[1]]
germ2flower_lsmeans_1

limits <- aes(ymax= lsmean + SE, ymin = lsmean - SE)
dodge <- position_dodge(width=0.9)
plotlength5 <- ggplot(data = germ2flower_lsmeans_1, 
                  aes(y=lsmean, x= Line))
plotlength5 <- plotlength5 + geom_bar(position=dodge, stat="identity") 
plotlength5 <- plotlength5 + geom_errorbar(limits, position = dodge, width = 0.25) 
plotlength5 <- plotlength5 + xlab("Nitrogen") + ylab("Leaf length (cm)")
plotlength5 <- plotlength5 + ggtitle("Leaf Length")
plotlength5 <- plotlength5 + theme(axis.title=element_text(face="bold",
                               size="14"), axis.text=element_text(face="bold",
                               size="10"))  
plotlength5
setwd("~/git.repos/brassica_field_2014/output/")
ggsave(plotlength5, file="brassica_germ2flower014.png", width=15, height=8)



# automated blup 
names(flowering)
varlist <- names(flowering)[8:ncol(flowering)]
varlist

models <- lapply(varlist, function(x) {
    lmer(substitute(i ~ (1|Line) + (1|Block), list(i = as.name(x))), 
                      data = flowering)
})

# take a peak
models[[1]]


#name the model list
names(models) <- varlist
names(models)
#print a few traits to take a look
flr <- as.data.frame(coef(models[[1]])$Line)
flr
names(flr) 
str(flr)
hist(flr$(Intercept))
coef(models[[2]])$Line



rils <- data.frame(RILs = "")
for (trait in varlist) {
  rils <- merge(rils, data.frame(RILs=rownames(coef(models[[trait]])$Line),                          
                                placeholder=coef(models[[trait]])$Line[,1]),
                all.y = T)
  colnames(rils)[length(colnames(rils))] <- trait
}

head(rils)
head(rils)
tail(rils)
rils$RILs <- as.character(rils$RILs)
str(rils)

head(rils)
tail(rils)

# quickly reformat RIL names to match genetic map

rils$RILs <- sub("(13RV)(00)(\\d)","RIL_\\3", rils$RILs)
rils$RILs <- sub("(13RV)(0)(\\d)","RIL_\\3", rils$RILs)
rils$RILs <- sub("(13RV)(\\d+)","RIL_\\2", rils$RILs)
rils$RILs

setwd("~/git.repos/NAM_field/output/")                
write.table(rils,file="colsha_flowering_blups.csv",sep=",",row.names=FALSE) 

#format for RQTL
head(rils)
str(rils)

rils.t <- as.data.frame(t(rils))
head(rils.t)
dim(rils.t)
rils.t[9,] <- rils.t[1,]
head(rils.t)
tail(rils.t)
rownames(rils.t)[9] <- "id"
rownames(rils.t)
rils.t <- rils.t[-1,]
rils.t <- rils.t[-3,]
head(rils.t)
tail(rils.t)

setwd("~/git.repos/NAM_field/output/")   
write.table(rils.t,file="colsha_blups_field_rqtl_full.csv",sep=",",row.names=TRUE, col.names = FALSE) 

library(qtl)
?read.cross
setwd("~/git.repos/NAM_field/output/")   
colsha_traits <- read.cross("csvsr", genfile ="colsha_map.csv", 
                         phefile="colsha_blups_field_rqtl_full.csv",
                         genotypes=c("AA","BB"), na.strings = "NA")
head(colsha_traits)
class(colsha_traits)[1] <- "riself"
colsha_traits <- jittermap(colsha_traits)
colsha_traits

colsha_traits <- est.rf(colsha_traits)
plot.rf(colsha_traits) 
colsha_traits
newmap <- est.map(colsha_traits,verbose = T,error.prob = .01)
plot.map(colsha_traits,newmap) #some compression in this colsha_traits set

colsha_traits

colsha_traits <- replace.map(colsha_traits,newmap) #use new map
plot(colsha_traits) 
colsha_traits

colsha_traits <- sim.geno(colsha_traits,step=1,n.draws=64) 
#uses imputation
  #creates n.draw number of populations where the missing 
  #genotypes are filled in based on recombination frequencies

#just calculates the probabilities at different locations
colsha_traits <- calc.genoprob(colsha_traits, step = 1)
colsha_traits <- calc.genoprob(colsha_traits, error.prob=0.01)


# Map some traits
AM_branches.perm.imp.1 <- scanone(colsha_traits, method = "imp",
                              pheno.col = 1, n.perm = 1000) 
summary(AM_branches.perm.imp.1) 

perm95.1 <- summary(AM_branches.perm.imp.1)[1]
perm95.1

AM_branches.imp.1 <- scanone(colsha_traits, pheno.col = 1, method = "imp")
plot(AM_branches.imp.1, bandcol = "gray90")
abline(h=perm95.1,lty=2) 
# NOTHING FOR AM_BRANCHES

#
# Map some traits
germ2flr.perm.imp.1 <- scanone(colsha_traits, method = "imp",
                              pheno.col = 4, n.perm = 1000) 
summary(germ2flr.perm.imp.1) 

perm95.1 <- summary(germ2flr.perm.imp.1)[1]
perm95.1

germ2flr.imp.1 <- scanone(colsha_traits, pheno.col = 4, method = "imp")
plot(germ2flr.imp.1, bandcol = "gray90")
abline(h=perm95.1,lty=2) 
# Flowering TIME QTL



branch_tot.perm.imp.1 <- scanone(colsha_traits, method = "imp",
                              pheno.col = 6, n.perm = 1000) 
summary(branch_tot.perm.imp.1) 

perm95.1 <- summary(branch_tot.perm.imp.1)[1]
perm95.1

branch_tot.imp.1 <- scanone(colsha_traits, pheno.col = 6, method = "imp")
plot(branch_tot.imp.1, bandcol = "gray90")
abline(h=perm95.1,lty=2) 
# Total Branching QTL on CHR5 


Basal.perm.imp.1 <- scanone(colsha_traits, method = "imp",
                              pheno.col = 2, n.perm = 1000) 
summary(Basal.perm.imp.1) 

perm95.1 <- summary(Basal.perm.imp.1)[1]
perm95.1

Basal.imp.1 <- scanone(colsha_traits, pheno.col = 2, method = "imp")
plot(Basal.imp.1, bandcol = "gray90")
abline(h=perm95.1,lty=2) 
# NO SIG QTL



germ2bolt.perm.imp.1 <- scanone(colsha_traits, method = "imp",
                              pheno.col = 4, n.perm = 1000) 
summary(germ2bolt.perm.imp.1) 

perm95.1 <- summary(germ2bolt.perm.imp.1)[1]
perm95.1

germ2bolt.imp.1 <- scanone(colsha_traits, pheno.col = 3, method = "imp")
plot(germ2bolt.imp.1, bandcol = "gray90")
abline(h=perm95.1,lty=2) 
# Germ to bolting QTL on CHR5 






