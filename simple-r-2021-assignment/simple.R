# specify any options

# Add any libraries
library(psych)

# Load any support functions

# load the data
rcases <- read.csv("data/ecases-at1.csv")
v <- list()

# clean/manipulate/prepare the data
rcases$honestyhumility <- (rcases$sincerity + rcases$fairness + rcases$greedavoidance + rcases$modesty) / 4
rcases$emotionality <- apply(rcases[,c("fearfulness", "anxiety", "dependence", "sentimentality")], 1, mean)
rcases$extraversion <- apply(rcases[,c("socialselfesteem", "socialboldness", "sociability", "liveliness")], 1, mean)
rcases$agreeableness <- apply(rcases[,c("forgiveness", "gentleness", "flexibility", "patience")], 1, mean)
rcases$conscientiousness<- apply(rcases[,c("organization",  "diligence", "perfectionism", "prudence")], 1, mean)
rcases$opennes <- apply(rcases[,c("aestheticappreciation",  "inquisitiveness", "creativity", "unconventionality")], 1, mean)

v$hexaco_facets <- c("sincerity", "fairness", "greedavoidance", 
"modesty", "fearfulness", "anxiety", "dependence", "sentimentality", 
"socialselfesteem", "socialboldness", "sociability", "liveliness", 
"forgiveness", "gentleness", "flexibility", "patience", "organization", 
"diligence", "perfectionism", "prudence", "aestheticappreciation", 
"inquisitiveness", "creativity", "unconventionality")
v$traitei <- c("overallei_allitems", "perceptions", "own",  "others", "utilization")
v$hexaco_domains <- c("honestyhumility",  "emotionality", "extraversion", "agreeableness",  "conscientiousness", "opennes")
v$hexaco_all <- c(v$hexaco_domains, v$hexaco_facets)

# run analyses
dput(names(rcases))
scree(rcases[,v$hexaco_facets])

fit <- factanal(rcases[,v$hexaco_facets], 6, rotation = "promax")
print(fit, cutoff = .3)

dput(names(rcases))

cormat <- round(cor(rcases[,v$hexaco_all], rcases[ v$traitei]), 2)
cormat

write.csv(cormat, file = "output/cormat.csv")


summary(lm(emotionality ~ gender, rcases))

t.test(honestyhumility ~ gender, rcases)
t.test(emotionality ~ gender, rcases)
t.test(extraversion ~ gender, rcases)

gentab <- data.frame(
    mean_male = sapply(rcases[rcases$gender == "male" ,v$hexaco_all], mean),
    sd_male = sapply(rcases[rcases$gender == "male" ,v$hexaco_all], sd),
    mean_female = sapply(rcases[rcases$gender == "female" ,v$hexaco_all], mean),
    sd_female = sapply(rcases[rcases$gender == "female" ,v$hexaco_all], sd))

gentab$d <- (gentab$mean_female - gentab$mean_male) / gentab$sd_male

round(gentab, 2)



