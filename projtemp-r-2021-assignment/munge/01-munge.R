# Data manipulations
# Highlight and run command below do load project without munging.
# This is useful when debugging data manipulation code.
# rm(list = ls()); library(ProjectTemplate); load.project(list(munging=FALSE)) 

# It is often useful to create sets of variable names
# By convention, the list that stores these variable sets is called 'v'
# v <- list()

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
