# MET-Analysis
Example code for basic mixed model analysis along with LSD calculation. Single location analysis considers block and multi year effects as random, with genotype as fixed effect. Multi location analysis is similar, with yearlocation and 
nested block effect as random effects. LSD calcualtion is custom and does not have multiple correction. Multi location analysis is friendly for users who cannot combine data from trials before hand, instead users drop
all files into the same sub folder and load in together in R.  
The most current two stage analysis accepts trial mean output from a Genovix server which allows
researchers to preserve trial results across locations in final summaries more consistently.
