library('vegan')
library('ggvegan')

group <- read.csv('./Groups.txt', sep = '\t', check.names = F, row.names = 1)
env <- read.csv('./Env.txt', sep = '\t', check.names = F, row.names = 1)

for (disName in c('bray_curtis', 'unweighted_unifrac', 'weighted_unifrac')) {
  print(disName)
  disMat <- read.csv(paste('./Distance/', disName, '.txt', sep = ''), sep = '\t', check.names = F, row.names = 1)
  
  adonisResult <- adonis(as.dist(disMat) ~ ., env, permutations = 9999)
  write.table(adonisResult$aov.tab, file = paste('./ADONIS/', disName, '.txt', sep = ''), quote = F, sep = '\t')
  
  anosimResult <- with(group, anosim(as.dist(disMat), GroupID, permutations = 9999))
  anosimResultDF <- data.frame(
    'Method' = c('ANOSIM'),
    'Sample Size' = c(length(group$GroupID)),
    'Number of Groups' = c(length(levels(group$GroupID))),
    'R statistics' = c(anosimResult$statistic),
    'P value' = c(anosimResult$signif),
    'Permutations' = c(anosimResult$permutations),
    check.names = F
  ) 
  write.table(anosimResultDF, file = paste('./ANOSIM/', disName, '.txt', sep = ''), quote = F, sep = "\t")
  
  p <- autoplot(anosimResult, notch = F)
  ggsave(paste('./ANOSIM/', disName, '.pdf', sep = ''), p, width = 150, height = 100, units = 'mm')
  ggsave(paste('./ANOSIM/', disName, '.png', sep = ''), p, width = 150, height = 100, units = 'mm', dpi = 300)
}

