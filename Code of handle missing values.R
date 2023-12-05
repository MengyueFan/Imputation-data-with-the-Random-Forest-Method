dfm[dfm == "" | dfm == " "] <- NA # We assign all blank cell with 'NA'

dfm[sapply(dfm, is.character)] <- lapply(dfm[sapply(dfm, is.character)], 
                                         as.factor)

library('missForest')
library('foreach')

library('PRROC') # ROC curves in data frames
library('tidyverse') # everything
library('imputeMissings') # median/mode imputation
library('visdat') # missingness plots
library('doParallel') # parallel missForest
library('doRNG') # reproducible parallel results
library('gridExtra') # combining graphs
library('randomForest') # random forests
library('kableExtra') # pretty tables

options(scipen = 999) # no scientific notation

RNGkind(kind = "Mersenne-Twister", 
        normal.kind = "Inversion", 
        sample.kind = "Rejection") # because different versions of R giving different RNG results annoys me

theme_set(theme_light()) # ggplot theme

#Increasing Accuracy
registerDoParallel(cores = 4) # set based on number of CPU cores
registerDoRNG(seed = 1)
missForest_v1 <- missForest(dfm, parallelize = 'forests', verbose = T) 
# For verbose = T gave us that iterations is sufficient (performance-wise or accuracy-wise) 
# while the code is still running for a large dataset, 10 iterations seems good
# so we could just stop the process and re-run specifying maxiter = 10

registerDoRNG(seed = 1)
missForest_v2 <- missForest(dfm, parallelize = 'forests', maxiter = 10)
missForest_v2$OOBerror

mtry_used <- c()
OOB_NRMSE <- c()
OOB_PFC <- c()

i <- 1

for (loop_seed in 1:5) {
  for (mtry in 1:29) { 
    print(paste0("seed = ", loop_seed, ", mtry = ", mtry))
    
    registerDoRNG(seed = loop_seed)
    missForest_v3 <- missForest(dfm, 
                                parallelize = 'forests', 
                                ntree = 100, 
                                maxiter = 10, 
                                mtry = mtry)
    
    OOB_NRMSE[i] <- missForest_v3$OOBerror[1]
    OOB_PFC[i] <- missForest_v3$OOBerror[2]
    mtry_used[i] <- mtry
    
    i <- i + 1
  }
}


mtry_df <- data.frame(mtry_used, OOB_NRMSE, OOB_PFC) %>%
  group_by(mtry_used) %>%
  summarize(`OOB NRMSE` = mean(OOB_NRMSE), 
            `OOB PFC` = mean(OOB_PFC)) %>%
  gather("metric", "error", -mtry_used)

write.csv(mtry_df, "mtry_df.csv", row.names = F)

mtry_df <- read.csv("mtry_df.csv")

ggplot(mtry_df, aes(x = mtry_used, y = error, col = factor(metric))) + 
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = seq(2, 40, 2)) + 
  scale_color_manual(values = c("deepskyblue", "mediumseagreen")) +
  facet_wrap(~ metric, scales = "free_y") + 
  theme(legend.position = "none", 
        axis.title.y = element_blank()) + 
  labs(x = "mtry") # According to the graph, mtry = 29

# The imputation error can be improved by tuning the values of mtry and ntree parameter. 
# mtry refers to the number of variables being randomly sampled at each split. 
# ntree refers to number of trees to grow in the forest. 
# Above is that we try to increase accuracy by adjusting mtry.
# Below is that we try to increase accuracy by adjusting ntree.
# We tried to increase ntree quite significantly to 1000.

registerDoRNG(seed = 1)
missForest_v4 <- missForest(dfm, parallelize = 'forests', verbose = T, maxiter = 10, mtry = 29, ntree = 1000)

missForest_v1$OOBerror # check imputation error; 0.48
missForest_v2$OOBerror # check imputation error; 0.48
missForest_v3$OOBerror # check imputation error; 0.48

missForest_v4$OOBerror # check imputation error; 0.47

# The v4 was improved from 48.48% of missForest_v1$OOBerror by tuning the values of mtry and ntree parameter. 
categorical_vars <- dfm %>% select_if(is.factor) %>% names()

X_imp_cat <- missForest(dfm, 
                        mtry = 29, 
                        parallelize = 'forests', 
                        maxiter = 10)$ximp[ ,categorical_vars]

X_imp <- X_imp_cat
registerDoRNG(seed = 1)
missForest_v5 <- missForest(dfm, parallelize = 'forests', verbose = T, maxiter = 10, mtry = 29, variablewise = T, ntree = 1000)

data.frame(varname = names(missForest_v5$ximp), 
           error_type = names(missForest_v5$OOBerror), 
           error = missForest_v5$OOBerror) %>%
  mutate(error_type = cell_spec(error_type, color = "white", background = ifelse(error_type == "MSE", "deepskyblue", "mediumseagreen"))) %>%
  kable(escape = F) %>% 
  kable_styling(full_width = F)

# Above is the imputation error table for all variables.

missForest_v4$ximp[] <- lapply(missForest_v4$ximp, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(missForest_v4$ximp, class) # We change char type of data to Numeric type.

write.csv(missForest_v4$ximp,"~/Proactive Field Study after imputation.csv", row.names = F) 

