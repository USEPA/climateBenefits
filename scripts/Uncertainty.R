## Example of getting model and sampling uncertainty

## Libraries
library(tidyverse)
library(MASS)

## Use Boston data set, for simplicity
data(Boston)
head(Boston)

## Linear model
full_lm = lm(medv ~ ., data = Boston)

## Doesn't really matter, but does the model fit?
preds = fitted(full_lm)
ggplot(data.frame('medv' = Boston$medv, 
                  'medv_fitted' = preds), 
       aes(x = medv, y = medv_fitted)) + 
  geom_point() +
  xlab('Median House Price') +
  ylab('Predicted House Price') +
  xlim(0,50) +
  ylim(0,50)

## Get a 'new' data set
n = 100
new_boston = MASS::mvrnorm(100, mu = apply(Boston, 2, mean),  
                           Sigma = diag(apply(Boston, 2, sd)/5))


## Two parts here:
# 1) Bootstrap for sampling uncertainty
# 2) Pull new coefficients to get model uncertainty 
n_sims = 1000
boot_list = NULL
for(i in 1:n_sims){
  # Bootstrap indices and temp data set
  boot_idx = sample(1:nrow(new_boston),nrow(new_boston),
                    replace = TRUE)
  boot_data = cbind(1, new_boston[boot_idx,-ncol(new_boston)])
  
  # Coefficients for temp model
  coefs = mvrnorm(1, mu = coef(full_lm), Sigma = vcov(full_lm))
  
  # Matrix multiplication - each row of boot_data gets multiplied by
  # the 1-dimensional vector of coefficients
  boot_preds = boot_data %*% coefs
  
  # We can add these up, or something else?
  boot_list = c(boot_list, sum(boot_preds))
  
  # Print some output
  if(i %% 50 == 0){
    print(paste('Done with', i,'iterations'))
  }
}

# Look at output
summary(boot_list)
# A confidence interval would be something like:
quantile(boot_list, c(0.025, 0.975))
