library("stringr")
library("rjags")
redfin <- read.csv("./test_1/redfin.csv")
View(redfin)
redfin1 <- redfin %>% 
  filter(date_sold>=as.Date("2024-05-17") & date_sold<=as.Date("2024-11-17")) %>%
  mutate(price=property.price) %>% 
  mutate(buyerfee=ifelse(date_sold>=as.Date("2024-08-17"), TRUE, FALSE)) %>% 
  mutate(logprice=log(price), logsqft=log(sqft)) %>%
  select(price, logprice, sqft, logsqft, beds, baths, buyerfee, interestcut)

### -------------------------------------------------------#
# Exploratory analysis
### -------------------------------------------------------#
# dependent variable ~ independent variables
plot(redfin1)
boxplot(price~buyerfee, data=redfin1)
boxplot(price~interestcut, data=redfin1)
# multicollinearity
boxplot(sqft~beds, data=redfin1)
table(redfin1$buyerfee, redfin1$interestcut)

# linear regression
lm_res <- lm(logprice ~ sqft + buyerfee, data=redfin1)
summary(lm_res)

### -------------------------------------------------------#
# Linear regression
# price ~ sqft + buyerfee + interestcut
### -------------------------------------------------------#
dat <- redfin1 %>% 
  select(price, sqft, buyerfee) %>%
  mutate(buyerfee = as.factor(buyerfee))
  
mod_string = " model {
    for (i in 1:length(price)) {
        price[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*sqft[i] + b[2]*buyerfee[i] 
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    b[1] ~ dnorm (1000.0, 1.0/1.0e6)
    b[2] ~ dnorm (0.0, 1.0/1.0e6)

    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(25)
data_jags = as.list(dat)
params <- c("b0", "b","sig")

mod <- jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)

### Run the model to get the posterior distribution of the parameters -----------------------#
update(mod, 1000)
mod.sim <- coda.samples(model = mod, variable.names = params, n.iter=5000)
mod.simc <- do.call(rbind, mod.sim)

### -------------------------------------------------------#
# Diagnostic
### -------------------------------------------------------#
### Diagnostics to check the autocorrelation and stableness of the chain#
plot(mod.sim)
autocorr.plot(mod.sim)
effectiveSize(mod.sim)
traceplot(mod.sim)
gelman.diag(mod.sim)


### Diagnositcs to check the assumptions ------------------#
pmean <- colMeans(mod.sim[[1]])[1:3]
x <- cbind(data_jags$sqft, data_jags$buyerfee, rep(1, length(data_jags$sqft)))
yhat <- x %*% pmean
resid <- data_jags$price - yhat

##### Linearity: scatter plot of x and y (yhat and residual, because y = yhat + residual) ----
plot(data_jags$price, yhat)
##### Independence: scatter plot of residual ----
plot(resid) 
##### Normality and equal variance given covariates: QQ plot, residual plot vs. yhat ----
qqnorm(resid) # left skewed
plot(yhat, resid) # variance larger with larger house value

### -------------------------------------------------------#
# Inference
### -------------------------------------------------------#
summary(mod.sim)
plot(mod.sim[[1]][,2])
sum(mod.sim[[1]][1:5000, 2]>0)/5000


