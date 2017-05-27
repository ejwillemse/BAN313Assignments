---
title       : Assignment 2 - Sensitivity analysis.
description : This assignment covers sensitivity analysis using R.




--- type:MultipleChoiceExercise lang:r xp:0 skills:1 key:8eaf7b9ebd
## Background

**Read the below instructions carefully.**

For this assignment you are required to perform sensitivity analysis on an inventory level simulation model.

For the model orders are placed by customers and filled from inventory. In parallel, products are manufactured and placed in inventory. A fixed number of products is produced per day, but not all products meet specifications. The total amount of sellable products produced is therefore random. Similarly, customer orders are also random.

For preparation first complete [Case study 11 - Sensitivity analysis on queueing models (Part II)](https://campus.datacamp.com/courses/industrial-analysis-using-r/12199).

Take note that the data used for this assignment is randomly generated and will change:

- each time the chapter is attempted; and
- when moving from one exercise to the next.

When completing the assignment, read all the available information and instructions _carefully_, and if necessary, review the applicable engineering statistics methods.

**DO NOT** jump straight to the instructions and skip the each questions' background information. The question backgrounds contain valuable information to assist you in completing the questions.

To continue with this chapter confirm the following:

*** =instructions

* I have read **ALL** the instructions on this page carefully.

*** =pre_exercise_code
```{r}

```

*** =sample_code
```{r}

```

*** =sct
```{r}
msg_success <- "Let's get started with the assignment."
test_mc(correct = 1, feedback_msgs = c(msg_success))
```

--- type:NormalExercise lang:r xp:200 skills:1 key:a5a83c84d5
## Question 1

To model inventory levels we first need to determine what distribution the two variables follow. Samples for both for 100 days are available in the `productionSample` data.frame.

For this question, fit an appropriate distribution for the number of products manufactured per day.

*** =instructions

1. View the `productionSample` data frame and decide which distribution the number of products manufactured per day follows. Thereafter conduct a $\chi^2$ goodness of fit test and report on the $p$-value of the test. Assign your answer to `p_production`. Hint: refer to [Case study 11 - Sensitivity analysis on queueing models (Part II)](https://campus.datacamp.com/courses/industrial-analysis-using-r/12199) to see how the $p$-value should be correctly retrieved.
2. View the `productionSample` data frame and decide which distribution the number of products ordered per day follows. Thereafter conduct a $\chi^2$ goodness of fit test and report on the $p$-value of the test. Assign your answer to `p_orders`.

*** =pre_exercise_code
```{r}
production_perDay <- round(rnorm(100, 150, 50), 0)
orders_perDay <- round(rnorm(100, 120, 50), 0)
productionSample <- data.frame(production_perDay, orders_perDay)
rm(production_perDay)
rm(orders_perDay)
```

*** =sample_code
```{r}

```

*** =solution
```{r}
goodnessFitNorm <- function(data)
{
  h <- hist(data)
  null.prob <- diff(pnorm(h$breaks, mean(data), sd(data)))
  results <- chisq.test(h$counts, p = null.prob, rescale.p = TRUE, simulate.p.value = TRUE)
  p_value <- results$p.value
  return(p_value)
}
p_production <- goodnessFitNorm(productionSample$production_perDay)
p_orders <- goodnessFitNorm(productionSample$orders_perDay)
```

*** =sct
```{r}
test_object("p_production", undefined_msg = "Something went wrong in calculating `p_production`.",
incorrect_msg = "Something went wrong in calculating `p_production`.")

test_object("p_orders", undefined_msg = "Something went wrong in calculating `p_orders`.",
incorrect_msg = "Something went wrong in calculating `p_orders`.")

success_msg("Correct!")
```

--- type:NormalExercise lang:r xp:400 skills:1 key:dec1c30a07
## Question 2

For this question you need to use the bootstrapping method to calculate 95% confidence intervals for the distribution parameters, as well as the expected values for the intervals. Similar to the previous exercises the sample data is available from the `productionSample` and `productionSample` dataframes.

*** =instructions

1. Use the bootstrapping method to calculate and view a 95% confidence interval and the expected value for mean products manufactured per day. Use 10000 bootstrap simulations and assign the interval to `productionMeanInterval` and the expected value to `meanProduction`.
2. Use the bootstrapping method to calculate and view a 95% confidence interval and the expected value for  the standard deviation of products manufactured per day. Use 10000 bootstrap simulations and assign the interval to `productionSdInterval` and the expected value to `sdProduction`.
3. Use the bootstrapping method to calculate and view a 95% confidence interval and the expected value for mean orders per day. Use 10000 bootstrap simulations and assign the interval to `orderMeanInterval` and the expected value to `meanOrder`.
4. Use the bootstrapping method to calculate and view a 95% confidence interval and the expected value for  the standard deviation of orders per day. Use 10000 bootstrap simulations and assign the interval to `orderSdInterval` and the expected value to `sdOrder`.

*** =pre_exercise_code
```{r}
production_perDay <- round(rnorm(100, 150, 50), 0)
orders_perDay <- round(rnorm(100, 120, 50), 0)
productionSample <- data.frame(production_perDay, orders_perDay)
rm(production_perDay)
rm(orders_perDay)
```

*** =sample_code
```{r}

```

*** =solution

```{r}
bootStrapFuncNorm <- function(data, boots = 10000)
{
  means <- rep(NA, boots)
  sds <- rep(NA, boots)
  n <- length(data)
  for (i in 1:boots)
  {
    s <- sample(data, size = n, replace = TRUE)
    means[i] <- mean(s)
    sds[i] <- sd(s)
  }
  return(list(means, sds))
}
prod <- bootStrapFuncNorm(productionSample$production_perDay)
productionMeanInterval <- quantile(prod[[1]], p = c(0.025, 0.975))
meanProduction <- mean(prod[[1]])
productionSdInterval <- quantile(prod[[2]], p = c(0.025, 0.975))
sdProduction <- mean(prod[[2]])

ord <- bootStrapFuncNorm(productionSample$orders_perDay)
orderMeanInterval <- quantile(ord[[1]], p = c(0.025, 0.975))
meanOrder <- mean(ord[[1]])
orderSdInterval <- quantile(ord[[2]], p = c(0.025, 0.975))
sdOrder <- mean(ord[[2]])
```

*** =sct
```{r}
test_object("productionMeanInterval", undefined_msg = "Something went wrong in calculating `productionMeanInterval`.",
incorrect_msg = "Something went wrong in calculating `productionMeanInterval`.")

test_object("meanProduction", undefined_msg = "Something went wrong in calculating `meanProduction`.",
incorrect_msg = "Something went wrong in calculating `meanProduction`.")

test_object("productionSdInterval", undefined_msg = "Something went wrong in calculating `productionSdInterval`.",
incorrect_msg = "Something went wrong in calculating `productionSdInterval`.")

test_object("sdProduction", undefined_msg = "Something went wrong in calculating `sdProduction`.",
incorrect_msg = "Something went wrong in calculating `sdProduction`.")

test_object("orderMeanInterval", undefined_msg = "Something went wrong in calculating `orderMeanInterval`.",
incorrect_msg = "Something went wrong in calculating `orderMeanInterval`.")

test_object("meanOrder", undefined_msg = "Something went wrong in calculating `meanOrder`.",
incorrect_msg = "Something went wrong in calculating `meanOrder`.")

test_object("orderSdInterval", undefined_msg = "Something went wrong in calculating `orderSdInterval`.",
incorrect_msg = "Something went wrong in calculating `orderSdInterval`.")

test_object("sdOrder", undefined_msg = "Something went wrong in calculating `sdOrder`.",
incorrect_msg = "Something went wrong in calculating `sdOrder`.")

success_msg("Correct!")
```

--- type:NormalExercise lang:r xp:100 skills:1 key:53546d675a
## Question 3

In the previous question we calculated parameter value ranges using the bootstrapping method. In this question we will perform one-factor-at-a-time sensitivity analysis on the mean products manufactured using the provided inventory simulation model that simulates inventory stock-outs.

Assume the following.

The mean and standard deviation for the products manufactured are:

* (152.69, 50.24)

The mean and standard deviation for the products ordered are:

* (126.79, 50.80)

Assume that the mean number of products ordered is within the following interval, consisting of the 2.5th, expected and 97.5th interval values:

* (143.08, 152.69, 162.47)

*** =instructions

1. Use the available simulation code and repeat the simulation 1000 times for `nDays = 30` days with a mean number of products manufactured of 143.08. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_low`.
2. Repeat the simulation 1000 times for `nDays = 30` days with a mean number of products manufactured of 152.69. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_expected`.
3. Repeat the simulation 1000 for `nDays = 30` days with a mean number of products manufactured of 152.69. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_high`.

*** =pre_exercise_code
```{r}



```

*** =sample_code
```{r}
inventorySimulation <- function(nDays, productionMean, productionSd, ordersMean, ordersSD)
{
  maxInventory <- 200
  startInventory <- 150
  startingInventory <- rep(NA, nDays + 1)
  endingInventory <- rep(NA, nDays)
  nStockOuts <- rep(NA, nDays)

  startingInventory[1] <- startInventory

  randomProductionPerDay <- round(rnorm(nDays, productionMean, productionSd), 0) # Complete this function
  randomOrdersPerDay <- round(rnorm(nDays, ordersMean, ordersSD), 0) # Complete this function

  for (i in 1:nDays)
  {
    randomProductionPerDay[i] <- max(0, randomProductionPerDay[i])
    randomOrdersPerDay[i] <- max(0, randomOrdersPerDay[i])
    if (startingInventory[i] < randomOrdersPerDay[i])
    {
      nStockOuts[i] <- 1
      endingInventory[i] <- 0
    }else{
      nStockOuts[i] <- 0
      endingInventory[i] <- startingInventory[i] - randomOrdersPerDay[i]
    }
    startingInventory[i + 1] <- min(maxInventory, endingInventory[i] + randomProductionPerDay[i])
  }
  nStockOuts <- sum(nStockOuts)
  return(nStockOuts)
}

```

*** =solution

```{r}
inventorySimulation <- function(nDays, productionMean, productionSd, ordersMean, ordersSD)
{
  maxInventory <- 200
  startInventory <- 150
  startingInventory <- rep(NA, nDays + 1)
  endingInventory <- rep(NA, nDays)
  nStockOuts <- rep(NA, nDays)

  startingInventory[1] <- startInventory

  randomProductionPerDay <- round(rnorm(nDays, productionMean, productionSd), 0) # Complete this function
  randomOrdersPerDay <- round(rnorm(nDays, ordersMean, ordersSD), 0) # Complete this function

  for (i in 1:nDays)
  {
    randomProductionPerDay[i] <- max(0, randomProductionPerDay[i])
    randomOrdersPerDay[i] <- max(0, randomOrdersPerDay[i])
    if (startingInventory[i] < randomOrdersPerDay[i])
    {
      nStockOuts[i] <- 1
      endingInventory[i] <- 0
    }else{
      nStockOuts[i] <- 0
      endingInventory[i] <- startingInventory[i] - randomOrdersPerDay[i]
    }
    startingInventory[i + 1] <- min(maxInventory, endingInventory[i] + randomProductionPerDay[i])
  }
  nStockOuts <- sum(nStockOuts)
  return(nStockOuts)
}

sensVals <- c(143.08, 152.69, 162.47)
sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, sensVals[1], 50.24, 126.79, 50.80)
}
medianSimStockout_low <- median(sim)

sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, sensVals[2], 50.24, 126.79, 50.80)
}
medianSimStockout_expected <- median(sim)

sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, sensVals[3], 50.24, 126.79, 50.80)
}
medianSimStockout_high <- median(sim)
```

*** =sct
```{r}
test_object("medianSimStockout_low", undefined_msg = "Something went wrong in calculate `medianSimStockout_low`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_low`.")

test_object("medianSimStockout_expected", undefined_msg = "Something went wrong in calculate `medianSimStockout_expected`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_expected`.")

test_object("medianSimStockout_high", undefined_msg = "Something went wrong in calculate `medianSimStockout_high`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_high`.")

success_msg("Correct!")
```

--- type:NormalExercise lang:r xp:100 skills:1 key:e4e2b433a6
## Question 4

In this question we will perform one-factor-at-a-time sensitivity analysis on the standard deviation of products manufactured using the provided inventory simulation model that simulates inventory stock-outs.

Assume the following.

The mean and standard deviation for the products manufactured are:

* (152.69, 50.24)

The mean and standard deviation for the products ordered are:

* (126.79, 50.80)

Assume that the standard deviation of the number of products manufactured is within the following interval, consisting of the 2.5th, expected and 97.5th interval values:

* (44.44, 50.24, 55.98)

*** =instructions

1. Use the available simulation code and repeat the simulation 1000 times for `nDays = 30` days with a standard deviation of products manufactured of 44.44. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_low`.
2. Repeat the simulation 1000 times for `nDays = 30` days with a standard deviation of products manufactured of 50.24. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_expected`.
3. Repeat the simulation 1000 for `nDays = 30` days with a standard deviation of products manufactured of 55.98. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_high`.

*** =pre_exercise_code
```{r}



```

*** =sample_code
```{r}
inventorySimulation <- function(nDays, productionMean, productionSd, ordersMean, ordersSD)
{
  maxInventory <- 200
  startInventory <- 150
  startingInventory <- rep(NA, nDays + 1)
  endingInventory <- rep(NA, nDays)
  nStockOuts <- rep(NA, nDays)

  startingInventory[1] <- startInventory

  randomProductionPerDay <- round(rnorm(nDays, productionMean, productionSd), 0) # Complete this function
  randomOrdersPerDay <- round(rnorm(nDays, ordersMean, ordersSD), 0) # Complete this function

  for (i in 1:nDays)
  {
    randomProductionPerDay[i] <- max(0, randomProductionPerDay[i])
    randomOrdersPerDay[i] <- max(0, randomOrdersPerDay[i])
    if (startingInventory[i] < randomOrdersPerDay[i])
    {
      nStockOuts[i] <- 1
      endingInventory[i] <- 0
    }else{
      nStockOuts[i] <- 0
      endingInventory[i] <- startingInventory[i] - randomOrdersPerDay[i]
    }
    startingInventory[i + 1] <- min(maxInventory, endingInventory[i] + randomProductionPerDay[i])
  }
  nStockOuts <- sum(nStockOuts)
  return(nStockOuts)
}

```

*** =solution

```{r}
inventorySimulation <- function(nDays, productionMean, productionSd, ordersMean, ordersSD)
{
  maxInventory <- 200
  startInventory <- 150
  startingInventory <- rep(NA, nDays + 1)
  endingInventory <- rep(NA, nDays)
  nStockOuts <- rep(NA, nDays)

  startingInventory[1] <- startInventory

  randomProductionPerDay <- round(rnorm(nDays, productionMean, productionSd), 0) # Complete this function
  randomOrdersPerDay <- round(rnorm(nDays, ordersMean, ordersSD), 0) # Complete this function

  for (i in 1:nDays)
  {
    randomProductionPerDay[i] <- max(0, randomProductionPerDay[i])
    randomOrdersPerDay[i] <- max(0, randomOrdersPerDay[i])
    if (startingInventory[i] < randomOrdersPerDay[i])
    {
      nStockOuts[i] <- 1
      endingInventory[i] <- 0
    }else{
      nStockOuts[i] <- 0
      endingInventory[i] <- startingInventory[i] - randomOrdersPerDay[i]
    }
    startingInventory[i + 1] <- min(maxInventory, endingInventory[i] + randomProductionPerDay[i])
  }
  nStockOuts <- sum(nStockOuts)
  return(nStockOuts)
}

sensVals <- c(44.44, 50.24, 55.98)
sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, sensVals[1], 126.79, 50.80)
}
medianSimStockout_low <- median(sim)

sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, sensVals[2], 126.79, 50.80)
}
medianSimStockout_expected <- median(sim)

sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, sensVals[3], 126.79, 50.80)
}
medianSimStockout_high <- median(sim)
```

*** =sct
```{r}
test_object("medianSimStockout_low", undefined_msg = "Something went wrong in calculate `medianSimStockout_low`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_low`.")

test_object("medianSimStockout_expected", undefined_msg = "Something went wrong in calculate `medianSimStockout_expected`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_expected`.")

test_object("medianSimStockout_high", undefined_msg = "Something went wrong in calculate `medianSimStockout_high`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_high`.")

success_msg("Correct!")
```

--- type:NormalExercise lang:r xp:100 skills:1 key:0ee6ed3212
## Question 5

In this question we will perform one-factor-at-a-time sensitivity analysis on the mean products ordered using the provided inventory simulation model that simulates inventory stock-outs.

Assume the following.

The mean and standard deviation for the products manufactured are:

* (152.69, 50.24)

The mean and standard deviation for the products ordered are:

* (126.79, 50.80)

Assume that the mean number of products ordered is within the following interval, consisting of the 2.5th, expected and 97.5th interval values:

* (114.72, 126.79, 132.85)

*** =instructions

1. Use the available simulation code and repeat the simulation 1000 times for `nDays = 30` days with a mean number of products ordered of 114.72. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_low`.
2. Repeat the simulation 1000 times for `nDays = 30` days with a mean number of products ordered of 126.79. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_expected`.
3. Repeat the simulation 1000 for `nDays = 30` days with a mean number of products ordered of 132.85. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_high`.

*** =pre_exercise_code
```{r}



```

*** =sample_code
```{r}
inventorySimulation <- function(nDays, productionMean, productionSd, ordersMean, ordersSD)
{
  maxInventory <- 200
  startInventory <- 150
  startingInventory <- rep(NA, nDays + 1)
  endingInventory <- rep(NA, nDays)
  nStockOuts <- rep(NA, nDays)

  startingInventory[1] <- startInventory

  randomProductionPerDay <- round(rnorm(nDays, productionMean, productionSd), 0) # Complete this function
  randomOrdersPerDay <- round(rnorm(nDays, ordersMean, ordersSD), 0) # Complete this function

  for (i in 1:nDays)
  {
    randomProductionPerDay[i] <- max(0, randomProductionPerDay[i])
    randomOrdersPerDay[i] <- max(0, randomOrdersPerDay[i])
    if (startingInventory[i] < randomOrdersPerDay[i])
    {
      nStockOuts[i] <- 1
      endingInventory[i] <- 0
    }else{
      nStockOuts[i] <- 0
      endingInventory[i] <- startingInventory[i] - randomOrdersPerDay[i]
    }
    startingInventory[i + 1] <- min(maxInventory, endingInventory[i] + randomProductionPerDay[i])
  }
  nStockOuts <- sum(nStockOuts)
  return(nStockOuts)
}

```

*** =solution

```{r}
inventorySimulation <- function(nDays, productionMean, productionSd, ordersMean, ordersSD)
{
  maxInventory <- 200
  startInventory <- 150
  startingInventory <- rep(NA, nDays + 1)
  endingInventory <- rep(NA, nDays)
  nStockOuts <- rep(NA, nDays)

  startingInventory[1] <- startInventory

  randomProductionPerDay <- round(rnorm(nDays, productionMean, productionSd), 0) # Complete this function
  randomOrdersPerDay <- round(rnorm(nDays, ordersMean, ordersSD), 0) # Complete this function

  for (i in 1:nDays)
  {
    randomProductionPerDay[i] <- max(0, randomProductionPerDay[i])
    randomOrdersPerDay[i] <- max(0, randomOrdersPerDay[i])
    if (startingInventory[i] < randomOrdersPerDay[i])
    {
      nStockOuts[i] <- 1
      endingInventory[i] <- 0
    }else{
      nStockOuts[i] <- 0
      endingInventory[i] <- startingInventory[i] - randomOrdersPerDay[i]
    }
    startingInventory[i + 1] <- min(maxInventory, endingInventory[i] + randomProductionPerDay[i])
  }
  nStockOuts <- sum(nStockOuts)
  return(nStockOuts)
}

sensVals <- c(114.72, 126.79, 132.85)
sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, 50.24, sensVals[1], 50.80)
}
medianSimStockout_low <- median(sim)

sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, 50.24, sensVals[2], 50.80)
}
medianSimStockout_expected <- median(sim)

sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, 50.24, sensVals[3], 50.80)
}
medianSimStockout_high <- median(sim)
```

*** =sct
```{r}
test_object("medianSimStockout_low", undefined_msg = "Something went wrong in calculate `medianSimStockout_low`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_low`.")

test_object("medianSimStockout_expected", undefined_msg = "Something went wrong in calculate `medianSimStockout_expected`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_expected`.")

test_object("medianSimStockout_high", undefined_msg = "Something went wrong in calculate `medianSimStockout_high`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_high`.")

success_msg("Correct!")
```

--- type:NormalExercise lang:r xp:100 skills:1 key:05f2fd368b
## Question 6

In this question we will perform one-factor-at-a-time sensitivity analysis on the standard deviation of products ordered using the provided inventory simulation model that simulates inventory stock-outs.

Assume the following.

The mean and standard deviation for the products manufactured are:

* (152.69, 50.24)

The mean and standard deviation for the products ordered are:

* (126.79, 50.80)

Assume that the standard deviation of the number of products ordered is within the following interval, consisting of the 2.5th, expected and 97.5th interval values:

* (40.51, 50.24, 51.30)

*** =instructions

1. Use the available simulation code and repeat the simulation 1000 times for `nDays = 30` days with a standard deviation of products ordered of 40.51. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_low`.
2. Repeat the simulation 1000 times for `nDays = 30` days with a standard deviation of products ordered of 50.24. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_expected`.
3. Repeat the simulation 1000 for `nDays = 30` days with a standard deviation of products ordered of 51.30. Assign the **median** of the simulated number of stock-outs to `medianSimStockout_high`.

*** =pre_exercise_code
```{r}



```

*** =sample_code
```{r}
inventorySimulation <- function(nDays, productionMean, productionSd, ordersMean, ordersSD)
{
  maxInventory <- 200
  startInventory <- 150
  startingInventory <- rep(NA, nDays + 1)
  endingInventory <- rep(NA, nDays)
  nStockOuts <- rep(NA, nDays)

  startingInventory[1] <- startInventory

  randomProductionPerDay <- round(rnorm(nDays, productionMean, productionSd), 0) # Complete this function
  randomOrdersPerDay <- round(rnorm(nDays, ordersMean, ordersSD), 0) # Complete this function

  for (i in 1:nDays)
  {
    randomProductionPerDay[i] <- max(0, randomProductionPerDay[i])
    randomOrdersPerDay[i] <- max(0, randomOrdersPerDay[i])
    if (startingInventory[i] < randomOrdersPerDay[i])
    {
      nStockOuts[i] <- 1
      endingInventory[i] <- 0
    }else{
      nStockOuts[i] <- 0
      endingInventory[i] <- startingInventory[i] - randomOrdersPerDay[i]
    }
    startingInventory[i + 1] <- min(maxInventory, endingInventory[i] + randomProductionPerDay[i])
  }
  nStockOuts <- sum(nStockOuts)
  return(nStockOuts)
}

```

*** =solution

```{r}
inventorySimulation <- function(nDays, productionMean, productionSd, ordersMean, ordersSD)
{
  maxInventory <- 200
  startInventory <- 150
  startingInventory <- rep(NA, nDays + 1)
  endingInventory <- rep(NA, nDays)
  nStockOuts <- rep(NA, nDays)

  startingInventory[1] <- startInventory

  randomProductionPerDay <- round(rnorm(nDays, productionMean, productionSd), 0) # Complete this function
  randomOrdersPerDay <- round(rnorm(nDays, ordersMean, ordersSD), 0) # Complete this function

  for (i in 1:nDays)
  {
    randomProductionPerDay[i] <- max(0, randomProductionPerDay[i])
    randomOrdersPerDay[i] <- max(0, randomOrdersPerDay[i])
    if (startingInventory[i] < randomOrdersPerDay[i])
    {
      nStockOuts[i] <- 1
      endingInventory[i] <- 0
    }else{
      nStockOuts[i] <- 0
      endingInventory[i] <- startingInventory[i] - randomOrdersPerDay[i]
    }
    startingInventory[i + 1] <- min(maxInventory, endingInventory[i] + randomProductionPerDay[i])
  }
  nStockOuts <- sum(nStockOuts)
  return(nStockOuts)
}

sensVals <- c(40.51, 50.24, 51.30)
sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, 50.24, 126.79, sensVals[1])
}
medianSimStockout_low <- median(sim)

sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, 50.24, 126.79, sensVals[2])
}
medianSimStockout_expected <- median(sim)

sim <- rep(NA, 1000)
for (i in 1:1000)
{
  sim[i] <- inventorySimulation(30, 152.69, 50.24, 126.79, sensVals[3])
}
medianSimStockout_high <- median(sim)
```

*** =sct
```{r}
test_object("medianSimStockout_low", undefined_msg = "Something went wrong in calculate `medianSimStockout_low`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_low`.")

test_object("medianSimStockout_expected", undefined_msg = "Something went wrong in calculate `medianSimStockout_expected`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_expected`.")

test_object("medianSimStockout_high", undefined_msg = "Something went wrong in calculate `medianSimStockout_high`.", incorrect_msg = "Something went wrong in calculate `medianSimStockout_high`.")

success_msg("Correct!")
```
