---
title       : Assignment 1 - Introduction to data, probability, and inference.
description : This assignment covers the first three chapters of Industrial Analysis using R, which deals with introduction to data, basic probability theory, and inference for categorical and numerical data.




--- type:NormalExercise lang:r xp:0 skills:1 key:a5a83c84d5
## Instructions

We work for a fast-food chain and have been instructed to analyse the performance of one of our most successful outlets. The outlet sells five main products:

1. Burger
2. Pizza
3. Fries
4. Cool-drink
5. Ice-cream

As well as two combo-meals, which is a main (either a burger or pizza) with fries and a cool-drink:

6. Burger combo
7. Pizza combo

Using the above information and the provided instructions, answer and submit the following questions in DataCamp. Also copy and paste your final code from the code editor for each question into the "Assignment 1B: datacamp backup answers" test on clickUP. Failure to do so may result in a final mark of *ZERO* for the assignment.

When answering the question, use the data loaded in your R workspace. *DO NOT* use the short-question data files.

Please take note that the data used for this assignment is randomly generated and will change:

- each time the assignment is attempted; and
- when moving from one exercise to the next.

There are no 'hints' available for the questions.

When completing the assignment, read the instructions _carefully_, and if necessary, review the applicable engineering statistics methods.

To continue with this chapter, hit the 'Submit Answer' button.

*** =instructions

- Hit the 'Submit Answer' button when you're done reading the instructions.

*** =hint
Just hit the 'Submit Answer'.

*** =pre_exercise_code
```{r}
#none

```

*** =sample_code
```{r}

```

*** =solution
```{r}
#none
```

*** =sct
```{r}
success_msg("Answer the following questions.")
```


--- type:NormalExercise lang:r xp:100 skills:1 key:f643a67dfc
## Hypothesis test for independence

In January 2017 a pilot was launched whereby staff aggressively marketed the combo to customers who only ordered a pizza or a burger. Staff were tasked to randomly target a client who only ordered a pizza or a bigger (therefore not each client was targeted) and to point-out that they will save money by taking the combo meal. Whether or not a client then upgraded his meal to a combo-meal was captured.

Results for this pilot can be found in the `comboUpgradeJan17` data-frame.

Based on the available data, does it seem that the probability of a combo-meal being upgraded is dependent or independent from the type of meal being ordered? Perform a hypothesis test to formally test this using an alpha level of 0.01. Use normal-distribution when completing the test. 

The hypotheses for the test are as follow:

* H0: The proportion of pizza orders that were upgraded to pizza-combos is the same as the proportion of burger orders that were upgraded to burger combos.
* HA: The proportion of pizza orders that were upgraded to pizza-combos is not the same as the proportion of burger orders that were upgraded to burger combos.

*** =instructions

1. Calculate the proportion of pizzas that were upgraded and assign your answer to `pPizzaUpgrade`.
2. Calculate the proportion of burgers that were upgraded and assign your answer to `pBurgerUpgrade`.
3. Calculate the pooled proportion of pizzas and burger that were upgraded and assign your answer to `pPooled`.
4. Calculate the Standard Error for hypothesis test and assign your answer to `SE`.
5. Calculate the T-score for the hypothesis test and assign your answer to `Z-score` (not that this part will not be marked, only the next part).
6. Calculate the p-value for the hypothesis test and assign your answer to `p_value`.
7. Use the p-value and the alpha value of 0.01 to conclude if the null-hypothesis should be rejected and assign you answer, which should be either `TRUE` for reject H0 and `FALSE` for do not reject H0, to `rejectH0`.

*** =hint

There are no hints available.

*** =pre_exercise_code
```{r}
genFastFoodSales <- function()
{
  nItems = round(runif(1, 5000, 10000), 0)
  food_items <- c('burger', 'pizza')
  foodItemSold <- sample(food_items, nItems, replace = TRUE)
  
  invoiceNumber <- paste('#', round(runif(nItems, 100000, 999999),0), sep= "")
  fastFoodSales <- data.frame(invoiceNumber, foodItemSold)
  fastFoodSales <- fastFoodSales[order(fastFoodSales$invoiceNumber),]
  return(fastFoodSales)
}

salesJan <- genFastFoodSales()
salesJanPizza <- subset(salesJan, foodItemSold == "pizza")
comboUpgradeAccepted <- sample(c(TRUE, FALSE), nrow(salesJanPizza), replace = TRUE, prob = c(0.25, 0.75))
salesJanPizza$comboUpgradeAccepted = comboUpgradeAccepted

salesJanBurger <- subset(salesJan, foodItemSold == "burger")
comboUpgradeAccepted <- sample(c(TRUE, FALSE), nrow(salesJanBurger), replace = TRUE, prob = c(0.45, 0.55))
salesJanBurger$comboUpgradeAccepted = comboUpgradeAccepted

comboUpgrade <- rbind(salesJanBurger, salesJanPizza)
comboUpgradeSample <- sample(1:nrow(comboUpgrade), size = 200)
comboUpgrade <- comboUpgrade[comboUpgradeSample,]

comboUpgradeJan17 <- comboUpgrade[order(comboUpgrade$invoiceNumber),][1:200,]
rm(salesJan)
rm(genFastFoodSales)
rm(salesJanPizza)
rm(comboUpgradeAccepted)
rm(salesJanBurger)
rm(comboUpgrade)
rm(comboUpgradeSample)
```

*** =sample_code
```{r}
# 1. Calculate the proportion of pizzas that were upgraded and assign your answer to `pPizzaUpgrade`.

pPizzaUpgrade <-

# 2. Calculate the proportion of burgers that were upgraded and assign your answer to `pBurgerUpgrade`.

pBurgerUpgrade <-

# 3. Calculate the pooled proportion of pizzas and burger that were upgraded and assign your answer to `pPooled`.

pPooled <-

# 4. Calculate the Standard Error for hypothesis test and assign your answer to `SE`.

SE <-

# 5. Calculate the T-score for the hypothesis test and assign your answer to `T_score` (not that this part will not be marked, only the next part).

T_score <-

# 6. Calculate the p-value for the hypothesis test and assign your answer to `p_value`.

p_value <-

# 7. Use the p-value and the alpha value of 0.01 to conclude if the null-hypothesis should be rejected and assign you answer, which should be either `TRUE` for reject H0 and `FALSE` for do not reject H0, to `rejectH0`.

rejectH0 <-

```

*** =solution
```{r}
# solution

salesTable <- table(comboUpgradeJan17$foodItemSold) #asdfe@###441
salesUpgradePropTable <- prop.table(table(comboUpgradeJan17$foodItemSold, comboUpgradeJan17$comboUpgradeAccepted),1)

nBurgerMcd = as.numeric(salesTable['burger'])
nPizzasHuts = as.numeric(salesTable['pizza'])

pBurgerUpgrade = salesUpgradePropTable[1,2]
pPizzaUpgrade = salesUpgradePropTable[2,2] #4334@39913
pPooled = as.numeric((table(comboUpgradeJan17$comboUpgradeAccepted)/nrow(comboUpgradeJan17))['TRUE'])

tenSuccess = pPooled*nPizzasHuts > 10 & pPooled*nBurgerMcd > 10
tenFailure = (1-pPooled)*nPizzasHuts > 10 & (1-pPooled)*nBurgerMcd > 10
successFailureCondition = tenSuccess & tenFailure

SE = sqrt(pPooled*(1-pPooled)/nPizzasHuts + pPooled*(1-pPooled)/nBurgerMcd)
T_score_ejw = (pPizzaUpgrade - pBurgerUpgrade)/SE
dfreedomss = min(nPizzasHuts - 1, nBurgerMcd - 1) #*&@34werwe

if (T_score_ejw > 0){p_value = 2*pnorm(T_score_ejw, lower.tail = FALSE)}else{p_value = 2*pnorm(T_score_ejw, lower.tail = TRUE)}

rejectH0 <- p_value < 0.01
```

*** =sct
```{r}
test_object("pPizzaUpgrade", undefined_msg = "Make sure to calculate the proportion of pizzas that were upgraded and assign your answer to `pPizzaUpgrade`.", incorrect_msg = "Make sure to calculate the proportion of pizzas that were upgraded and assign your answer to `pPizzaUpgrade`. Assign only the numeric value to `pPizzaUpgrade`.")

test_object("pBurgerUpgrade", undefined_msg = "Make sure to calculate the proportion of burgers that were upgraded and assign your answer to `pBurgerUpgrade`.", incorrect_msg = "Make sure to calculate the proportion of burgers that were upgraded and assign your answer to `pBurgerUpgrade`. Assign only the numeric value to `pBurgerUpgrade`.")

test_object("pPooled", undefined_msg = "Make sure to calculate the pooled proportion upgrades and assign your answer to `pPooled`.", incorrect_msg = "Make sure to calculate the pooled proportion upgrades and assign your answer to `pPooled`. Assign only the numeric value to `pPooled`.")
            
test_object("SE", undefined_msg = "Make sure to calculate the Standard Error and assign your answer to `SE`.",
            incorrect_msg = "Make sure to calculate the Standard Error and assign your answer to `SE`. What could have possibly gone wrong is that you used the wrong formula (check when and when not to use the pooled proportion), or you calculated the number of samples per group incorrectly.")
            
test_object("p_value", undefined_msg = "Make sure to calculate the p-value and assign your answer to `p_value`.",
            incorrect_msg = "Make sure to calculate the p-value and assign your answer to `p_value`. What could have possibly gone wrong is that you incorrectly calculated the Z-score, or you incorrectly calculate `p-value` which depends on whether it's a double sided hypothesis test and whether Z-score is positive or negative.")
            
test_object("rejectH0", undefined_msg = "Make sure to define a variable `rejectH0`.",
            incorrect_msg = "Make sure that you correctly assigned the `TRUE` or `FALSE` value to `rejectH0`. Refer to the prescribed textbook on how to determine if we can reject (`TRUE`) or not reject (`FALSE`) the null hypothesis based on alpha.")

success_msg("Correct! Remember to copy and paste your answer from the editor into the `Assignment 1B: datacamp backup answers` test on clickUP.")
```



--- type:NormalExercise lang:r xp:100 skills:1 key:2188ad509e
## Confidence interval for proportions

Calculate a 97% confidence interval for the true proportion of _burgers_ that will be upgraded to a combo-meal.

Results for the pilot can be found in the `comboUpgradeJan17` data-frame.

*** =instructions

1. Calculate the Standard Error for the Confidence Interval and assign your answer to `SE`.
2. Calculate the Margin of Error for the 97% Confidence Interval and assign your answer to `ME`.
3. Calculate the lower value of the Confidence Interval and assign your answer to `CI_low`.
4. Calculate the upper value of the Confidence Interval and assign your answer to `CI_high`.

*** =hint

There are no hints available.

*** =pre_exercise_code
```{r}
genFastFoodSales <- function()
{
  nItems = round(runif(1, 5000, 10000), 0)
  food_items <- c('burger', 'pizza')
  foodItemSold <- sample(food_items, nItems, replace = TRUE)
  
  invoiceNumber <- paste('#', round(runif(nItems, 100000, 999999),0), sep= "")
  fastFoodSales <- data.frame(invoiceNumber, foodItemSold)
  fastFoodSales <- fastFoodSales[order(fastFoodSales$invoiceNumber),]
  return(fastFoodSales)
}

salesJan <- genFastFoodSales()
salesJanPizza <- subset(salesJan, foodItemSold == "pizza")
comboUpgradeAccepted <- sample(c(TRUE, FALSE), nrow(salesJanPizza), replace = TRUE, prob = c(0.25, 0.75))
salesJanPizza$comboUpgradeAccepted = comboUpgradeAccepted

salesJanBurger <- subset(salesJan, foodItemSold == "burger")
comboUpgradeAccepted <- sample(c(TRUE, FALSE), nrow(salesJanBurger), replace = TRUE, prob = c(0.45, 0.55))
salesJanBurger$comboUpgradeAccepted = comboUpgradeAccepted

comboUpgrade <- rbind(salesJanBurger, salesJanPizza)
comboUpgradeSample <- sample(1:nrow(comboUpgrade), size = 200)
comboUpgrade <- comboUpgrade[comboUpgradeSample,]

comboUpgradeJan17 <- comboUpgrade[order(comboUpgrade$invoiceNumber),][1:200,]
rm(salesJan)
rm(genFastFoodSales)
rm(salesJanPizza)
rm(comboUpgradeAccepted)
rm(salesJanBurger)
rm(comboUpgrade)
rm(comboUpgradeSample)
```

*** =sample_code
```{r}
# 1. Calculate the Standard Error for the Confidence Interval and assign your answer to `SE`.

SE <- 

# 2. Calculate the Margin of Error for the 97% Confidence Interval and assign your answer to `ME`.

ME <-

# 3. Calculate the lower value of the Confidence Interval and assign your answer to `CI_low`.

CI_low <-

# 4. Calculate the upper value of the Confidence Interval and assign your answer to `CI_high`.

CI_high <-

```

*** =solution
```{r}
salesTable <- table(comboUpgradeJan17$foodItemSold) #asdfe@###441
salesUpgradePropTable <- prop.table(table(comboUpgradeJan17$foodItemSold, comboUpgradeJan17$comboUpgradeAccepted),1)

nBurgerMcd = as.numeric(salesTable['burger'])
pBurgerUpgrade = salesUpgradePropTable[1,2]

CI_level = 0.97

SE = sqrt(pBurgerUpgrade*(1-pBurgerUpgrade)/nBurgerMcd)
df = nBurgerMcd - 1

ME = abs(qnorm((1-CI_level)/2, lower.tail = TRUE))*SE
CI = c(pBurgerUpgrade - ME, pBurgerUpgrade + ME)
CI_low = CI[1]
CI_high = CI[2]
```

*** =sct
```{r}
test_object("SE", undefined_msg = "Make sure to calculate the Standard Error and assign your answer to `SE`.",
            incorrect_msg = "Make sure to calculate the Standard Error and assign your answer to `SE`. What could have possibly gone wrong is that you used the wrong formula (check when and when not to use the pooled proportion), or you calculated the number of samples incorrectly.")
            
test_object("ME", undefined_msg = "Make sure to calculate the margin of error and assign your answer to `ME`.",
            incorrect_msg = "Make sure to calculate the margin of error and assign your answer to `ME`. What could have possibly gone wrong is that you incorrectly calculated the critical z* value, or your ME is negative whereas it should always be positive.")
            
test_object("CI_low", undefined_msg = "Make sure to calculate the lower confidence interval value and assign your answer to `CI_low`.",
            incorrect_msg = "Make sure to calculate the lower confidence interval value and assign your answer to `CI_low`.")

test_object("CI_high", undefined_msg = "Make sure to calculate the higher confidence interval value and assign your answer to `CI_high`.",
            incorrect_msg = "Make sure to calculate the higher confidence interval value and assign your answer to `CI_high`.")

success_msg("Correct! Remember to copy and paste your answer from the editor into the `Assignment 1B: datacamp backup answers` test on clickUP.")
```

--- type:NormalExercise lang:r xp:100 skills:1 key:0b9e31a47e
## Hypothesis test for numerical data

We wish to analyse the burger-making process, which management is unhappy with. The time taken to complete a burger was observed for a random sampled of ordered burgers. The results of which can be found in the `burgerProcessTimes` data-frame.

Burger making consists of four processes. The time taken to complete each process, in seconds, was captured for each burger in the sample and can be found in the frame.

An external trainer claims that his specialised training will result in a mean processing time of the patty preparation to about 58 seconds. Management has indicated that if there is sufficient evidence that the current process takes longer than 59 seconds, we should consider hiring the trainer.

Is there sufficient evidence using an alpha = 0.01 level to suggest that the current patty preparation time is more than 59 seconds?

The hyptheses are:

* H0: The current patty preparation process takes on average 59 seconds.
* HA: The current patty preparation process takes on average more than 59 seconds.

*** =instructions

1. Calculate the sample mean and standard deviation of the patty preperation process and assign your answers to the `x` and `sdev`.
2. Calculate the Standard Error for the hypothesis test and assign your answer to `SE`.
3. Calculate the T-score for the hypothesis test and assign your answer to `T_score`.
4. Calculate the p-value for the hypothesis test and assign your answer to `p_value`.
5. Use the p-value and the alpha value of 0.01 to conclude if the null-hypothesis should be rejected and assign you answer, which should be either `TRUE` for reject H0 and `FALSE` for do not reject H0, to `rejectH0`.
6. Confirm the results of your calculations using the `t.test()` function.

*** =hint

There are no hints available.

*** =pre_exercise_code
```{r}
nSamples <- round(runif(1, 200, 400),0)
sampleNumber <- c(1:nSamples)
preparePatty <- round(abs(rnorm(nSamples, mean = 60, sd = 10)), 0)
prepareBun <- round(abs(rnorm(nSamples, mean = 20, sd = 3)), 0)
assembleBurger <- round(abs(rnorm(nSamples, mean = 40, sd = 8)), 0)
packageBurger <- round(abs(rnorm(nSamples, mean = 15, sd = 3)), 0)

burgerProcessTimes <- data.frame(sampleNumber, preparePatty, prepareBun, assembleBurger, packageBurger)

randomOutlierRow <- round(runif(1, 1, nSamples), 0)
randomOutlierColumn <- 5
burgerProcessTimes[randomOutlierRow,randomOutlierColumn] = round(abs(rnorm(1, mean = 400, sd = 20)), 0)
```

*** =sample_code
```{r}
# 1. Calculate the sample mean and standard deviation of the patty preperation process and assign your answers to the `x` and `sd`.

x <-
sdev <-

# 2. Calculate the Standard Error for the hypothesis test and assign your answer to `SE`.

SE <-

# 3. Calculate the T-score for the hypothesis test and assign your answer to `T_score`.

T_score <-

# 4. Calculate the p-value for the hypothesis test and assign your answer to `p_value`.

p_value <-

# 5. Use the p-value and the alpha value of 0.01 to conclude if the null-hypothesis should be rejected and assign you answer, which should be either `TRUE` for reject H0 and `FALSE` for do not reject H0, to `rejectH0`.

rejectH0 <-

# 6. Confirm the results of your calculations using the `t.test()` function.



```

*** =solution
```{r}
burgerProcessTimesPreparePatty = burgerProcessTimes$preparePatty
x = mean(burgerProcessTimesPreparePatty)
sd = sd(burgerProcessTimesPreparePatty)
n = nrow(burgerProcessTimes)

SE = sd/sqrt(n)
T_score = (x - 59)/SE

df = n - 1

p_value <- pt(T_score, df, lower.tail = FALSE)
rejectH0 <- p_value < 0.01
rejectH0

t.test(x = burgerProcessTimesPreparePatty, mu = 59, alternative = "greater")
```

*** =sct
```{r}
test_object("x", undefined_msg = "Make sure to calculate the mean patty preperation process time and assign your answer to  `x`.",
            incorrect_msg = "Make sure to calculate the mean patty preperation process time and assign your answer to  `x`.")

test_object("x", undefined_msg = "Make sure to calculate the standard deviation of the patty preperation process time and assign your answer to  `sdev`.",
            incorrect_msg = "Make sure to calculate the standard deviation of the patty preperation process time and assign your answer to  `sdev`.")
            
test_object("SE", undefined_msg = "Make sure to define a variable `SE`.",
            incorrect_msg = "Make sure that you calculated the standard error of the sample mean price differences correctly and assigned your answer to `SE`. Refer to the prescribed textbook for the correct standard error formula to use.")
            
test_object("T_score", undefined_msg = "Make sure to define a variable `T_score`.",
            incorrect_msg = "Make sure that you calculated the T-score of the sample mean price differences correctly and assigned your answer to `T_score`.")
            
test_object("p_value", undefined_msg = "Make sure to define a variable `p_value`.",
            incorrect_msg = "Make sure that you calculated the p-value correctly and assigned your answer to `p_value`.")

test_object("rejectH0", undefined_msg = "Make sure to define a variable `rejectH0`.",
            incorrect_msg = "Make sure that you correctly assigned the `TRUE` or `FALSE` value to `rejectH0`. Refer to the prescribed textbook on how to determine if we can reject (`TRUE`) or not reject (`FALSE`) the null hypothesis based on alpha.")
            
test_function('t.test', args = c("x", "mu", "alternative"), not_called_msg = "How about using the `t.test` function to check your answer?", args_not_specified_msg = "You need to specify certain input arguments for the function. Have a look at the documentation and figure out which input arguments to use. You also need to set the input arguments to the correct value inside the function call. For example, `t.test(x = variableA ...)`", incorrect_msg = "Some of the input arguments that you specified for the function is incorrect. Have a look at the documentation and figure out which input arguments to use. For example, `t.test(x = variableA ...)`")

success_msg("Correct! Remember to copy and paste your answer from the editor into the `Assignment 1B: datacamp backup answers` test on clickUP.")
```

