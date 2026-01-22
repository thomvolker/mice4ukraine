
library(mice)
library(ggmice)
library(ggplot2)
library(purrr)
library(dplyr)

set.seed(123)

head(boys)

# Generate imputations
imp <- mice(boys)
imp

attributes(imp)
head(imp$data)

imp$imp

# Filling in the data
comp <- complete(imp, action = "all")
comp2 <- complete(imp, action = 2)
complong <- complete(imp, action = "long")

# no missing data
plot_pattern(comp2)

# iterations (convergence) and imputations (variance)
impm <- mice(boys, m = 10)
impit <- mice(boys, maxit = 7)

# check convergence
plot_trace(impit)
# common sources of non-convergence: ID variables, deterministic systems

# checking imputations
ggmice(impit, aes(x = age, y = hgt)) +
  geom_point()

ggmice(impit, aes(x = age, y = tv)) +
  geom_point()

ggmice(impit, aes(x = gen, y = tv)) +
  geom_jitter(width = 0.2)

ggmice(impit, aes(x = hgt, y = bmi)) +
  geom_point()

ggmice(impit, aes(x = wgt/(hgt/100)^2, y = bmi)) +
  geom_point()

# the method vector
methods(mice)
?mice

method <- impit$method
method <- make.method(boys)

method["bmi"] <- "~I(wgt/(hgt/100)^2)"

# When using passive imputation, also alter the predictors
pred <- impit$predictorMatrix
pred <- make.predictorMatrix(boys)

pred[c("hgt", "wgt"), "bmi"] <- 0
plot_pred(pred, method = method)

impit_new <- update(impit, method = method, predictorMatrix = pred)

plot_trace(impit)
plot_trace(impit_new)

impit_new <- mice.mids(impit_new, maxit = 13)
plot_trace(impit_new)

ggmice(impit_new, aes(x = wgt/(hgt/100)^2, y = bmi)) +
  geom_point()


# inferences with missing data

# two sample t-test
with(impit_new, t.test(hc[reg == "city"], hc[reg != "city"])) |>
  pool() # crashes


with(impit_new, lm(hc ~ reg == "city")) |>
  pool()
# what does this mean?
# estimate qbar = sum(q_i/m)
# total variance = ubar + b + b/m

with(impit_new, lm(hc ~ age + hgt + wgt + reg)) |>
  pool() |>
  summary()

# logistic regression
fit <- complete(impit_new, "all") |>
  map(\(x) mutate(x, phb3 = ifelse(phb %in% c("P1", "P2", "P3"), 0, 1))) |>
  map(\(x) glm(phb3 ~ age + bmi + tv, family = binomial(), data = x))

pool(fit)
summary((pool(fit)), exponentiate = TRUE)

# Optional: futuremice
# more information: https://www.gerkovink.com/miceVignettes/futuremice/Vignette_futuremice.html
fimp <- futuremice(boys, m = 50)
fimp <- futuremice(boys, m = 50, n.core = 10)




# Want to know more?
# Some additional resources in the slides.
# Summer school on multiple imputation: 
# https://utrechtsummerschool.nl/courses/data-science/data-science-solving-missing-data-problems-in-r

