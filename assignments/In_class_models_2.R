data(iris)

fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)

summary(fit_species)

plot(
  Petal.Width ~ Petal.Length,
  data = iris,
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

summary(fit_petals)

model_resids = residuals(fit_species)

shapiro.test(model_resids)


                 