install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)
library("palmerpenguins")

par(mfrow = c(1, 2))
boxplot(penguins$bill_length_mm)
boxplot(bill_length_mm ~ sex, data = penguins)

require(here)
png(filename = here("coplot.png"), width = 800, height = 600)
coplot(body_mass_g ~ bill_depth_mm | island, data = penguins)
dev.off()

