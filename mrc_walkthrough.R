# 1. Install packages:
#    - untidydata
#    - plot3D
#    - xaringan
# 2. Load language_diversity dataset
# 3. Explore variables, tidy (long to wide)
# 4. Check normality, transform, plot
# 5. Fit model (MRC, 3 params)
# 6. Write up results
# 7. Convert to html presentation (xaringan)
# 8. Create github repo, make website



library(tidyverse)
library(ds4ling)
library(untidydata)
library(plot3D)

devtools::install_github("jvcasillas/ds4ling", force = T)

data(language_diversity)
glimpse(language_diversity)
head(language_diversity)

ld <- language_diversity %>% 
  filter(., Continent == "Africa") %>% 
  pivot_wider(names_from = "Measurement", values_from = "Value")

ld %>% 
ggplot(., aes(x = Population, y = Langs, color = Area, label = Country)) +
  geom_text() + 
  geom_smooth(method = lm)


my_mod <- lm(Langs ~ Area + Population, data = ld)
summary(my_mod)
plot(my_mod, which = 1:4)

ds4ling::diagnosis(my_mod)

ld <- ld %>% 
  mutate(., logPop = log(Population), 
            logArea = log(Area))

hist(ld$Population)
hist(ld$logPop)
hist(ld$Area)
hist(ld$logArea)



ld %>% 
ggplot(., aes(x = logPop, y = Langs, color = logArea, label = Country)) +
  geom_text() + 
  geom_smooth(method = lm)

# Fit a multiplicative model (number of languages as a function of lopPop and 
# logArea)

#this is the one we want! without interaction
log_mod_no_int <- lm(Langs ~ logPop + logArea, data = ld)
summary(log_mod_no_int)

log_mod <- lm(Langs ~ logPop * logArea, data = ld)
summary(log_mod)

diagnosis(log_mod)
diagnosis(log_mod_no_int)


# For fun
x <- ld$logPop
y <- ld$logArea
z <- ld$Langs

plot3D::scatter3D(x, y, z, 
    pch = 21, cex = 1, expand = 0.75, colkey = F,
    theta = 45, phi = 20, ticktype = "detailed",
    xlab = "logPop", ylab = "Area", zlab = "Langs")


