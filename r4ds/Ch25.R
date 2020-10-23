library(modelr)
library(tidyverse)

library(gapminder)
gapminder

gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country

by_country$data[[1]]

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)
models

by_country <- by_country %>%
  mutate(model = map(data, country_model))

by_country %>%
  filter(continent == "Europe")

by_country %>%
  arrange(continent, country)

by_country <- by_country %>%
  mutate(resids = map2(data, model, add_residuals)
  )
by_country
         

resids <- unnest(by_country, resids)
resids

resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

resids %>%
  ggplot(aes(year, resid, group = country)) + 
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~ continent)

broom::glance(nz_mod)

by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

glance %>%
  arrange(r.squared)

glance %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.3)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color= country)) +
  geom_line()

data.frame(x = list(1:3, 3:5))
data.frame(
  x = I(list(1:3, 3:5)),
  y = c("1, 2", "3, 4, 5")
)

tibble(
  x = list(1:3, 3:5),
  y = c("1, 2", "3, 4, 5")
)


tribble(
  ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
)
 

gapminder %>%
  group_by(country, continent) %>%
  nest()

gapminder %>%
  nest(year:gdpPercap)

df <- tribble(
  ~x1,
  "a,b,c",
  "d,e,f,g"
)

df %>%
  mutate(x2 = stringr::str_split(x1, ",")) %>%
  unnest()
