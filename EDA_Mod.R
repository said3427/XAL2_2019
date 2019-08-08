library(tidyverse)
library(gapminder)
library(modelr)

head(gapminder)


gapminder %>%
  ggplot(aes(year,lifeExp, group=country))+
  geom_line(alpha= .33)

mx <- filter(gapminder, country == "Mexico")

mx_mod <- lm(lifeExp ~ year , data = mx)

mx %>%
  ggplot(aes(year,lifeExp))+
  geom_line()+
  ggtitle("Full data = ")

mx %>%
  add_predictions(mx_mod)%>%
  ggplot(aes(year,pred))+
  geom_line()+
  ggtitle("Linear trend + ")
  
mx %>%
  add_residuals(mx_mod)%>%
  ggplot(aes(year,resid))+
  geom_hline(yintercept = 0, color = "white", size = 3)+
  geom_line()+
  ggtitle("Remaining Pattern")


mx %>%
  add_residuals(mx_mod)%>%
  ggplot(aes(year,resid))+
  geom_hline(yintercept = 0, color = "white", size = 3)+
  geom_point(aes(color =  "red"))+
  ggtitle("Remaining Pattern")

by_country <- gapminder %>%
  group_by(country,continent) %>%
  nest()

head(by_country)

by_country$data[1] #afghanistan

country_model <- function(df){
  lm(lifeExp ~ year, data = df )
}

by_country <- by_country %>%
  mutate(model = map(data,country_model))
by_country

by_country <- by_country %>%
  mutate(resids = map2(data,model,add_residuals))
by_country


resids <- unnest(by_country,resids)
resids

resids %>%
  ggplot(aes(year,resid))+
  geom_line(aes(group = country), alpha = .33)+
  geom_smooth(se = F)

resids %>%
  ggplot(aes(year,resid, group = country)) + 
  geom_line(alpha = .33) +
  facet_wrap(~continent)

glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = T)
glance

glance %>%
  arrange(r.squared)

glance %>%
  ggplot(aes(continent, r.squared))+
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year,lifeExp, color = country)) +
  geom_line()

EUAvMX <- filter(gapminder, (country == "United States") | (country == "Mexico"))
head(EUAvMX)

EUA_mod <- lm(lifeExp ~ year , data = EUAvMX , subset = country == "United States" )

EUAvMx %>%
  ggplot(aes(year,lifeExp, group = country, color = country))+
  geom_line()

broom::tidy(summary(mx_mod))
broom::tidy(summary(EUA_mod))
