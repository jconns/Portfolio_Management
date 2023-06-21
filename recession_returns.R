### Dot-Com Bubble recession 1 = March - November 2001
### expected returns for dot com recession
portfolio %>%
  filter(date >= as.Date("2001-03-01") & date <= as.Date("2001-12-01")) %>%
  group_by(symbol) %>%
  summarise(expected_returns = mean(returns, na.rm = TRUE))

### sd for dot com recession
portfolio %>%
  filter(date >= as.Date("2001-03-01") & date <= as.Date("2001-12-01")) %>%
  group_by(symbol) %>%
  summarise(risk = sd(returns, na.rm = TRUE))

### dotcom correlation
dotcom_correlation <- portfolio %>%
  filter(symbol %in% c("SPY", "VUSTX")) %>% 
  filter(date >= as.Date("2001-03-01") & date <= as.Date("2001-12-01"))

cor(dotcom_correlation$returns[!is.na(dotcom_correlation$returns) & 
                                 dotcom_correlation$symbol=="SPY"],
    dotcom_correlation$returns[!is.na(dotcom_correlation$returns) & 
                                 dotcom_correlation$symbol=="VUSTX"])

### 2008 Great Recession Dec 2007 - March 2009 ###
### expected returns 
portfolio %>%
  filter(date >= as.Date("2007-11-30") & date <= as.Date("2009-04-01")) %>%
  group_by(symbol) %>%
  ggplot(aes(date, returns, color=symbol))+
  geom_line() +
  labs(title="Great Financial Crisis", caption = "Source: Yahoo Finance",
       y="Returns", x="Date") +
  theme_minimal()

summarise(expected_returns = mean(returns, na.rm = TRUE))

### ggplot add on
ggplot(aes(date, returns, color=symbol))+
  geom_line() +
  labs(title="Great Financial Crisis", caption = "Source: Yahoo Finance",
       y="Returns", x="Date") +
  theme_minimal()

### sd for great recession recession
portfolio %>%
  filter(date >= as.Date("2007-12-01") & date <= as.Date("2009-03-31")) %>%
  group_by(symbol) %>%
  summarise(risk = sd(returns, na.rm = TRUE))

### great recession correlation
great_correlation <- portfolio %>%
  filter(date >= as.Date("2007-12-01") & date <= as.Date("2009-03-31"))

cor(great_correlation$returns[!is.na(great_correlation$returns) & 
                                great_correlation$symbol=="SPY"],
    great_correlation$returns[!is.na(great_correlation$returns) & 
                                great_correlation$symbol=="VUSTX"])

### COVID Recession Feb 2020 - Now ###
### expected returns 
portfolio %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2023-05-29")) %>%
  group_by(symbol) %>%
  summarise(expected_returns = mean(returns, na.rm = TRUE))

### sd for covid recession
portfolio %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2023-05-29")) %>%
  group_by(symbol) %>%
  summarise(risk = sd(returns, na.rm = TRUE))

### covid correlation
covid_correlation <- portfolio %>%
  filter(symbol %in% c("SPY", "VUSTX")) %>% 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2023-05-29"))

cor(covid_correlation$returns[!is.na(covid_correlation$returns) & 
                                covid_correlation$symbol=="SPY"],
    covid_correlation$returns[!is.na(covid_correlation$returns) & 
                                covid_correlation$symbol=="VUSTX"])
