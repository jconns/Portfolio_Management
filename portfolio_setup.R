#### final part 1

#### 1.	Using Yahoo Finance and time period since 1997/1/1 until 
##2023/5/31, calculate weekly net rates of returns (aka returns) 
##of stocks and bonds. Present them to the client in such a form that 
##they will understand the main differences between stock and bond 
##returns (compare average returns and standard deviations, plot 
##time series of these returns, etc.)

library(tidyquant)
library(quantmod)
library(ggplot2)
library(dplyr)
library(tsibble)
library(xts)

#### 1
port <- c("SPY", "VUSTX") %>%
  tq_get(get = "stock.prices", from = "1997/1/1", to = "2023/5/31",
         periodicity='weekly')
head(port)
t_bill <- read.csv("/Users/jackconnors/Downloads/^IRX (1).csv")

t_bill <- t_bill %>% 
  mutate(date = ymd(Date),
         symbol="BIL") %>% 
  rename(adjusted=Adj.Close) %>% 
  as_tibble()

portfolio <- bind_rows(t_bill, port)

## 10 year graph
stock_bonds <- portfolio %>% 
  filter(symbol %in% c("SPY", "VUSTX")) %>% 
  group_by(symbol) %>% 
  mutate(returns = ((adjusted -lag(adjusted))/lag(adjusted)) 
         *100, na.rm=T) %>% 
  ggplot(aes(date, returns, color=symbol)) +
  geom_line() +
  labs(title="Weekly Returns", caption="Source: Yahoo Finance",
       y="Returns", x="Date") +
  theme_minimal()

## weekly returns of all assets
### cement returns as a column in portfolio data frame
portfolio <- portfolio %>% 
  group_by(symbol) %>% 
  mutate(returns = ((adjusted -lag(adjusted))/lag(adjusted)) 
         *100, na.rm=T) 
ggplot(aes(date, returns, color=symbol)) +
  geom_line() +
  labs(title="Weekly Returns for Stocks & Bonds", 
       caption="Source=Yahoo Finance",
       y="Returns", x="Date") +
  theme_minimal()

### 3
### is MPT applicable to these portfolio?
### minimized portfolio
portfolio <- portfolio %>% 
  select(symbol, date, adjusted, returns)

### standard deviation of returns per asset
portfolio %>% 
  group_by(symbol) %>% 
  summarize(sd = sd(returns, na.rm=T))

### expected returns
### standard deviation of returns per asset
portfolio %>% 
  group_by(symbol) %>% 
  summarize(expected_returns = mean(returns, na.rm=T))

### risk premium
risk_premium = .188 - 0.665

### correlation btwn stocks & bonds
correlation <- cor(portfolio$returns[!is.na(portfolio$returns) &
                                       portfolio$symbol =="SPY"],
                   portfolio$returns[!is.na(portfolio$returns) &
                                       portfolio$symbol =="VUSTX"])


correlation