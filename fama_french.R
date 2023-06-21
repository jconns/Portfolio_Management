#### AUTO Stocks ####
autos <- c("TSLA", "GM", "ADR", "SPY") %>%
  tq_get(get = "stock.prices", from = "2010/12/1", to = "2023/5/31",
         periodicity='monthly')

### plot returns
autos
autos %>% 
  group_by(symbol) %>% 
  mutate(returns = ((adjusted - lag(adjusted))/lag(adjusted)) *100, na.rm=T) %>% 
  ggplot(aes(date, returns, color=symbol)) +
  geom_line() +
  labs(title="Auto Stocks Against the Market", caption="Source: Yahoo Finance",
       y="Returns", x="Date") +
  theme_minimal()

### cyclical and counter cyclical
library(glmnet)

### CAPM beta tesla 
summary(lm(autos$returns[autos$symbol=="TSLA"] ~ 
             autos$returns[autos$symbol=="SPY"], data=autos))

### CAPM beta GM
summary(glm(autos$returns[autos$symbol=="GM"] ~ 
              autos$returns[autos$symbol=="SPY"], data=autos))

### CAPM beta toyota
summary(lm(autos$returns[autos$symbol=="ADR"] ~ 
             autos$returns[autos$symbol=="SPY"], data=autos))

### fama - french
fama <- read.csv("/Users/jackconnors/Downloads/F-F_Research_Data_Factors-3.csv")
fama <- fama %>% 
  mutate(Date = ymd(paste0(date, "01")))

fama <- fama %>% 
  select(-date)
autos$Date <- autos$date

### merge autos & fama data frames
fama_full <- merge(fama, autos, by="Date")

fama_full <- fama_full %>% 
  select(-open, -high, -date, -low, -close, -volume, -na.rm) 

### convert all necessary columns into decimal format
fama_full <- fama_full %>% 
  mutate( dec_returns = returns / 100,
          mrk = MKT/100,
          smb = SMB,
          hml = HML/100,
          rf = RF/100,
          excess_mrk_returns = mrk - rf)

### excess equity returns
fama_full <- fama_full %>% 
  group_by(symbol)  %>% 
  mutate(excess_stock_returns = dec_returns - RF)


### compute fama-french cyclical for Tesla
summary(lm(excess_stock_returns[-1] ~ diff(SMB) + diff(HML) + diff(excess_mrk_returns), 
           data = subset(fama_full, symbol == "TSLA")))

### compute fama-french cyclical for GM
summary(lm(excess_stock_returns[-1] ~ diff(smb) + diff(hml) + diff(excess_mrk_returns), 
           data = subset(fama_full, symbol == "GM")))

### compute fama-french cyclical for ADR
summary(lm(excess_stock_returns[-1] ~ diff(smb) + diff(hml) + diff(excess_mrk_returns), 
           data = subset(fama_full, symbol == "ADR")))

### CAPM alpha
fama_full %>% 
  group_by(symbol) %>% 
  summarize(actual_return = mean(dec_returns, na.rm=T))

### tesla
tesla_alpha <- 5.15 - (1.69 * 8)

tesla_alpha

### GM 
gm_alpha <- 0.619 - (1.45 * 8)

gm_alpha

### toyota 
toyota_alpha <- -0.557 - (0.3846 * 8)

toyota_alpha

2+1.5*1
### fama french alpha calculation
tesla_alpha_fama <- 5.15 - (0.88 * 8)

tesla_alpha_fama

### GM 
gm_alpha_fama <- 0.619 - (0.15 * 8)

gm_alpha_fama

### toyota 
toyota_alpha_fama <- -0.557 - (0.1 * 8)

toyota_alpha_fama