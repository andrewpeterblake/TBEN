## "Multiple multiple regressions"
##
## This program reads the McCracken and Ng FRED-MD dataset then
##  - Transforms it as suggested
##  - Unit root tests before and after the transform 
##  - Fits AR models to all the series and ploys summary statistics/coefficients
## 

# URL for information: 
browseURL("https://research.stlouisfed.org/econ/mccracken/fred-databases/")
#
# download.file("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/current.csv", "current.csv")
#
# download.file("https://files.stlouisfed.org/files/htdocs/uploads/FRED-MD%20Appendix.zip", "FRED-MD Appendix.zip", mode = "wb")
# unzip("FRED-MD Appendix.zip")
#

###################################################

library(tidyverse)
library(lubridate)
library(broom)       # To 'tidy' dataframes
library(readr)

library(aTSA)        # Unit roots 

###################################################
##
## Data
##
###################################################

# Groups are just numeric, need to assign names
gnames <- tibble(group = 1:8, 
                 Group = c("Output and income", "Labor market", "Housing", 
                            "Consumption, orders and inventories", "Money and credit", 
                            "Interest and exchange rates", "Prices", "Stock market"))

# Read meta data from auxiliary file - this is so we can group the data
meta <- read_csv("FRED-MD Appendix/FRED-MD_updated_appendix.csv", col_types = "iicccci")  %>%
  left_join(gnames) %>% 
  select(fred, tcode, Group) %>% 
  rename(FRED = fred) %>% 
  mutate(FRED = if_else(FRED == "IPB51222s", toupper(FRED), FRED)) # Lower case "s" needs fixing
  
# Read Data file twice to get the names and then the data and assign names as second row is transforms
McNames <- read_csv("current.csv", n_max=1, show_col_types=FALSE)
McNN    <- read_csv("current.csv", skip=4, show_col_types=FALSE, col_names=FALSE) %>% 
  rename_all(~ names(McNames))

# Sort out date, drop missing, put in group names
Data <- McNN %>%
  mutate(sasdate = as.Date(sasdate, format="%m/%d/%Y")) %>%
  rename(Date    = sasdate) %>%
  filter(!is.na(Date)) %>% 
  pivot_longer(cols = -Date, names_to = "FRED", values_to = "Value") %>% 
  left_join(meta, by="FRED")

# Plot raw data by group
ggplot(Data) + 
  geom_line(aes(x=Date, y=Value, group=FRED, color=FRED), show.legend=FALSE) +
  facet_wrap(~Group, scales = "free_y") + 
  theme_minimal() +
  labs(title="Untransformed data by group", x="", y="")

############################################################
## Apply transforms
############################################################

DataS <- Data %>%
  group_by(FRED) %>%
  mutate(Value = case_when(tcode==1 ~ Value,
                           tcode==2 ~ Value-lag(Value,1), 
                           tcode==3 ~ Value-2*lag(Value,1)+lag(Value,2),
                           tcode==4 ~ log(Value),
                           tcode==5 ~ log(Value)-log(lag(Value,1)), 
                           tcode==6 ~ log(Value)-2*log(lag(Value,1))+log(lag(Value,2)), 
                           tcode==7 ~ Value/lag(Value,1)-lag(Value,1)/lag(Value,2)) ) %>%
  slice(-(1:3)) %>%
  mutate(Value = scale(Value)) %>% 
  ungroup() %>%
  mutate(tcode=as.factor(tcode))

# Plot transformed data in groups
ggplot(DataS) + 
  geom_line(aes(x=Date, y=Value, group=FRED, color=tcode), show.legend=TRUE) +
  facet_wrap(~ Group, scales="free_y") +
  theme_minimal() +
  labs(title="All transformed series by group", x="", y="")

grp <- unique(DataS$Group)
# Plot transformed data by groups
for (i in 1:length(grp)) {
  p <- DataS %>% 
    filter(Group==grp[i]) %>% 
    ggplot() + 
    geom_line(aes(x=Date, y=Value, group=FRED, color=tcode), show.legend=TRUE) +
    facet_wrap( ~ FRED) +
    theme_minimal() +
    labs(title = grp[i], x="", y="")
  plot(p)
}

#####################################################################################
## Test for stationarity
#####################################################################################

n <- 6
m <- 3  # Model 1 - nothing, 2 - drift, 3 - trend

# Two (identical) ur tests on a single series of transformed data
adf  <- adf.test(filter(DataS, FRED=="RPI")$Value, (n+1), output=TRUE) # Easy access to p-values
udf  <- urca::ur.df(filter(DataS, FRED=="RPI")$Value, lags=n, type="trend")  # Offers AIC/BIC selection
udf2 <- urca::ur.df(filter(DataS, FRED=="RPI")$Value, lags=n, selectlags = "AIC", type="trend")  # 

# Will use aTSA::adf.test as we can get the pvalues easily
tests1 <- Data %>% 
  group_by(FRED, Group) %>% 
  summarise(p_adf = adf.test(Value, n, output=FALSE)[[m]][n,3]) 

ggplot(tests1) + 
  geom_hline(yintercept=0.05, linetype=5, alpha=.3) + 
  geom_jitter(aes(x=Group, y=p_adf, colour=FRED), show.legend = FALSE) + 
  theme_minimal() + 
  coord_flip() +
  ylim(0,1) +
  labs(title=paste0("ADF", m, " tests, lag=",n," p-value"), x="", y="")

## Retest the transformed data for stationarity. 

tests2 <- DataS %>% 
  group_by(FRED, Group) %>% 
  summarise(p_adf = adf.test(Value, n, output=FALSE)[[m]][n,3]) 

ggplot(tests2) + 
  geom_hline(yintercept=0.05, linetype=5, alpha=.3) + 
  geom_jitter(aes(x=Group, y=p_adf, colour=FRED), show.legend = FALSE) + 
  theme_minimal() + 
  coord_flip() +
  ylim(0,1) +
  labs(title=paste0("ADF", m, " tests, lag=",n," p-value"), x="", y="")

##########################################################################
## Fit an autoregressive model to every series, and retrieve some stats
##########################################################################

# AR(2) in each series
AR <- function(df) { 
  lm(Value ~ lag(Value,1) + lag(Value,2), data = df) 
  }

estimates <- DataS %>% 
  group_by(FRED, Group) %>% 
  nest() %>%                                      # Wrap all data into a list
  mutate(model  = map(data, AR)) %>%              # map AR function we wrote to data
  mutate(cvals  = map(model, coefficients)) %>%   # Export coefficients
  mutate(glance = map(model, glance))             # Get some stats, using broom

stats <- estimates %>% 
  unnest(glance)

## Let's look at $R^2$s for the Stock Market group
stats %>% 
  filter(Group == "Stock market") %>% 
  select(FRED, Group, r.squared) 

stats %>% 
  ggplot() + 
  geom_jitter(aes(x=Group, y=r.squared, colour=FRED), show.legend = FALSE) + 
  theme_minimal() + 
  coord_flip() +
  ylim(0,1) +
  labs(title="R-squared by Group", x="", y="")

## Histograms of AR coefficients

# Names
cnames <- c("alpha", paste0("beta[",1:2,"]"))

DS <- stats %>% 
  select(FRED, Group, cvals) %>% 
  mutate(Group = gsub(",", "", Group), Group = gsub(" ", "_", Group)) %>%  # Messes up with commas so need to fix
  unnest(cvals) %>% 
  group_by(FRED) %>%
  mutate(coef=cnames) 

DS %>% 
  ggplot() +
  geom_vline(xintercept=0.0, linetype=5, alpha=.3) + 
  geom_histogram(aes(x=cvals, fill=coef), alpha=.55, bins=25, show.legend=FALSE) +
  facet_grid(Group~coef, scales="free_x", labeller=label_parsed) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(title="AR(2) coefficients by group", x="", y="")

DS %>% 
  pivot_wider(names_from = coef, values_from = cvals) %>%  
  mutate(`sum(beta)` = `beta[1]` + `beta[2]`) %>% 
  pivot_longer(cols=-c(FRED, Group), names_to = "coef", values_to = "cvals") %>% 
  ggplot() +
  geom_vline(xintercept=0.0, linetype=5, alpha=.3) + 
  geom_histogram(aes(x=cvals, fill=coef), alpha=.55, bins=50, show.legend=FALSE) +
  facet_grid(Group~coef, scales="free", labeller = label_parsed) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(title="AR(2) coefficients by group", x="", y="")
