library(readr)
library(tibble)
library(dplyr)
library(ggplot2)

# Import data sets into R studio

consumption <- read_delim("C:/Users/Nikhila Mariam Biju/Desktop/datamining/New folder/HH_SAMPLE_POWER_CONSUMPTION_GRID_A.csv",";",na = "empty", trim_ws = TRUE)

summary(consumption)

hh_spc <- tibble(month_billing = consumption$MONTH_BILLING,

# Create a new table or tibble with the name hh_spc
                 swt_reason = consumption$SWITCHING_REASON,
                 cust_decl_kwh = consumption$DECLARATION_CONSUMP_CUST,
                 grid_decl_kwh = consumption$DECLARATION_CONSUMP_CUST + consumption$CORRECTIVE_VALUE,
                 bill = consumption$BILLING_TYPE,
                 inv_kwh_365 = round(consumption$CONSUMPTION_INVOICED * 365 / consumption$DAYS_INVOICED,digits= 0))

# Limit the selection to:
#   inv_kwh_365 < 10000
#   cust_decl_kwh < 7500
#   cust_decl_kwh > 0

hh_spc <- filter(hh_spc,inv_kwh_365 < 10000, cust_decl_kwh < 7500, cust_decl_kwh > 0)

# Order by month_billing

hh_spc <- arrange(hh_spc, month_billing)

# Plot a sample of 400 of these point & including smoothing function
#   Compare customers' declared consumption and (invoiced) 365 day consumption
#   Separate switching reason by color
#   Use se = FALSE for the smoothing


ggplot(data = sample_n(tbl=hh_spc, size=400),aes(x = cust_decl_kwh,y = inv_kwh_365,color = swt_reason)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  xlim(0,10000) + 
  ylim(0,10000)



# Create a boxplot with switching reason and 365 day consumption by
# using a 10% sample

ggplot(data = sample_frac(tbl=hh_spc, size=0.1),aes(x = swt_reason, y = inv_kwh_365)) +
  geom_boxplot()
