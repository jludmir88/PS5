rm(list=ls())
set.seed(12435)
options(stringsAsFactors=F)
library(foreign)

## read in data
anes <- read.dta("C:/Users/Joseph/Downloads/anes_timeseries_2012_stata12 (1).dta")
anes$gun_control <- ifelse(anes$gun_control == "1. More difficult", 
-1, ifelse(anes$gun_control == "3. Keep these rules about the same", 0, 
ifelse(anes$gun_control == "2. Easier", 1, NA)))
model_one <- lm(anes$ft_dpc ~ anes$gun_control, na.action = na.omit)
predict(model_one, data.frame(anes$gun_control))

anes$candrel_dpc <- ifelse(anes$candrel_dpc == "01. Protestant", 
3, ifelse(anes$candrel_dpc == "10. Christian {VOL}", 1, 
ifelse(anes$candrel_dpc == "02. Catholic", 0, 
ifelse(anes$candrel_dpc == "04. Muslim", -1, -3)))
model_two <- lm(anes$ft_dpc ~ anes$candrel_dpc)
predict(model_two, data.frame(anes$candrel_dpc))

anes$pctlikely_howlikvt2 <- ifelse(anes$pctlikely_howlikvt2 == "1. Extremely likely", 
2, ifelse(anes$pctlikely_howlikvt2 == "2. Very likely", 1, 
ifelse(anes$pctlikely_howlikvt2 == "3. Moderately likely", 0, 
ifelse(anes$pctlikely_howlikvt2 == "4. Slightly likely", -1, 
ifelse(anes$pctlikely_howlikvt2 == "5. Not likely at all", -2, NA)))))
model_three <- lm(anes$ft_dpc ~ anes$pctlikely_howlikvt2, na.action = na.omit)
predict(model_three, data.frame(anes$pctlikely_howlikvt2))
                

