data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
plot(statedata$x, statedata$y)
#tapply which region of the US (West, North Central, South, or Northeast) has the highest average high school graduation rate
tapply(statedata$HS.Grad, statedata$state.region, mean)
#make a boxplot of murder rate by region
boxplot(statedata$Murder ~ statedata$state.region, data = statedata)
#outlier in the Northeast
subset(statedata, state.region == "Northeast", select = Murder)
#linear model of life expectancy ~ Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area
life_exp1 <- lm( Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata )
#plot(statedata$Income, statedata$Life.Exp)
plot(statedata$Income, statedata$Life.Exp)
# "backwards variable selection"
life_exp2 <- lm( Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata )
life_exp3 <- lm( Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata )
life_exp4 <- lm( Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)