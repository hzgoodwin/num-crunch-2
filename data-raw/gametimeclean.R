```{r}
library(tidyverse)
```
# Run Once
gametimedata <- read_csv("data-raw/gametimedata.csv")
View(gametimedata)
names(gametimedata) <- gametimedata[1,]
gametimedata <- gametimedata[-1,]
colnames(gametimedata)[10] <- "Location"

# Cleaning
gametimedata$Location[is.na(gametimedata$Location)] <- "home"
gametimedata$Location[gametimedata$Location == "@"] <- "away"

write.csv(gametimedata, "updated_gametime.csv")



