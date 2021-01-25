# Load important libraries
library(dplyr)
library(ggplot2)

# Dataset about average income for people working in agriculture
df <- read.csv("agriculture_dataset.csv")

## Clean the data, remove Nulls and NaN

df <- df[!sapply(df, is.null)]
df_cleaned <- df[2:34] %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

df_cleaned <- cbind(df$Financial.Year, df_cleaned)
names(df_cleaned)[1] <- "Financial.Year"

summary(df_cleaned)
rm(df)

# Plot 1

# df_cleaned %>% select(where(~contains("Average"))) -> male_data
df_cleaned %>% select(starts_with("Men")) -> male_data
df_cleaned %>% select(starts_with("Women")) -> female_data
df_cleaned %>% select(starts_with("Children")) -> children_data

data <- data.frame(
  fin_year = df_cleaned$Financial.Year,
  avg_earning_male = rowMeans(male_data),
  avg_earning_female = rowMeans(female_data),
  avg_earning_children = rowMeans(children_data)
)

colors <- c("Male Earning"="red", "Female Earning"="blue", "Children Earning"="green")
ggplot(data=data, aes(x=fin_year, group=1)) +
        geom_line(aes(y=avg_earning_male, color="Male Earning")) +
        geom_line(aes(y=avg_earning_female, color="Female Earning")) +
        geom_line(aes(y=avg_earning_children, color="Children Earning")) +
        labs(title="Agriculture Sector Disparity in Earning based on Gender and Age ",
             x="Financial Year",
             y="Average Earning",
             color="Legend") + 
        scale_color_manual(values=colors) +
        theme(axis.text.x = element_text(angle = 90))

rm(data, children_data, male_data, female_data)


## Plot 2

# Extract columns with 
columns <- df_cleaned %>% select(contains("Average"))

avg_incomes <- c(mean(columns$Men..Average.Agriculture.Wage.Rate), 
                 mean(columns$Women..Average.Agricultural.Wage.Rate), 
                 mean(columns$Children..Average.Agricltural.Wage.Rate)
                 )

labels <- c("Male Average", "Female Average", "Children Average")
piepercent <- round(100*avg_incomes/sum(avg_incomes), 1)

pie(avg_incomes, labels=piepercent, main="Comparison of Average Incomes of Different Working Groups",col=topo.colors(3))
legend("topright", labels, cex=0.8, fill=topo.colors(3))
    

## Plot 3

df3 <- data.frame(
  years = df_cleaned$Financial.Year,
  incomes = c(df_cleaned$Women..Average.Agricultural.Wage.Rate, df_cleaned$Men..Average.Agricultural.Wage.Rate),
  gender = c(rep("FEMALE", 16), rep("MALE",16))
)

brks <- c(seq(30,200, 50), seq(30,200,50))
lbls <- paste0(as.character(c(seq(30,200, 50), seq(30,200, 50))), "/-")

ggplot(df3, aes(x=years, y=incomes, fill=gender)) +
  geom_bar(stat="identity", width=0.6) + 
  scale_y_continuous(breaks=brks, labels=lbls) +
  coord_flip() +
  labs(title="Yearwise comparison of Incomes", x="Incomes", y="Years") +
  theme(plot.title = element_text(hjust=0.5),axis.ticks = element_blank()) + 
  scale_fill_brewer(palette = "Dark2")



## Plot 4


## Plot 5