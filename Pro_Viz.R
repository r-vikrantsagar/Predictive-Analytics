df = read.csv("train.csv")
library(dplyr)

df_1 <- df %>%
  select(-id, -Region_Code)
View(df_1)

#DATA VISUALIZATION

library(ggplot2)

#----Gender Distribution----
df %>%
  count(Gender) %>%
  ggplot(aes(x = Gender, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count")

#----Has a Driving License or Not----

# Plot count of Driving_License using ggplot2 with color and modified x-axis labels
ggplot(df, aes(x = factor(Driving_License), fill = factor(Driving_License))) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Count Plot of Driving License") +
  scale_x_discrete(labels = c("0" = "No License", "1" = "Has License")) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"))

# Display value counts of Driving_License
df %>%
  count(Driving_License)

#----Previously Insured or Not----

# Plotting the count using ggplot2 with specified fill colors
library(ggplot2)
ggplot(df, aes(x = factor(Previously_Insured))) +
  geom_bar(fill = c("0" = "red", "1" = "green")) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Count Plot of Previously Insured") +
  scale_x_discrete(labels = c("0" = "No Insurance", "1" = "Had Insurance"))

#----Age of Vehicle----

# Calculate counts
counts <- table(df$Vehicle_Age)

# Create a data frame for ggplot
df_pie <- data.frame(categories = names(counts), counts = as.numeric(counts))

# Define custom colors
custom_colors <- c("#66c2a5", "#fc8d62", "#8da0cb")

# Create a pie chart using ggplot2
ggplot(df_pie, aes(x = "", y = counts, fill = categories)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  scale_fill_manual(values = custom_colors) +
  theme(legend.position = "bottom") +
  labs(title = "Pie Chart of Vehicle Age", fill = "Vehicle Age") +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8))

#----vehicle Damage----

# Plotting the count using ggplot2 with custom colors and presentation
ggplot(df, aes(x = Vehicle_Damage, fill = Vehicle_Damage)) +
  geom_bar(color = "white", size = 0.7, show.legend = FALSE, fill = c("yes" = "coral1", "no" = "chartreuse4")) +
  labs(title = "Count Plot of Vehicle Damage")

#----Response----

ggplot(df, aes(x = Response, fill = Response)) +
  geom_bar(color = "white", size = 0.7, show.legend = FALSE, fill = c("0" = "#3498db", "1" = "#e74c3c")) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Count Plot of Response")

#----Boxplot for Age----

ggplot(df, aes(y = Age)) +
  geom_boxplot(fill = "lightblue", color = "steelblue") +
  labs(title = "Boxplot of Age")

#----Distribution of Age----

# Plotting the distribution using a histogram
hist(df$Age, breaks = 5, col = "skyblue", main = "Distribution of Age", xlab = "Age", ylab = "Frequency")

#----Boxplot for Annual Premium----

ggplot(df, aes(y = Annual_Premium)) +
  geom_boxplot(fill = "skyblue", color = "steelblue", width = 0.7) +
  labs(title = "Boxplot of Annual Premium")

#----Density plot for Annual Premium----

# Plotting the density using ggplot2
ggplot(df, aes(x = Annual_Premium, fill = Annual_Premium)) +
  geom_density(fill = "skyblue", color = "navy") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Density Plot of Annual Premium")

#----Boxplot for Vintage----

ggplot(df, aes(y = Vintage)) +
  geom_boxplot(fill = "skyblue", color = "steelblue", width = 0.7) +
  labs(title = "Boxplot of Vintage")

#Data visualization - Relationshio between variables and interaction terms

#----Response vs Age----

ggplot(df, aes(x = Response, y = Age, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Bar Plot of Age by Response")

#----Response vs Annual Premium----

ggplot(df, aes(x = factor(Response), y = Annual_Premium)) +
  geom_boxplot(fill = "#66c2a5", color = "#1f78b4") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Bar Plot of Response vs. Annual Premium")

#----Response vs Vintage----

ggplot(df, aes(x = factor(Response), y = Vintage)) +
  geom_boxplot(fill = "#66c2a50", color = "#e41a1c") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Box Plot of Response vs. Vintage")

#----Response by Annual Premium and Driving License----

ggplot(df, aes(x = factor(Response), y = Annual_Premium, fill = factor(Driving_License))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Bar Plot of Annual Premium by Response and Driving License",
       x = "Response",
       y = "Annual Premium",
       fill = "Driving License")

#----Count Plot of Vehicle Damage by Response----

ggplot(df, aes(x = Vehicle_Damage, fill = Response)) +
  geom_bar(position = "dodge", color = "white", size = 0.7, show.legend = TRUE) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Count Plot of Vehicle Damage by Response")

#----Count Plot of Previously Insured with Response----

# Plotting the count using ggplot2 with custom colors and presentation
ggplot(df, aes(x = Previously_Insured, fill = interaction(Previously_Insured, Response))) +
  geom_bar(position = "dodge", color = "white", size = 0.7, show.legend = TRUE) +
  scale_fill_manual(values = custom_colors, name = "Response") +
  labs(title = "Count Plot of Previously Insured with Response")
