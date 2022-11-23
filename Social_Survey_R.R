# Survey Data was collected from students and family members of the authors.
# Rapid data-cleaning was done manually in excel due to small dataset

# All packages used and loaded
install.packages('ggpubr')
install.packages('rlang')
install.packages('ggplot2')
install.packages('dplyr')
library(ggplot2)
library(dplyr)
library(rlang)
library(ggpubr)

#Setting the working directory
getwd()
setwd('C:/Users/Alex/Documents/Social')

# Data-Cleaning - Age Order
Survey_Data <- read.csv('SocialSurveyResultsBangor.csv')
str(Survey_Data)
unique(Survey_Data$Age)
Survey_Data$Sorted_Year <- factor(Survey_Data$Age,      # Reordering age levels for illustration purposes
                                   levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "64+"))
unique(Survey_Data$Sorted_Year)

# Data-Cleaning - Education
unique(Survey_Data$Education)
Survey_Data$Sorted_Education <- factor(Survey_Data$Education,      # Reordering age levels for illustration purposes
                                  levels = c("High school graduate, diploma or the equivalent", "Vocational training", "Bachelor degree", "Master degree", "Doctorate degree"))

# Data-Cleaning - Recognition
unique(Survey_Data$Recognition)
Survey_Data$Sorted_Recognition <- factor(Survey_Data$Recognition,      # Reordering age levels for illustration purposes
                                       levels = c("None", "Rainforest Alliance", "Fairtrade Logo", "EU Organic Leaf", "Rainforest Alliance & EU Organic Leaf", "Fairtrade Logo & Rainforest Alliance",
                                                  "Fairtrade Logo & EU Organic Leaf", "Rainforest Alliance,EU Organic Leaf & Faitrade Logo"))
# Graphing
Age <- ggplot(Survey_Data, aes(x=Sorted_Year))+
  geom_bar(stat="count", width=0.7, fill="#0D3F19")+
  theme_bw()+
  xlab("Age Groups") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size = 9.5, angle = 70, hjust = 1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15)))

Education <- ggplot(Survey_Data, aes(x=Sorted_Education))+
  geom_bar(stat="count", width=0.7, fill="#0D3F19")+
  theme_bw()+
  xlab("Education") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size = 9.5, angle = 70, hjust = 1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15)))

Preference <- ggplot(Survey_Data, aes(x=Preference))+
  geom_bar(stat="count", width=0.7, fill="#0D3F19")+
  theme_bw() +
  xlab("Preference") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size = 9.5, angle = 70, hjust = 1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15)))

Recognition <- ggplot(Survey_Data, aes(x=Sorted_Recognition))+
  geom_bar(stat="count", width=0.7, fill="#0D3F19")+
  theme_bw()+
  xlab("Recognized Ecolabel") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size = 9.5, angle = 70, hjust = 1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15)))

# Merging Basic data Graphs
Combined_Graph <- ggarrange(Age, Education, Preference, Recognition,
                            labels = c("A", "B", "C", "D"),
                            ncol = 2, nrow = 2)
Combined_Graph

# Based on the graphs above, I filtered the data with the highest counts to calculate
# the frequency and manually the percentage.

Age_Freq <- filter(Survey_Data, Age == "18-24")
Age_Perc <- nrow(Age_Freq)/36*100  

Education_Freq <- filter(Survey_Data, Education == "Master degree")
Education_Perc <- nrow(Education_Freq)/36*100  

Preference_Freq <- filter(Survey_Data, Preference == "Nescafe")
Preference_Perc <- nrow(Preference_Freq)/36*100  

Recognition_Freq <- filter(Survey_Data, Recognition == "None")
Recognition_Perc <- nrow(Recognition_Freq)/36*100  


# DATA EXPLORATION
# Graphing Definition Accuracy
Survey_Accuracy <- read.csv('Survey_Accuracy.csv')
Clean_Survey_Accuracy <- select(Survey_Accuracy, -ID)
Clean_Survey <- Clean_Survey_Accuracy[!apply(Clean_Survey_Accuracy == "", 1, all),]

Clean_Survey$Order_Cat <- factor(Clean_Survey$Accuracy_Cat,
                                 levels = c("Correct", "Partially Correct", "Incorrect", "No Idea"))

#Calculating the percentage of each column and plotting in a stacked bar plot
Definition_Accuracy <- Clean_Survey%>%
        # count how often each class occurs in each sample.
        count(Accuracy_Group, Order_Cat)%>% 
        group_by(Accuracy_Group)%>%
        mutate(pct = n / sum(n))%>%
        ggplot(aes(x = Accuracy_Group, y = pct*100, fill = Order_Cat)) + 
        geom_col(width=0.7, alpha = 0.5)+
        geom_text(aes(label = paste0(round(pct * 100), '%')),
                  position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c("#0D3F19", "#B04808", "#4C0000", "#151515"), name = NULL) +
        scale_x_discrete(labels=c("Fairtrade", "EU Organic","Rainforest Alliance", "No Idea")) +
        theme_bw()+
        xlab("Ecolabels") +
        ylab("Definition Accuracy") +
        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15)))


# Scatter Plot comparing whether there's a relationship between a person's age/education level and their choice product
Scatter_Analysis <- ggplot(Survey_Data, aes(x = Sorted_Year, y = Sorted_Education, color = Preference)) +
  geom_jitter(size = 4)+
  scale_color_brewer(palette = "Dark2") +
  theme_bw()+
  xlab("Age Group") +
  ylab("Education Level") +
  theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15)))

# Descriptive data analysis of the (why) section
Extracted_Words <- data.frame(table(unlist(strsplit(tolower(Survey_Data$Why), " "))))

  # The file location needs to be amended to your working directory
  getwd() # This commands shows the working directly you're using
  write.csv(Extracted_Words,"/Users/alex/Desktop/Extracted_Keyords.csv", row.names = FALSE)
  
  
# Cleaning irrelevant words (e.g., "Respondent", "by", "can" etc.) was done by manually deleting within the newly created excel file
# The cleaned excel file was re-saved as Extracted_Keyords_Cleaned to maintain the original extraction file for comparisons
  
install.packages('ggwordcloud')
library(ggwordcloud)

why_cleaned <- read.csv('Extracted_Keyords_Cleaned.csv')

Why_Wordcloud <- ggplot(why_cleaned, aes(label = Var1, size = Freq, color = Freq)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_gradient(low = "lightgrey", high = "darkgreen")

# Merging Data exploration graphs
Combined_Exploration_Graph <- ggarrange(Definition_Accuracy, Scatter_Analysis, Why_Wordcloud,
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)
Combined_Exploration_Graph

# END 