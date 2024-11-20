library(lattice)
library(datasets)
library(readxl)
library(sf)
library(cartography)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(reshape)
library(flexdashboard)
library(rmarkdown)


##########################      successful candidate
primary_result = read_excel("D:\\Second Semester\\MBA - Data visualization\\Project\\2-List of Successful Candidates.xlsx")
primary_result
ggplot(data=primary_result)+aes(x=PARTY,Y=WINNER, fill=PARTY)+geom_bar()+
  labs(title="Bar graph of political party",x = "PARTY", y = "Number of seats")

########################## MAP
himachalmap = st_read("D:\\Second Semester\\MBA - Data visualization\\Project\\himachal map\\S08_AC.shp")
plot(st_geometry(himachalmap))
Constiturcy_and_map = inner_join(himachalmap,primary_result)
Constiturcy_and_map
ggplot(Constiturcy_and_map)+geom_sf(aes(fill=PARTY))+labs(title="Geographical view of Himachal Pradesh election result")+
  scale_fill_manual(values = c("orange", "lightblue", "grey"))



########################## Contestede and won
Contested = read_excel("D:\\Second Semester\\MBA - Data visualization\\Project\\5-Performance of Political Parties.xlsx")
Contested
ggplot(data=Contested, aes(x = PARTY1, y = Seats, fill = result, group=WON)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="comparative chart of Seat contested and won",x = "PARTY", y = "NUMBER OF SEATS CONTESTED") +
   theme_classic()

########################## TOtal vote percentage by party
vote_percent = read_excel("D:\\Second Semester\\MBA - Data visualization\\Project\\5-Performance of Political Parties.xlsx")
vote_percent
ggplot(vote_percent, aes(x = "", y = VOTES_per, fill = PARTY)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "My Pie Chart", x = NULL, y = NULL, fill = "Legend")+
  scale_fill_manual(values = c("yellow", "orange", "blue", "skyblue", "grey"))+
  geom_label(aes(label = paste0(VOTES_per, "%")), position = position_stack(vjust = 0.5))

########################  candidate percentage by Gender
candidate_percent = read_excel("D:\\Second Semester\\MBA - Data visualization\\Project\\10-Detailed Results.xlsx") %>% 
  group_by(SEX) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
candidate_percent
ggplot(candidate_percent, aes(x="", y = n, fill = SEX)) +
  geom_col(color = "white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), color="white") +
  coord_polar(theta = "y", start = 0)+
  scale_fill_manual(values = c("#CD534CFF","#0073C2FF", "white")) +
  theme_void()





###################### voters data


# create a data frame for the given data
df <- data.frame(Gender = c("Male", "Female", "Third Gender"),
                 Electors = c(2854945, 2737845, 38),
                 Voted = c(2020509, 2101674, 26))

# calculate percentage of electors who voted
df$voted_percent <- df$Voted / df$Electors * 100

# create the combined bar and stacked chart
ggplot(df, aes(x = Gender)) +
  geom_col(aes(y = Electors, fill = "Total Electors"), width = 0.5) +
  geom_col(aes(y = Voted, fill = "Votes Polled"), width = 0.5) +
  scale_fill_manual(values = c("Total Electors" = "#0072B2", "Votes Polled" = "#E69F00"),
                    name = "") +
  labs(x = NULL, y = "Number of Electors",
       title = "Number of Electors and Votes Polled by Gender") +
  theme_minimal() +
  # add percentage labels for "Voted" segment
  geom_text(aes(y = Voted/2, label = paste0(round(voted_percent), "%")),
            color = "white", size = 4, fontface = "bold") +
  # add label for "Total Electors" segment
  geom_text(aes(y = Electors, label = format(Electors, big.mark = ",")),
            vjust = -0.5, size = 3.5) +
  # add label for "Votes Polled" segment
  geom_text(aes(y = Voted, label = format(Voted, big.mark = ",")),
            vjust = 1, size = 3.5)

library(palmerpenguins)
