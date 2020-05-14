library(tidyverse) ; library(scales)

segments_with_apc_analytics <- read_json("./data/segments_analyzed.geojson")

market_jfk <- segments_with_apc_analytics %>% 
  filter(FINAL_ID == 104311 | FINAL_ID == 107111) %>%
  select(c(FINAL_ID, trips, service_hours, ridership, riders_per_km))  %>%
  mutate(segment_name = case_when(
    FINAL_ID == 104311 ~ "Market Street",
    FINAL_ID == 107111 ~ "JFK Blvd"))

gathered <- market_jfk %>%
  mutate(zero = service_hours) %>%
  mutate(five = service_hours * .95) %>%
  mutate(ten = service_hours * .90) %>%
  mutate(fifteen = service_hours * .85) %>%
  mutate(twenty = service_hours * .80) %>%
  gather(`zero`, `five`, `ten`, `fifteen`, `twenty`,
         key = "reduction_level", value = "revenue_hours") %>%
  mutate(cost = as.numeric(revenue_hours) * 156.05) %>% # $156.05 - SEPTA NTD report bus cost per service hour
  mutate(cost_per_rider = cost / ridership) %>%
  mutate(service_min_per_trip = as.numeric(revenue_hours) / trips * 60) %>%
  mutate(reduction_level = factor(reduction_level, levels = c("zero", "five", "ten", "fifteen", "twenty")))

ggplot(gathered, aes(x=reduction_level, y=as.numeric(revenue_hours), fill = segment_name)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(limits = c(0, 35), breaks=seq(0, 35, by = 5)) + 
  scale_x_discrete(labels=c("0%", "5%", "10%", "15%", "20%")) + 
  coord_cartesian(ylim=c(10,35)) + 
  scale_fill_discrete(name="Segment Name") + 
  theme_minimal() + 
  ggtitle("Estimated Reduction in Revenue Hours") + 
  xlab("Percent Reduction in Running Time (Revenue Hours)") + 
  ylab("Daily Revenue Hours Provided on Corridor)")

ggplot(gathered, aes(x=reduction_level, y=cost, fill = segment_name)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(labels=dollar, limits = c(0, 6000), breaks=seq(0, 6000, by = 500)) + 
  scale_x_discrete(labels=c("0%", "5%", "10%", "15%", "20%")) + 
  coord_cartesian(ylim=c(3000,5500)) + 
  scale_fill_discrete(name="Segment Name") + 
  theme_minimal() + 
  ggtitle("Estimated Reduction in Operating Costs") + 
  xlab("Percent Reduction in Running Time (Revenue Hours)") + 
  ylab("Daily Cost of Service on Corridor (Revenue Hours x Expense per Revenue Hour)") 

ggplot(gathered, aes(x=reduction_level, y=service_min_per_trip, fill = segment_name)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, by = 0.25)) + 
  scale_x_discrete(labels=c("0%", "5%", "10%", "15%", "20%")) + 
  coord_cartesian(ylim=c(2,4)) + 
  scale_fill_discrete(name="Segment Name") + 
  theme_minimal() + 
  ggtitle("Estimated Reduction in Running Time per Trip") + 
  xlab("Percent Reduction in Running Time (Revenue Hours)") + 
  ylab("Revenue Minutes per Trip on Corridor") 

