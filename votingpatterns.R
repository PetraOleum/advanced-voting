library(dplyr)
library(tidyr)
dat <- read.csv("AdvancedVotesNewZealand.csv", stringsAsFactors = FALSE) %>%
    mutate(Party = gsub(" $", "", Party),
           Party = gsub("Mâori", "Māori", Party)) %>%
    pivot_wider(id_cols = c(Year, Party), names_from = Type, values_from = c(Votes, Seats)) %>%
    mutate(Votes_Other = Votes_Total - Votes_Advance,
           Advance_Proportion = Votes_Other / Votes_Total,
           Seat_Difference = Seats_Advance - Seats_Total)

elec <- dat %>% group_by(Year) %>%
    summarise(Total_Votes = sum(Votes_Total),
              Total_Advance = sum(Votes_Advance),
              Total_Other = sum(Votes_Other),
              Total_Seats = sum(Seats_Total),
              Advance_Seats = sum(Seats_Advance),
              Seat_Differences = sum(abs(Seat_Difference)),
              .groups = "drop") %>%
    mutate(Prop_Advance = Total_Advance / Total_Other)

partylev <- dat %>% left_join(elec, by = "Year") %>%
    mutate(PV_Prop_Total = Votes_Total / Total_Votes,
           PV_Prop_Advance = Votes_Advance / Total_Advance)

partyav <- partylev %>% group_by(Party) %>%
    summarise(Total_Mean_Prop = mean(PV_Prop_Total),
              Advance_Mean_Prop = mean(PV_Prop_Advance),
              .groups = "drop") %>% arrange(desc(Total_Mean_Prop))

partylev %>% select(Year:Party, Votes_Advance, Votes_Other, Seats_Advance, Seats_Total, PV_Prop_Advance, PV_Prop_Total) %>%
    pivot_longer(cols = Votes_Advance:PV_Prop_Total,
                 names_to = c("Quantity", "Type"),
                 names_pattern = "(.*)_([^_]*)$",
                 values_to = "value") %>%
    pivot_wider(names_from = "Quantity", values_from = "value") %>% drop_na()
