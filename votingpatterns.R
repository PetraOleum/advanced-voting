library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
theme_set(theme_classic())
partycolours <- c(
                  "National Party" = "#00529F",
                  "Labour Party" = "#D82A20",
                  "Green Party" = "#098137",
                  "New Zealand First Party" = "#000000",
                  "The Opportunities Party" = "#32DAC3",
                  "ACT New Zealand" = "#FDE401",
                  "Conservative Party" = "#00AEEF",
                  "United Future" = "#501557",
                  "Māori Party" = "#B2001A"
                  )
dat <- read.csv("AdvancedVotesNewZealand.csv", stringsAsFactors = FALSE) %>%
    mutate(Party = gsub(" $", "", Party),
           Party = gsub("Mâori", "Māori", Party),
           Party = gsub("^Conservative$", "Conservative Party", Party),
           Party = gsub("^United Future New Zealand$", "United Future", Party),
           Party = gsub(" Coalition$", "", Party),
           Party = gsub("MANA", "Mana", Party),
           Party = gsub(" \\(.*\\)$", "", Party)) %>%
    pivot_wider(id_cols = c(Year, Party),
                names_from = Type, values_from = c(Votes, Seats)) %>%
    mutate(Votes_Other = Votes_Total - Votes_Advance,
           Advance_Proportion = Votes_Advance / Votes_Total,
           Seat_Difference = Seats_Total - Seats_Advance)

e.years <- sort(unique(dat$Year))

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

partylev$Party = factor(partylev$Party, levels = partyav$Party)

typecomp <- partylev %>%
    select(Year:Party, Votes_Advance,
           Votes_Other, Seats_Advance,
           Seats_Total, PV_Prop_Advance,
           PV_Prop_Total) %>%
    pivot_longer(cols = Votes_Advance:PV_Prop_Total,
                 names_to = c("Quantity", "Type"),
                 names_pattern = "(.*)_([^_]*)$",
                 values_to = "value") %>%
    pivot_wider(names_from = "Quantity", values_from = "value")

vtypes <- elec %>% select(Year, Total_Advance, Total_Other) %>%
    pivot_longer(Total_Advance:Total_Other, names_to = "Type", names_prefix = "Total_",
                 values_to = "Votes") %>%
    mutate(Type = factor(Type, levels = c("Other", "Advance")))

advplot <- ggplot(vtypes, aes(x = Year, y = Votes, fill = Type)) + geom_area() +
    scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
    scale_x_continuous("General Election Year",
                       breaks = e.years,
                       expand = c(0,0)) +
    scale_fill_brewer("Vote Type", palette = "Paired") +
    labs(title = "Total Advance Votes", subtitle = "2002 to 2017")
advplot

propplot <- ggplot(filter(partylev, Party %in% names(partycolours)),
       aes(x = Year, y = Advance_Proportion, colour = Party)) + geom_line() + geom_point() +
    scale_colour_manual(values = partycolours) +
    scale_y_continuous("Percent of votes from Advance votes",
                       labels = scales::percent, limits=c(0, NA), expand=c(0,0)) +
    scale_x_continuous("General Election Year",
                       breaks = e.years) +
    labs(title = "Share of Each Party's Votes From Advanced Votes by Year")
propplot


scplot <- ggplot(filter(partylev, Party %in% names(partycolours)),
       aes(x = Year, y = Seat_Difference, colour = Party)) +
    geom_line() + geom_point() + 
    scale_colour_manual(values = partycolours) +
    scale_x_continuous("General Election Year",
                       breaks = e.years) +
    scale_y_continuous("Seat change from advance results and final results", n.breaks = 8,
                       labels = function(x) { paste0(ifelse(x > 0, "+", ""), x) }) +
    labs(title = "Seat Change Between Advance Vote Results and Final Results")
scplot

pvplot <- ggplot(filter(partylev, Party %in% names(partycolours)),
       aes(x = Year, y = PV_Prop_Total, colour = Party)) +
    geom_line() + geom_point() +
    scale_colour_manual(values = partycolours) +
    scale_x_continuous("General Election Year",
                       breaks = e.years) +
    scale_y_continuous("Party vote proportion", n.breaks = 10,
                       expand=c(0,0), limits = c(0, 0.5), labels = scales::percent) +
    labs(title = "Party vote trajectories")
pvplot

daily <- read.csv("2020-advance-vote-data.csv", stringsAsFactors = FALSE) %>%
    filter(Date != "Totals") %>% mutate(Date = as.Date(Date)) %>%
    pivot_longer(cols = X2020.General.Election:X2014.General.Election, names_to = "Year", names_pattern = "^X([0-9]{4}).*$", values_to = "Votes") %>%
    mutate(Votes = ifelse(is.na(Votes), 0, Votes))

dailyplot <- ggplot(daily, aes(x = Date, y = Votes, fill = Year)) +
    geom_col(position="dodge") +
    scale_x_date("2020 election-equivalent date",
                 date_breaks = "1 day", date_labels = "%a %b %e",
                 expand = c(0,0)) +
    scale_fill_brewer("Election year", palette = "Set2") +
    scale_y_continuous("Advance votes", labels = scales::comma, expand = c(0,0)) +
    labs(title = "Daily Advance Votes", subtitle = "Last updated 8 October 2020") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
dailyplot
