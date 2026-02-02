library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

pwe <- read_excel("C:/Users/EJacinto/data/R Weather Graphic Input.xlsx", sheet = "Crater Lake")

pwe <- janitor::clean_names(pwe)

pwe <- pwe |>
  mutate(newDay = seq(1, length(day)))

p <- ggplot(pwe, aes(newDay, high_f)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(pwe, mapping=aes(x=newDay, ymin=record_low_f, ymax=record_high_f), colour = "#A5E1AD", alpha=.9) 

p <- p + 
  geom_linerange(pwe, mapping=aes(x=newDay, ymin=average_low_f, ymax=average_high_f), colour = "wheat4")

p <- p + 
  geom_line(pwe, mapping=aes(x=newDay, y=low_f, group=1), color = "#2C4CE9") +
  geom_vline(xintercept = 0, colour = "black", linetype=1, size=0.5)

p <- p + 
  geom_line(pwe, mapping=aes(x=newDay, y=high_f, group=1), color = "#E2573E") +
  geom_vline(xintercept = 0, colour = "black", linetype=1, size=0)

p <- p + 
  geom_hline(yintercept = -20, colour = "white", linetype=1) +
  geom_hline(yintercept = -10, colour = "white", linetype=1) +
  geom_hline(yintercept = 0, colour = "white", linetype=1) +
  geom_hline(yintercept = 10, colour = "white", linetype=1) +
  geom_hline(yintercept = 20, colour = "white", linetype=1) +
  geom_hline(yintercept = 30, colour = "white", linetype=1) +
  geom_hline(yintercept = 40, colour = "white", linetype=1) +
  geom_hline(yintercept = 50, colour = "white", linetype=1) +
  geom_hline(yintercept = 60, colour = "white", linetype=1) +
  geom_hline(yintercept = 70, colour = "white", linetype=1) +
  geom_hline(yintercept = 80, colour = "white", linetype=1) +
  geom_hline(yintercept = 90, colour = "white", linetype=1) +
  geom_hline(yintercept = 100, colour = "white", linetype=1)

p <- p + 
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 

dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}
a <- dgr_fmt(seq(-20,100, by=10))

p <- p +
  coord_cartesian(ylim = c(-20,100)) +
  scale_y_continuous(breaks = seq(-20,100, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

PresentLows <- pwe %>%
  mutate(record = ifelse(record_low_f==low_f, "Y", "N")) %>% 
  filter(record == "Y")  

PresentHighs <- pwe %>%
  mutate(record = ifelse(record_high_f==high_f, "Y", "N")) %>% 
  filter(record == "Y")  

p <- p +
  geom_point(data=PresentLows, aes(x=newDay, y=low_f), colour="blue3") +
  geom_point(data=PresentHighs, aes(x=newDay, y=high_f), colour="firebrick3")

p <- p +
  ggtitle("Crater Lake's Weather in 2022") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 20, y = 98, label = "Temperature", size=4, fontface="bold")


p <- p +
  annotate("text", x = 52, y = 94, 
           label = "Data represents highest and lowest daily temperatures.", size=3, colour="gray30") +
  annotate("text", x = 45.5, y = 91, 
           label = "Accessible data dates back to January 1, 2022.", size=3, colour="gray30")

legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))

p <- p +
  annotate("text", x = 335, y = 82, label = "There are 3 days", size=3, colour="firebrick3") +
  annotate("text", x = 334, y = 79, label = "hottest in 2022.", size=3, colour="firebrick3")

p <- p +
  annotate("segment", x = 181, xend = 181, y = 5-20, yend = 25-20, colour = "#A5E1AD", size=3) +
  annotate("segment", x = 181, xend = 181, y = 12-20, yend = 18-20, colour = "wheat4", size=3) +
  geom_line(data=legend_data, aes(x=x,y=y+2-20), color = "#E2573E") +
  geom_line(data=legend_data, aes(x=x,y=y-2.5-20), color = "#2C4CE9") +
  annotate("segment", x = 183, xend = 185, y = 17.7-20, yend = 17.7-20, colour = "wheat4", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2-20, yend = 12.2-20, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2-20, yend = 17.7-20, colour = "wheat4", size=.5) +
  annotate("text", x = 196, y = 14.75-20, label = "NORMAL RANGE", size=2, colour="gray30") +
  annotate("text", x = 157, y = 15-20, label = "HIGH TEMPERATURE", size=2, colour="#E2573E") +
  annotate("text", x = 157, y = 10-20, label = "LOW TEMPERATURE", size=2, colour="#2C4CE9") +
  annotate("text", x = 193, y = 25-20, label = "RECORD HIGH", size=2, colour="gray30") +
  annotate("text", x = 193, y = 5-20, label = "RECORD LOW", size=2, colour="gray30")

print(p)

glimpse(pwe)
