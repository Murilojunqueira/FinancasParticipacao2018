
################## Interaction between Budget per capita (log) and lag.pb ##################

# Distribution of InvestPer
hist(DataAnalysis_Complete$BudgetPP_log)

# Distribution of InvestPer
hist(DataAnalysis_Complete$InvestPer)

# Quantities of interest
SemPB <- setx(Zelig_pb_Min, BudgetPP_log = seq(6, 9, by=0.25), InvestPer = 0.03)
ComPB <- setx(Zelig_pb_Min, BudgetPP_log = seq(6, 9, by=0.25), InvestPer = 0.20)


# Zelig Graph
s_out <- sim(Zelig_pb_Min, x = SemPB, x1 = ComPB)

# summary(s_out)

# Same info in ggplot graph

# Extract simulated data
qi_Values <- list(SemPB, ComPB)

plotdata <- GraphData(qi_Values, Zelig_pb_Min, "BudgetPP_log", ci = 95)
levels(plotdata$Group) <- c("Low Invest", "High Invest") 

#plot in ggplot2
ggplot(data=plotdata, aes(x = BudgetPP_log, y =mean, fill = Group)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  geom_bar(aes(y = (0.2/min(mean))*Freq/sum(Freq)), stat="identity", 
           fill = "grey", colour = "white", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(6:9), labels=c(exp_f(6:9))) +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  labs(x = "Budget per capita (log)", y = "PB Adoption/Continuity") +
  scale_fill_manual(values=c("light blue", "orange"), name="Previous Adminsitration:") +
  theme_classic(base_size = 14) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 


rm(ComPB, SemPB, s_out)
rm(qi_Values, plotdata)


#######################

# Quantities of interest
SemPB <- setx(Zelig_pb_Min, InvestPer = seq(0, 0.25, by=0.01), BudgetPP_log = 6)
ComPB <- setx(Zelig_pb_Min, InvestPer = seq(0, 0.25, by=0.01), BudgetPP_log = 9)


# Zelig Graph
s_out <- sim(Zelig_pb_Min, x = SemPB, x1 = ComPB)

# summary(s_out)


# Same info in ggplot graph

# Extract simulated data
qi_Values <- list(SemPB, ComPB)

plotdata <- GraphData(qi_Values, Zelig_pb_Min, "InvestPer", ci = 95)
levels(plotdata$Group) <- c("Low BudgetPP", "High BudgetPP") 

#plot in ggplot2
ggplot(data=plotdata, aes(x = InvestPer, y =mean, fill = Group)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  geom_bar(aes(y = (0.2/min(mean))*Freq/sum(Freq)), stat="identity", 
           fill = "grey", colour = "white", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  labs(x = "Investment Rate", y = "PB Adoption/Continuity") +
  scale_fill_manual(values=c("light blue", "orange"), name="Previous Adminsitration:") +
  theme_classic(base_size = 14) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 


rm(ComPB, SemPB, s_out)
rm(qi_Values, plotdata)



#######################

# Quantities of interest
SemPB <- setx(Zelig_pb_Min, InvestPer = seq(0, 0.25, by=0.01), BudgetPP_log = 6, lag.pb = 0)
ComPB <- setx(Zelig_pb_Min, InvestPer = seq(0, 0.25, by=0.01), BudgetPP_log = 9, lag.pb = 0)


# Zelig Graph
s_out <- sim(Zelig_pb_Min, x = SemPB, x1 = ComPB)

# summary(s_out)


# Same info in ggplot graph

# Extract simulated data
qi_Values <- list(SemPB, ComPB)

plotdata <- GraphData(qi_Values, Zelig_pb_Min, "InvestPer", ci = 95)
levels(plotdata$Group) <- c("Low BudgetPP", "High BudgetPP") 

#plot in ggplot2
ggplot(data=plotdata, aes(x = InvestPer, y =mean, fill = Group)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  geom_bar(aes(y = (0.2/min(mean))*Freq/sum(Freq)), stat="identity", 
           fill = "grey", colour = "white", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  labs(x = "Investment Rate", y = "PB Adoption/Continuity") +
  scale_fill_manual(values=c("light blue", "orange"), name="Previous Adminsitration:") +
  theme_classic(base_size = 14) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 


rm(ComPB, SemPB, s_out)
rm(qi_Values, plotdata)


#######################

# Quantities of interest
SemPB <- setx(Zelig_pb_Min, InvestPer = seq(0, 0.25, by=0.01), BudgetPP_log = 6, lag.pb = 1)
ComPB <- setx(Zelig_pb_Min, InvestPer = seq(0, 0.25, by=0.01), BudgetPP_log = 9, lag.pb = 1)


# Zelig Graph
s_out <- sim(Zelig_pb_Min, x = SemPB, x1 = ComPB)

# summary(s_out)


# Same info in ggplot graph

# Extract simulated data
qi_Values <- list(SemPB, ComPB)

plotdata <- GraphData(qi_Values, Zelig_pb_Min, "InvestPer", ci = 95)
levels(plotdata$Group) <- c("Low BudgetPP", "High BudgetPP") 

#plot in ggplot2
ggplot(data=plotdata, aes(x = InvestPer, y =mean, fill = Group)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  geom_bar(aes(y = (0.2/min(mean))*Freq/sum(Freq)), stat="identity", 
           fill = "grey", colour = "white", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  labs(x = "Investment Rate", y = "PB Adoption/Continuity") +
  scale_fill_manual(values=c("light blue", "orange"), name="Previous Adminsitration:") +
  theme_classic(base_size = 14) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 


rm(ComPB, SemPB, s_out)
rm(qi_Values, plotdata)



stargazer(attitude)

