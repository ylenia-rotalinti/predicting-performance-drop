#performance degradation
ggplot(training, 
              aes(x = batch, y = performance, group=1)) + 
  geom_line(colour="blue3", size=0.6)+
  geom_vline(xintercept=5, color="firebrick3")+
  geom_vline(xintercept=4, color="red", linetype="dotted")+
  geom_vline(xintercept=6, color="red", linetype="dotted")+
  geom_point(colour="blue3", size=1.8)+
  theme_bw()+
  ylab("accuracy")
colors <- c("dodgerblue","dodgerblue","firebrick", "forestgreen")

#performance shift
ggplot(training, 
       aes(x = batch, y = performance_shift, group=1)) + 
  geom_line(colour="blue3", size=0.6)+
  geom_vline(xintercept=5, color="firebrick3")+
  geom_vline(xintercept=4, color="red", linetype="dotted")+
  geom_vline(xintercept=6, color="red", linetype="dotted")+
  geom_point(colour="blue3", size=1.8)+
  theme_bw()+
  ylab("accuracy shift")

#discrimination errror ("darkorchid2")
ggplot(training, 
       aes(x = batch, y = discrimination_error, group=1)) + 
  geom_line(colour="firebrick", size=0.665)+
  #geom_vline(xintercept=5, color="firebrick3")+
  #geom_vline(xintercept=4, color="red", linetype="dotted")+
  #geom_vline(xintercept=6, color="red", linetype="dotted")+
  geom_point(colour="firebrick", size=1.8)+
  theme_bw()+
  ylab("discrimination error")

#average score
ggplot(training, 
       aes(x = batch, y = avr_prob_score, group=1)) + 
  geom_line(colour="forestgreen", size=0.665)+
  #geom_vline(xintercept=5, color="firebrick3")+
  #geom_vline(xintercept=4, color="red", linetype="dotted")+
  #geom_vline(xintercept=6, color="red", linetype="dotted")+
  geom_point(colour="forestgreen", size=1.8)+
  theme_bw()+
  ylab("average score")

# plot #instances per class per months
# df <- parse_COVID_data("SYN")
# # Create a new column for month
# df <- df %>%
#   mutate(month = format(covid_dt, "%Y-%m"))
# 
# # Group and summarize the data based on the "Death" variable
# df_counts <- df %>%
#   group_by(month, Death) %>%
#   summarize(count = n()) %>%
#   ungroup()
# 
# # Create a bar plot using ggplot2
# ggplot(df_counts, aes(x = month, y = count, fill = Death)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Count of Death by Month",
#        x = "Month",
#        y = "Count") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))