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
