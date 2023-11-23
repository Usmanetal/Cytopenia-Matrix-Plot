library(readxl)
Usman <- read_excel("C:/Users/Usman Ola/Downloads/ERIOLUWa MAIN DATA.xlsx", 
                    sheet = "Data")
View(Usman)

base<-select(Usman,ends_with("BL"),contains("BL_"),contains("BL ("))%>%
  select(3:9,15:17)%>%
  rename(CD4=CD4_BL,WBC="WBC_BL_(10^9/L)",
         LYMP="LYMP_BL (10^9/L)",
         PLAT=PLATELET_BL,
         HCT=HCT_BL,
         HGB="HGB _BL",
         GRAN=GRAN_BL,
         MID=MID_BL,
         RBC=RBC_BL,
         "IL-18"="IL18_BL_(pg/mL)")%>%
        mutate(grp="Baseline")
cart<-Usman[,c(25,27,29,31,33,35,37,39,41,43)]%>%
  rename(CD4=CD4_6M,WBC="WBC_6M_(10^9/L)",
         LYMP="LYMP_6M (10^9/L)",
         PLAT=PLATELET_6M,
         HCT=HCT_6M,HGB="HGB_6M",
         GRAN=GRAN_6M,MID=MID_6M,
         RBC=RBC_6M,
         "IL-18"="IL18_6M (pg/mL)")%>%
  mutate(grp="cART24")
# To produce the table, the "base" and "cart" vector dataframe will be binded together
table<-rbind (base,cart)

# to plot the figures embedding the correlation analysis

New2<-data.frame(Hbg=Usman$HGB_6M,Wbc=Usman$`WBC_6M_(10^9/L)`,
         Lymp=Usman$`LYMP_6M (10^9/L)`,
         Plat=Usman$PLATELET_6M,
         Cd4=Usman$CD4_6M,
         Anemia=cut(Usman$HGB_6M,breaks = c(0,11.9,20),labels=c("<12",">12")),
         Leucopenia=cut(Usman$`WBC_6M_(10^9/L)`,breaks = c(0,3.9,20),labels = c("<4",">4")),
         Lymphopenia=cut(Usman$`LYMP_6M (10^9/L)`,breaks = c(0,0.79,20),labels = c("<0.8",">0.8")),
         Thrombocytopenia=cut(Usman$PLATELET_6M,breaks = c(0,149.9,1000),labels = c("<150",">150")),
         CD4=cut(Usman$CD4_6M,breaks=c(0,199,2000),labels = c("<200",">200")))

New1<-data.frame(Hbg=Usman$`HGB _BL`,Wbc=Usman$`WBC_BL_(10^9/L)`,
         Lymp=Usman$`LYMP_BL (10^9/L)`,
         Plat=Usman$PLATELET_6M,Cd4=Usman$CD4_BL,
         Anemia=cut(Usman$`HGB _BL`,breaks = c(0,11.9,20),labels=c("<12",">12")),
         Leucopenia=cut(Usman$`WBC_BL_(10^9/L)`,breaks = c(0,3.9,20),labels = c("<4",">4")),
         Lymphopenia=cut(Usman$`LYMP_BL (10^9/L)`,breaks = c(0,0.79,20),labels = c("<0.8",">0.8")),
         Thrombocytopenia=cut(Usman$PLATELET_BL,breaks = c(0,149.9,1000),labels = c("<150",">150")),
         CD4=cut(Usman$CD4_BL,breaks=c(0,199,2000),labels = c("<200",">200")))

newd<-bind_rows(Baseline=New1,cART24=New2,.id="grp")

A<-newd%>%mutate(Anemia=cut(Hbg,breaks = c(0,11.9,100),labels = c("<12",">12")),
         Leucopenia=cut(Wbc,breaks = c(0,3.9,100),labels = c("<4",">4")),
         Lymphocytopenia=cut(Lymp,breaks = c(0,0.7,100),labels = c("<0.8",">0.8")),
         Throbocytopenia=cut(Plat,breaks = c(0,149,Inf),labels = c("<150",">150")),
         immune=cut(Cd4,breaks = c(0,199,Inf),labels = c("<200",">200")))%>%
  mutate(Anemia=factor(Anemia,labels = c("<12"=expression(bold("Anemic")),">12"=expression(bold("non-Anemic")))),
         immune=factor(immune, labels = c("<200"=expression(bold("<200")),">200"=expression(bold("">="200")))))%>%
  ggplot(.,aes(x=Cd4,y=Hbg,col=grp))+
  geom_point()+stat_cor(method = "kendall",cor.coef.name="tau")+
  geom_smooth(se=FALSE,method = "lm",fullrange=TRUE)+
  facet_grid(Anemia~immune,scales = "free",labeller = label_parsed,switch ="y")+
  theme_minimal()+scale_color_manual(name="",values = c("blue","black"))+
  theme(axis.line.y = element_line(colour="black",linewidth = 1),legend.key.size = unit(0.5,"cm"),legend.key.width =unit(0.1,"cm"),
  legend.text = element_text(face="bold"),legend.box = "horizontal",                                                                                                                                                                                                                                                                                                              strip.placement = "outside",strip.text.y =element_text(color = "darkblue",face = "bold",size = 12),legend.direction = "vertical",legend.position="none")+
  labs(x="",y=expression(bold(Haemoglobin~(g/dl))))

# Analysis Leucopenia
B<-table%>%mutate(
  Anemia=cut(HGB,breaks = c(0,11.9,100),labels = c("<12",">12")),
  Leucopenia=cut(WBC,breaks = c(0,3.9,100),labels = c("Leucopenic","non-Leucopenic")),
  Lymphocytopenia=cut(LYMP,breaks = c(0,0.7,100),labels = c("<0.8",">0.8")),
  Throbocytopenia=cut(PLAT,breaks = c(0,149,Inf),labels = c("<150",">150")),
  immune=cut(CD4,breaks = c(0,199,Inf),labels = c("<200",">200")))%>%
  mutate(Anemia=factor(Anemia,labels = c("<12",">12")),
  immune=factor(immune, labels = c("<200"=expression(bold("<200")),">200"=expression(bold("">="200")))))%>%
  ggplot(.,aes(x=CD4,y=WBC,col=grp))+
  geom_point()+stat_cor(method = "kendall",cor.coef.name="tau")+
  geom_smooth(se=FALSE,method = "lm",fullrange=TRUE)+
  facet_grid(Leucopenia~immune,scales = "free",switch = "y")+
  theme_minimal()+scale_color_manual(name="",values = c("blue","black"))+
  theme(axis.line = element_line(colour="black",linewidth = 1),
  legend.key.size = unit(0.5,"cm"),legend.key.width =unit(0.1,"cm"),
  strip.placement = "outside",strip.text.y =element_text(color = "darkblue",
  face = "bold",size = 12), strip.text.x = element_blank(),
  legend.direction = "vertical",legend.position="none")+
  labs(x="",y=expression(bold("Leucocytes (×10"^6*"/mm"^3*")")))

# Analysis Thrombocytopenia, Platelet count

D<-table%>%mutate(
  Anemia=cut(HGB,breaks = c(0,11.9,100),labels = c("<12",">12")),
  Leucopenia=cut(WBC,breaks = c(0,3.9,100),labels = c("<4",">4")),
  Lymphocytopenia=cut(LYMP,breaks = c(0,0.7,100),labels = c("<0.8",">0.8")),
  Thrombocytopenia=cut(PLAT,breaks = c(0,149,Inf),labels = c("Thrombocytopenic","non-Thrombocytopenic")),
  immune=cut(CD4,breaks = c(0,199,Inf),labels = c("<200",">200")))%>%
  mutate(immune=factor(immune, labels = c("<200"=expression(bold("<200")),">200"=expression(bold("">="200")))))%>%
  ggplot(.,aes(x=CD4,y=PLAT,col=grp))+geom_point()+stat_cor(method = "kendall",cor.coef.name="tau")+
  geom_smooth(se=FALSE,method = "lm",fullrange=TRUE)+
  facet_grid(Thrombocytopenia~immune,scales = "free",switch ="y")+
  theme_minimal()+
  scale_color_manual(name="",values = c("blue","black"),labels=c("Baseline"="Baseline","cART24"="cART24"))+
  theme(axis.line = element_line(colour="black",linewidth = 1),legend.key.size = unit(0.5,"cm"),
        legend.key.width =unit(0.1,"cm"),strip.placement = "outside",
        strip.text.y =element_text(color = "darkblue",face = "bold",size = 12), 
        strip.text.x = element_blank(),legend.direction = "horizontal",legend.position = "bottom")+
  labs(x="",y=expression(bold("Thrombocytes (×10"^3*"/mm"^3*")")))

C<-table%>%mutate(
  Anemia=cut(HGB,breaks = c(0,11.9,100),labels = c("<12",">12")),
  Leucopenia=cut(WBC,breaks = c(0,3.9,100),labels = c("<4",">4")),
  Lymphocytopenia=cut(LYMP,breaks = c(0,0.7,100),
                      labels = c("Lymphocytopenic","non-Lymphocytopenic")),
  Thrombocytopenia=cut(PLAT,breaks = c(0,149,Inf),labels = c("Thrombocytopenic","non-Thrombocytopenic")),
  immune=cut(CD4,breaks = c(0,199,Inf),labels = c("<200",">200")))%>%
  mutate(Lymphocytopenia=factor(Lymphocytopenia,
         labels = c(
          "Lymphocytopenic"=expression(bold("Lymphocytopenic")),
         "non-Lymphocytopenic"=expression(bold("non-Lymphocytopenic")))),
         immune=factor(immune, labels = c("<200"=expression(bold("<200")),">200"=expression(bold("">="200")))))%>%
  ggplot(.,aes(x=CD4,y=LYMP,col=grp))+
  geom_point()+stat_cor(method = "kendall",cor.coef.name="tau")+
  geom_smooth(se=FALSE,method = "lm",fullrange=TRUE)+
  facet_grid(Lymphocytopenia~immune,scales = "free",labeller = label_parsed,switch ="y")+
  theme_minimal()+scale_color_manual(name="",values = c("blue","black"))+
  theme(axis.line.y = element_line(colour="black",linewidth = 1),
        legend.key.size = unit(0.5,"cm"),legend.key.width =unit(0.1,"cm"),
        strip.placement = "outside",strip.text.y =element_text(color = "darkblue",
        face = "bold",size = 10) ,legend.direction = "vertical",legend.position="none")+
  labs(x="",y=expression(bold("Lymphocytes (×10"^3*"/mm"^3*")")))

figure<-ggarrange(A,C,B,D,common.legend = TRUE,legend = "bottom")
Cytopenia_Matrix_Plot<-annotate_figure(figure,
               bottom = text_grob(expression(bold("CD4 count(log10"~cell/mm^"3"*")"),labels=c("Baseline","cART24")),
               col="darkblue",vjust = -3.5,size = 12))
ggsave("Cytopenia_Matrix_plot.pdf",width = 16,height = 10)
