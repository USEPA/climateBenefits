# this loads NLA, chesLakeConc, ChesLakeLoad
load(file.path("C:/Users/JBEAULIE/Environmental Protection Agency (EPA)/",
              "Moore, Chris - Climate benefits of nutrient management/",
              "Lakes Modeling/ChesapeakeLoadData20140203.rda"))

# have a look at data
NLA # PTL, NTL, and chla are in ug/L

# rename to be consistent with GHG model
NLA %<>%
  mutate(`log10(TP..ug.L.)` = log10(PTL), # ug in NLA
         `log10(Nvv)` = log10(NTL), # ug in NLA
         `log10(Chlorophyll.a..ug.L.)` = log10(CHLA))


#Estimate Chla
chla_mod <- with(NLA, lm(`log10(Chlorophyll.a..ug.L.)` ~ `log10(TP..ug.L.)` + `log10(Nvv)`)) #linear model for Chla from NLA
summary(chla_mod)
sqrt(mean(chla_mod$residuals^2))
saveRDS(chla_mod, file = "store/pChla.rds")

ChesLakeConc[,"Chla_sim"]<-10**predict(CHLA,
                                 newdata=data.frame(PTL=ChesLakeConc[,"PvvTMDLnew"],NTL=ChesLakeConc[,"NvvTMDLnew"]))

# Compare Brian's reported chla to those predicted from our NLA07 model. 
# Perfect correspondence.
ggplot(ChesLakeConc, aes(ChlaTMDLnew, Chla_sim)) +
  geom_point()
