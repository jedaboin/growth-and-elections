###Results
##1997 Election
#1997 Election during Samper
capt_liberal1997 <- rdrobust(rd_final_liberal$cap.trnf.chg, rd_final_liberal$marginRD, p=1, kernel="uniform", h=0.1)
summary(capt_liberal1997)

capt_liberal1997 <- rdrobust(rd_final_liberal$cap.trnf.chg, rd_final_liberal$marginRD, p=1, kernel="uniform", h=0.05)
summary(capt_liberal1997)

cur_liberal1997 <- rdrobust(rd_final_liberal$cur.trnf.chg, rd_final_liberal$marginRD, p=1, kernel="uniform", h=0.05)
summary(cur_liberal1997)

cur_liberal1997 <- rdrobust(rd_final_liberal$cur.trnf.chg, rd_final_liberal$marginRD, p=1, kernel="uniform", h=0.1)
summary(cur_liberal1997)

cur_liberal1997_flip <- rdrobust(rd_flip_liberal$cur.trnf.chg, rd_flip_liberal$marginRD, p=1, kernel="uniform", h=0.1)
summary(cur_liberal1997_flip)

capt_liberal1997_flip <- rdrobust(rd_flip_liberal$cap.trnf.chg, rd_flip_liberal$marginRD, p=1, kernel="uniform", h=0.1)
summary(capt_liberal1997_flip)

#1997 Election during Pastrana

capt_1997 <- rdrobust(rd_final$cap.trnf.chg, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(capt_1997)

cur_1997 <- rdrobust(rd_final$cur.trnf.chg, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(cur_1997)

gr.nl_1997 <- rdrobust(rd_final$gr_avg, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(gr.nl_1997)

gr.gdp_1997 <- rdrobust(rd_final$gdp.gr, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(gr.gdp_1997)

#All elections

capt_2000 <- rdrobust(rd_final$cap.trnf.chg, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(capt_2000)

cur_2000 <- rdrobust(rd_final$cur.trnf.chg, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(cur_2000)

roy_2000 <- rdrobust(rd_final$roy.trnf.chg, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(roy_2000)

gr.nl_2000 <- rdrobust(rd_final$gr_avg, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(gr.nl_2000)

gr.gdp_2000 <- rdrobust(rd_final$gdp.gr, rd_final$marginRD, p=1, kernel="uniform")
summary(gr.gdp_2000)



rdplot(rd_final_liberal$cap.trnf.chg, rd_final_liberal$marginRD, binselect = "es", p=1)
out=rdrobust(rd_final_liberal$cap.trnf.chg[rd_final$year==1997], rd_final_liberal$marginRD[rd_final$year==1997], p=1, h=0.1, c=0, kernel = "uniform")
summary(out)
