##Compiling the Dataset
nl_col_growth(1999,2000,elecciones1997_final)
nl_col_growth(2001,2003,elecciones2000_final)
nl_col_growth(2004,2007,elecciones2003_final)
nl_col_growth(2008,2011,elecciones2007_final)
nl_col_growth(2012,2015,elecciones2011_final)

##Dropping outliers
q99 <- quantile(rd_final$gdp.gr, 0.99, na.rm = TRUE)
q1 <- quantile(rd_final$gdp.gr, 0.01, na.rm = TRUE)
rdd1 <- rd_final
rdd1$gdp.gr[rdd1$gdp.gr > q99 | rdd1$gdp.gr < q1] <- NA
q99_cap <- quantile(rd_final$cap.trnf.chg, 0.99, na.rm = TRUE)
q1_cap <- quantile(rd_final$cap.trnf.chg, 0.01, na.rm = TRUE)
rdd1$cap.trnf.chg[rdd1$cap.trnf.chg > q99_cap | rdd1$cap.trnf.chg < q1_cap] <- NA

##Density
density_sample <- density(rd_final$marginRD)
rd_final$density <- density_sample$y

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

capt_1997 <- rdrobust(rd_final$cap.trnf.chg, rd_final$marginRD, p=1, kernel="triangular", h=0.1)
summary(capt_1997)

cur_1997 <- rdrobust(rd_final$cur.trnf.chg, rd_final$marginRD, p=1, kernel="triangular", h=0.1)
summary(cur_1997)

gr.nl_1997 <- rdrobust(rd_final$gr_avg, rd_final$marginRD, p=1, kernel="triangular", h=0.1)
summary(gr.nl_1997)

gr.gdp_1997 <- rdrobust(rd_final$gdp.gr, rd_final$marginRD, p=1, kernel="triangular", h=0.1)
summary(gr.gdp_1997)

#All elections

capt_2000 <- rdrobust(rd_final$cap.trnf.chg, rd_final$marginRD, p=1, kernel="uniform", h=0.1)
summary(capt_2000)

cur_2000 <- rdrobust(rd_final$cur.trnf.chg, rd_final$marginRD, p=1, kernel="triangular", h=0.05)
summary(cur_2000)

roy_2000 <- rdrobust(rd_final$roy.trnf.chg, rd_final$marginRD, p=1, kernel="uniform", h=0.05)
summary(roy_2000)

gr.nl_2000 <- rdrobust(rd_final$gr_avg, rd_final$marginRD, p=1, kernel="triangular", h=0.1)
summary(gr.nl_2000)

gr.gdp_2000 <- rdrobust(rd_final$gdp.gr, rd_final$marginRD, p=1, kernel="triangular", h=0.025)
summary(gr.gdp_2000)

##Flipping Observations

gdp_flip <- rdrobust(rd_final$gdp.gr[rd_final$flip==1], rd_final$marginRD[rd_final$flip==1], p=1, kernel="triangular", h=0.05)
summary(gdp_flip)

capt_flip <- rdrobust(rd_final$cap.trnf.chg[rd_final$flip==0], rd_final$marginRD[rd_final$flip==0], p=1, kernel="triangular", h=0.1)
summary(capt_flip)

rdplot(rd_final_liberal$cap.trnf.chg, rd_final_liberal$marginRD, binselect = "es", p=1)
out=rdrobust(rd_final_liberal$cap.trnf.chg[rd_final$year==1997], rd_final_liberal$marginRD[rd_final$year==1997], p=1, h=0.1, c=0, kernel = "uniform")
summary(out)
