setwd(getwd())
dts.all = read.csv( "sample_set_LB.csv" )
dim(dts.all); dts.all[1:3,]

dts.all$BET_NYcatch = ifelse(dts.all$BET_N == 0, 0, 1)            #標記有無catch
  std.CPUE = data.frame( Year = min(dts.all$Year) : max(dts.all$Year))
  COEFFS = data.frame(matrix(NA,ncol=9))
  colnames(COEFFS) = c("reg", "Null dev", "Null df", "Res dev", "Res df", "R^2", "adj. R^2", "AIC", "BIC")

for( r in 3:6 ) {
  dts.R = dts.all[dts.all$reg == r, ]
  dts.R.1 = dts.R[dts.R$BET_NYcatch == 1, ]                       #選取有漁獲者
  nominal = aggregate(betCPUE ~ Year, data = dts.R, FUN = mean)     #實質(名目)CPUE
    nominal = merge(nominal, std.CPUE, by="Year", all=T)           #在沒資料的年代補NA
  fit.glm.a = glm(log(betCPUE) ~ 
                  as.factor(Year) + as.factor(Quarter) + 
                  as.factor(Longitude) + as.factor(Latitude) +
                  as.factor(Cluster) + as.factor(CTlevel), 
                  data=dts.R.1)
    dts.R[dts.R$BET_NYcatch == 1, "BET_N_pred"] = exp(predict(fit.glm.a, data = dts.R.1, type = "response"))     #預估CPUE放入dts.R中，若沒有catch的會是NA
      fit.glm.a
      summary(fit.glm.a)
      anova(fit.glm.a)
  fit.glm.b = glm(BET_NYcatch ~ as.factor(Year) + as.factor(Quarter) + 
                                as.factor(Longitude) + as.factor(Latitude) +
                                as.factor(Cluster) + as.factor(CTlevel), 
                                data=dts.R)
    dts.R$BET_NYcatch_pred = predict(fit.glm.b, data = dts.R, type = "response")     #有無漁獲的預估
      fit.glm.b
      summary(fit.glm.b)
      anova(fit.glm.b)
  dts.R$pred = dts.R$BET_N_pred * dts.R$BET_NYcatch_pred                       #相乘才是真正的CPUE估計
  temp = aggregate(pred ~ Year, data = dts.R, FUN = mean)
  names(temp) = c("Year", paste0("CPUE of R", r) )       #算各年平均值
    std.CPUE = merge(std.CPUE, temp, by="Year", all=T)

    yup = max(nominal[,2], temp[,2], na.rm=T)    #界定圖的y軸上線
  windows(height=5, width=10)                 #nominal CPUE和標準化CPUE
    plot(nominal[,1], nominal[,2],
         ylim = c(0,yup), type = "b", main = paste0("BET R ",r), xlab = "Year", ylab = "CPUE")
    lines(std.CPUE[,1], std.CPUE[,r-1], type="b", pch=2, col=2)
    legend("topright", legend = c("nom. CPUE","std. CPUE"), pch = 1:2, col = 1:2)
  savePlot(paste0("圖表\\CPUE_BET_R_",r),"png"); dev.off()

  windows(height = 5, width = 10)
  par(mfrow = c(1,2))                         #殘差直方圖和QQ plot
    hist(fit.glm.a$residual, freq = F, 
         ylim = c(0,0.7), xlim = c(-4,4), xlab = "", main = paste("BET R ",r))
    lines(seq(-4,4,by = 0.001), dnorm(seq(-4,4,by = 0.001), mean(fit.glm.a$residual), var(fit.glm.a$residual)), col = 2)
    qqnorm(fit.glm.a$residual, main = "")
      qqline(fit.glm.a$residual, col = 2)
  savePlot(paste0("圖表\\QQ_BET_R_",r), "png"); dev.off()

  COEFFS = rbind( COEFFS, c( r, fit.glm.a$null.deviance, fit.glm.a$df.null, fit.glm.a$deviance, fit.glm.a$df.res,
                                1-fit.glm.a$deviance/fit.glm.a$null.deviance,                                        #R^2
                                1-(fit.glm.a$deviance/fit.glm.a$df.res)/(fit.glm.a$null.deviance/fit.glm.a$df.null), #adj. R^2,
                                AIC(fit.glm.a), BIC(fit.glm.a) ) )
} #r
write.csv(COEFFS[-1,], "圖表\\BET_Coefficients.csv", row.names=F)
write.csv(std.CPUE, "圖表\\BET_standard.CPUE.csv", row.names=F)
