setwd(getwd())
dts.all = read.csv("��l���\\�C��OFDC����_��Cluster.csv")
dim(dts.all)
dts.all[1:2,]
library("psych")    #geometric.mean

for(fish in c("BET","YFT") ) {
  for(r in 3:6){                                

    NZ = c(); COEFFS = c(); dts.R = dts.all[dts.all$reg == r,] 
      dts.R$zz = 1
      if(fish == "BET")  dts.R[dts.R$BET_N == 0,]$zz = 0
      if(fish == "YFT")  dts.R[dts.R$YFT_N == 0,]$zz = 0
      dts.R1 = dts.R[dts.R$zz == 1,]

  ####nominal�p��
        if(fish == "BET")  agg = aggregate(betCPUE ~ Year ,data = dts.R1 , FUN = geometric.mean)           #
        if(fish == "YFT")  agg = aggregate(yftCPUE ~ Year ,data = dts.R1 , FUN = geometric.mean)
      M2 = data.frame(matrix(c(c(1964:2015), rep(NA,2015-1964+1)), nrow = (2015-1964+1), ncol=2))
      colnames(M2) = names(agg)
      M2[ M2$Year %in% c(agg$Year),][,c(2)] = agg[,c(2)]
    NZ = M2 

        if(fish == "BET")  fit.glm.a = glm(log(betCPUE) ~ as.factor(Year), data = dts.R1)              #
        if(fish == "YFT")  fit.glm.a = glm(log(yftCPUE) ~ as.factor(Year), data = dts.R1)
      agg.a = c(exp(coefficients(fit.glm.a)["(Intercept)"]), 
                exp(coefficients(fit.glm.a)[paste0("as.factor(Year)",1965:2015)] + coefficients(fit.glm.a)["(Intercept)"]))
  
      fit.glm.b = glm(zz ~ as.factor(Year) + as.factor(Quarter) +
                           as.factor(Longitude) + as.factor(Latitude) +
                           as.factor(Cluster) + as.factor(CTlevel), 
                           data = dts.R, family = binomial(link=probit))
      glm.b = predict(fit.glm.b, data = dts.R, type = "response")
        dts.R$b = glm.b
      agg.b = aggregate(b ~ Year, data = dts.R, FUN = mean) 
      M2[ M2$Year %in% c(agg.b$Year),][,c(2)] = agg.b[,c(2)]
        
      agg = agg.a * M2[,2] 
    NZ$only_year = agg 

          if(fish == "BET")  fit.glm.a = glm(log(betCPUE) ~ as.factor(Year) + as.factor(Quarter), data = dts.R1)              #
          if(fish == "YFT")  fit.glm.a = glm(log(yftCPUE) ~ as.factor(Year) + as.factor(Quarter), data = dts.R1) 
        agg.a = c(exp(coefficients(fit.glm.a)["(Intercept)"]), 
                  exp(coefficients(fit.glm.a)[paste0("as.factor(Year)",1965:2015)] + coefficients(fit.glm.a)["(Intercept)"]))
        
        agg = agg.a * M2[,2] 
    NZ$plus_quarter = agg 

          if(fish == "BET")  fit.glm.a = glm(log(betCPUE) ~ as.factor(Year) + as.factor(Quarter) + as.factor(Longitude), data = dts.R1)              #
          if(fish == "YFT")  fit.glm.a = glm(log(yftCPUE) ~ as.factor(Year) + as.factor(Quarter) + as.factor(Longitude), data = dts.R1) 
        agg.a = c(exp(coefficients(fit.glm.a)["(Intercept)"]), 
                  exp(coefficients(fit.glm.a)[paste0("as.factor(Year)",1965:2015)] + coefficients(fit.glm.a)["(Intercept)"]))
        
        agg = agg.a * M2[,2] 
    NZ$plus_lon = agg 

          if(fish == "BET")  fit.glm.a = glm(log(betCPUE) ~ as.factor(Year) + as.factor(Quarter) + as.factor(Longitude) + as.factor(Latitude), data = dts.R1)              #
          if(fish == "YFT")  fit.glm.a = glm(log(yftCPUE) ~ as.factor(Year) + as.factor(Quarter) + as.factor(Longitude) + as.factor(Latitude), data = dts.R1)
        agg.a= c(exp(coefficients(fit.glm.a)["(Intercept)"]), 
                 exp(coefficients(fit.glm.a)[paste0("as.factor(Year)",1965:2015)] + coefficients(fit.glm.a)["(Intercept)"]))
        
        agg = agg.a * M2[,2] 
    NZ$plus_lat = agg 

          if(fish == "BET")  fit.glm.a = glm(log(betCPUE) ~ as.factor(Year) + as.factor(Quarter) + as.factor(Longitude) + as.factor(Latitude) + as.factor(Cluster), data = dts.R1)              #
          if(fish == "YFT")  fit.glm.a = glm(log(yftCPUE) ~ as.factor(Year) + as.factor(Quarter) + as.factor(Longitude) + as.factor(Latitude) + as.factor(Cluster), data = dts.R1) 
        agg.a = c(exp(coefficients(fit.glm.a)["(Intercept)"]), 
                  exp(coefficients(fit.glm.a)[paste0("as.factor(Year)",1965:2015)] + coefficients(fit.glm.a)["(Intercept)"]))
        
        agg = agg.a * M2[,2] 
    NZ$plus_cluster = agg 

          if(fish == "BET")  fit.glm.a = glm(log(betCPUE) ~ as.factor(Year) + as.factor(Quarter) + as.factor(Longitude) + as.factor(Latitude) + as.factor(Cluster) + as.factor(CTlevel), data = dts.R1)              #
          if(fish == "YFT")  fit.glm.a = glm(log(yftCPUE) ~ as.factor(Year) + as.factor(Quarter) + as.factor(Longitude) + as.factor(Latitude) + as.factor(Cluster) + as.factor(CTlevel), data = dts.R1)              # 
        agg.a = c(exp(coefficients(fit.glm.a)["(Intercept)"]), 
                  exp(coefficients(fit.glm.a)[paste0("as.factor(Year)",1965:2015)] + coefficients(fit.glm.a)["(Intercept)"]))

        agg = agg.a * M2[,2] 
    NZ$plus_ctlevel = agg 

#---->�ƭ��x�s�U�Ӧn�F 
    write.csv(NZ, paste0("��2(a)_��X�󥭧�\\", fish, "\\�]�l�֥[CPUE�зǤƵ��GR", r, "_", fish, ".csv"), row.names=F)

  #---->��(a) 
yup = ceiling(max( subset(NZ,select = -c(1:2)) , na.rm = T))

  layout(matrix(1:6, ncol = 1), widths = 1, heights = c(1.56,1,1,1,1,1.56) , respect = FALSE)
    par(mar = c(0, 4, 4, 2))
      plot(NZ[,1],NZ[,3], 
           type = "l", ylim = c(0,yup), xaxt = "n", yaxt="n", ylab = "", xlab = "", main = paste(fish,"_R",r))
      points(NZ[,1], NZ[,3], pch = 19)
        axis(side = 2, at = seq(0.5,yup,yup/5), labels = T)
        text(1966,yup,names(NZ)[3])
    par(mar = c(0, 4, 0, 2))
      plot(NZ[,1], NZ[,3], 
           type = "l", ylim = c(0,yup), xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "gray")
      points(NZ[,1], NZ[,3], pch = 19, col = "gray")
        lines(NZ[,1], NZ[,4], type="l", ylim = c(0,yup))
        points(NZ[,1], NZ[,4], pch = 19)
        axis(side = 2, at = seq(0.5,yup,yup/5), labels = T)
        text(1966, yup, names(NZ)[4])
      plot(NZ[,1], NZ[,4], 
           type = "l", ylim = c(0,yup), xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "gray")
      points(NZ[,1], NZ[,4], pch = 19, col = "gray")
        lines(NZ[,1], NZ[,5], type = "l", ylim = c(0,yup))
        points(NZ[,1], NZ[,5], pch = 19)
        axis(side = 2, at=seq(0.5,yup,yup/5), labels = T)
        text(1966, yup, names(NZ)[5])
      plot(NZ[,1], NZ[,5], type = "l", ylim = c(0,yup), xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "gray")
      points(NZ[,1], NZ[,5], pch = 19, col = "gray")
        lines(NZ[,1], NZ[,6], type = "l", ylim = c(0,yup))
        points(NZ[,1], NZ[,6], pch = 19)
        axis(side = 2, at = seq(0.5,yup,yup/5), labels = T)
        text(1966, yup, names(NZ)[6])
      plot(NZ[,1], NZ[,6], type = "l", ylim = c(0,yup), xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "gray")
      points(NZ[,1], NZ[,6], pch = 19, col = "gray")
        lines(NZ[,1], NZ[,7], type = "l", ylim = c(0,yup))
        points(NZ[,1], NZ[,7], pch = 19)
        axis(side = 2, at = seq(0.5,yup,yup/5), labels = T)
        text(1966, yup, names(NZ)[7])
    par(mar = c(4, 4, 0, 2))
      plot(NZ[,1], NZ[,7], type = "l", ylim = c(0,yup), yaxt = "n", ylab = "", xlab = "", col = "gray")
      points(NZ[,1], NZ[,7], pch = 19, col = "gray")
        lines(NZ[,1], NZ[,8], type = "l", ylim = c(0,yup))
        points(NZ[,1], NZ[,8], pch = 19)
        axis(side = 2, at = seq(0.5,yup,yup/5), labels = T)
        text(1966, yup, names(NZ)[8])
savePlot(paste0("��2(a)_��X�󥭧�\\", fish, "\\��a�]�l�֥[CPUE�зǤƵ��GR", r, "_", fish), "png"); dev.off()

#-----> �n�}�l�e��(b)�F�A���p��Ѽ� 
  #-----> eq(2a)                                
    ITCP = as.numeric(fit.glm.a$coefficients["(Intercept)"])                       #�I�Z���Y�� 
      avg.Quarter = ITCP
      for(QQ in sort(unique(dts.R1$Quarter))[-1]) avg.Quarter = as.numeric(avg.Quarter + (sum(dts.R1$Quarter == QQ)*fit.glm.a$coefficients[paste0("as.factor(Quarter)",QQ)]) / nrow(dts.R1) )
      avg.Longitude = ITCP
      for(LO in sort(unique(dts.R1$Longitude))[-1]) avg.Longitude = as.numeric(avg.Longitude + (sum(dts.R1$Longitude == LO)*fit.glm.a$coefficients[paste0("as.factor(Longitude)",LO)]) / nrow(dts.R1) )
      avg.Latitude = ITCP
      for(LA in sort(unique(dts.R1$Latitude))[-1]) avg.Latitude = as.numeric(avg.Latitude + (sum(dts.R1$Latitude == LA)*fit.glm.a$coefficients[paste0("as.factor(Latitude)",LA)]) / nrow(dts.R1))
      avg.Cluster = ITCP
      for(CL in sort(unique(dts.R1$Cluster))[-1]) avg.Cluster = as.numeric(avg.Cluster + (sum(dts.R1$Cluster == CL)*fit.glm.a$coefficients[paste0("as.factor(Cluster)",CL)]) / nrow(dts.R1))
      avg.CTlevel = ITCP
      for(CT in sort(unique(dts.R1$CTlevel))[-1]) avg.CTlevel = as.numeric(avg.CTlevel + (sum(dts.R1$CTlevel == CT)*fit.glm.a$coefficients[paste0("as.factor(CTlevel)",CT)]) / nrow(dts.R1))

  #-----> eq(2b) �A eq(4)��exp                                
      Ifc = c()
    for(YY in sort(unique(dts.all$Year)) )  {
      dts.R1Y = dts.R1[dts.R1$Year == YY,]
      if( nrow(dts.R1Y) == 0 ) {  
        Ifc = rbind(Ifc, c(YY,NA,NA,NA,NA,NA))
      } else {
        delta.Quarter = ITCP
        for( QQ in sort(unique(dts.R1$Quarter))[-1] ) delta.Quarter = as.numeric( delta.Quarter + (sum(dts.R1Y$Quarter == QQ)*fit.glm.a$coefficients[paste0("as.factor(Quarter)",QQ)]) / nrow(dts.R1Y) )
        delta.Quarter = delta.Quarter - avg.Quarter
          delta.Longitude = ITCP
          for( LO in sort(unique(dts.R1$Longitude))[-1] )  delta.Longitude = as.numeric( delta.Longitude + (sum(dts.R1Y$Longitude == LO)*fit.glm.a$coefficients[paste0("as.factor(Longitude)",LO)]) / nrow(dts.R1Y) )
          delta.Longitude = delta.Longitude - avg.Longitude
        delta.Latitude = ITCP
        for( LA in sort(unique(dts.R1$Latitude))[-1] ) delta.Latitude = as.numeric( delta.Latitude + (sum(dts.R1Y$Latitude == LA)*fit.glm.a$coefficients[paste0("as.factor(Latitude)",LA)]) / nrow(dts.R1Y) )
        delta.Latitude = delta.Latitude - avg.Latitude
          delta.Cluster = ITCP
          for( CL in sort(unique(dts.R1$Cluster))[-1] ) delta.Cluster = as.numeric( delta.Cluster + (sum(dts.R1Y$Cluster == CL)*fit.glm.a$coefficients[paste0("as.factor(Cluster)",CL)]) / nrow(dts.R1Y) )
          delta.Cluster = delta.Cluster - avg.Cluster
        delta.CTlevel = ITCP
        for( CT in sort(unique(dts.R1$CTlevel))[-1] ) delta.CTlevel = as.numeric( delta.CTlevel + (sum(dts.R1Y$CTlevel == CT)*fit.glm.a$coefficients[paste0("as.factor(CTlevel)",CT)]) / nrow(dts.R1Y) )
        delta.CTlevel = delta.CTlevel - avg.CTlevel
          Ifc = rbind(Ifc, c(YY, delta.Quarter, delta.Longitude, delta.Latitude, delta.Cluster, delta.CTlevel))
      }
    }     
    Influence = Ifc
    Influence[, 2:6] = exp(Ifc[, 2:6])
    Influence = data.frame(Influence)
    names(Influence) = c('Year', 'delta.Quarter', 'delta.Longitude', 'delta.Latitude', 'delta.Cluster', 'delta.CTlevel')
    Influence = rbind(Influence, c(NA, exp(mean(Ifc[,2], na.rm = T)) - 1 , exp(mean(Ifc[,3], na.rm = T)) - 1 ,                                        #�p��eq(5)�A�Yoverall influence 
                                       exp(mean(Ifc[,4], na.rm = T)) - 1 , exp(mean(Ifc[,5], na.rm = T)) - 1 , exp(mean(Ifc[,6],na.rm = T)) - 1) )    #�p��eq(5)�A�Yoverall influence 
    write.csv(Influence, paste0("��2(b)\\", fish, "\\Influence�Y��_", r ,".csv"), row.names=F)

  #---->��(b)
INFL = Influence[1:length(unique(dts.all$Year)),]
yup = round(max( subset(INFL,select = -1), na.rm=T), 1) + 0.1
ydown = round(min( subset(INFL,select = -1), na.rm=T), 1) - 0.1

  layout(matrix(1:5, ncol = 1), widths = 1, heights = c(1.38,1,1,1,1.38), respect = FALSE)
    par(mar = c(0, 4, 4, 2))
      plot(INFL[,1], INFL[,2], 
           type = "l", ylim = c(ydown,yup), xaxt = "n", yaxt = "n", ylab = "", xlab = "", main = paste(fish, "_R", r))
      points(INFL[,1], INFL[,2], pch = 19)
        axis(side = 2, at = seq(ydown,yup-0.2, 0.2), labels = T)
        text(1966, yup, names(Influence)[2])
        abline(h = 1, lty = 3, col = "grey")
    par(mar = c(0, 4, 0, 2))
      plot(INFL[,1], INFL[,3], 
           type = "l", ylim = c(ydown, yup), xaxt = "n", yaxt = "n", ylab = "", xlab = "")
      points(INFL[,1], INFL[,3], pch = 19)
        axis(side = 2, at = seq(ydown, yup-0.2, 0.2), labels = T)
        text(1966, yup, names(Influence)[3])
        abline(h = 1, lty = 3, col = "grey")
      plot(INFL[,1], INFL[,4], type = "l", ylim = c(ydown, yup), xaxt = "n", yaxt = "n", ylab = "", xlab = "")
      points(INFL[,1], INFL[,4], pch = 19)
        axis(side = 2, at = seq(ydown, yup-0.2, 0.2), labels = T)
        text(1966, yup, names(Influence)[4])
        abline(h = 1, lty = 3, col = "grey")
      plot(INFL[,1], INFL[,5], 
           type = "l", ylim = c(ydown,yup), xaxt = "n", yaxt = "n", ylab = "", xlab = "")
      points(INFL[,1], INFL[,5], pch = 19)
        axis(side = 2, at = seq(ydown,yup-0.2, 0.2), labels = T)
        text(1966, yup, names(Influence)[5])
        abline(h = 1, lty = 3, col = "grey")
    par(mar = c(4, 4, 0, 2))
      plot(INFL[,1], INFL[,6], type = "l", ylim = c(ydown,yup), yaxt = "n", ylab = "", xlab = "")
      points(INFL[,1], INFL[,6], pch = 19)
        axis(side = 2, at = seq(ydown,yup-0.2, 0.2), labels = T)
        text(1966, yup, names(Influence)[6])
        abline(h = 1, lty = 3, col = "grey")
savePlot(paste0("��2(b)\\", fish, "\\Influence�v�B_", r), "png"); dev.off()

gc()
    }       #r=3:6 
}           #for fish 

