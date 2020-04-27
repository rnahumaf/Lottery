# OPEN MEGA SENA SHEET FROM CAIXA BANK WEBSITE
# http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena/
# http://www1.caixa.gov.br/loterias/_arquivos/loterias/D_megase.zip

library(xlsx)
library(ggplot2)

mega_sena <- read.xlsx("MegaSena.xlsx", sheetIndex = 1)

# variável separada para manipulação
mega_earn <- mega_sena[,which(colnames(mega_sena) %in% c("Concurso", "Estimativa_Premio", "Rateio_Sena", "Rateio_Quadra", "Rateio_Quina"))]
mega_earn <- mega_earn[which(mega_earn$Estimativa_Premio>0),]
mega_earn <- mega_earn[which(mega_earn$Concurso>=868),]

# Ajustar coluna da estimativa de prêmio
mega_ajust <- mega_earn[-1,]
for(i in 1:nrow(mega_ajust)){
  mega_ajust$Estimativa_Premio[i] <- mega_earn$Estimativa_Premio[i]
}

sena_05 <- mega_ajust[which(mega_ajust$Concurso%%5==0),]
sena_else <- mega_ajust[which(mega_ajust$Concurso%%5!=0),]

ggplot(mega_ajust) +
  # geom_point(data = sena_05, aes(x = Rateio_Sena, y = Estimativa_Premio), color = "black", alpha = 0.2) +
  # geom_point(data = sena_else, aes(x = Rateio_Sena, y = Estimativa_Premio), color = "blue", alpha = 0.2) +
  geom_point(aes(x = Rateio_Sena, y = Estimativa_Premio, alpha = Concurso), color = "black") +
  geom_point(aes(x = Rateio_Quina, y = Estimativa_Premio, alpha = Concurso), color = "red") +
  geom_point(aes(x = Rateio_Quadra, y = Estimativa_Premio, alpha = Concurso), color = "purple") +
  scale_x_log10() + scale_y_log10() +
  xlab("Ganho individual") + ylab("Estimativa de prêmio") +
  theme(legend.position="") +
  geom_vline(xintercept = mean(mega_ajust$Rateio_Quina)) +
  geom_vline(xintercept = mean(mega_ajust$Rateio_Quadra)) +
  annotate("text", x = mean(mega_ajust$Rateio_Quina)*3.5, y = 1e+06, label=paste0(floor(mean(mega_ajust$Rateio_Quina)/1000), " mil reais"))+
  annotate("text", x = mean(mega_ajust$Rateio_Quadra)*3.5, y = 1e+06, label=paste0(floor(mean(mega_ajust$Rateio_Quadra)), " reais"))

