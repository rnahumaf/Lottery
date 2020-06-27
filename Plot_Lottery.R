# OPEN MEGA SENA SHEET FROM CAIXA BANK WEBSITE
# http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena/
# http://www1.caixa.gov.br/loterias/_arquivos/loterias/D_megase.zip

library(xlsx)
library(ggplot2)

mega_sena <- read.xlsx("MegaSena.xlsx", sheetIndex = 1)

# variável separada para manipulação
mega_earn <- mega_sena[,which(colnames(mega_sena) %in% c("Concurso", "Estimativa_Premio", "Rateio_Sena", "Rateio_Quadra", "Rateio_Quina", "Ganhadores_Sena"))]
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







# CREATE ADDITIONAL PLOTS TO UNDERSTAND THE RELATIONSHIP BETWEEN PRICES

# GANHO SENA X JOGADORES DA SENA (ganhadores ~ Estimativa*2e-8)
library(ggpubr)
ggplot(mega_ajust, aes(x = Estimativa_Premio, y = Ganhadores_Sena)) +
  geom_point(aes(x = Estimativa_Premio, y = Ganhadores_Sena, alpha = Concurso), color = "red") +
  xlab("Estimativa de Premio") + ylab("Ganhadores na sena") +
  geom_smooth(method="lm", aes(x = Estimativa_Premio, y = Ganhadores_Sena)) +
  theme(legend.position="") +
  stat_cor(label.y = 10) +
  stat_regline_equation(label.y = 5)

# GANHO QUINA X QUADRA (quina ~ quadra/0.013)
library(ggpubr)
ggplot(mega_ajust, aes(x = Rateio_Quina, y = Rateio_Quadra)) +
  geom_point(aes(x = Rateio_Quina, y = Rateio_Quadra, alpha = Concurso), color = "red") +
  xlab("Ganho na quina") + ylab("Ganho na quadra") +
  geom_smooth(method="lm", aes(x = Rateio_Quina, y = Rateio_Quadra)) +
  theme(legend.position="") +
  stat_cor(label.y = 1500) +
  stat_regline_equation(label.y = 1250)

# GANHO QUINA X SENA (quina ~ sena/550)
library(ggpubr)
ggplot(mega_ajust[which(mega_ajust$Rateio_Sena>0),], aes(x = Rateio_Quina, y = Rateio_Sena)) +
  geom_point(aes(x = Rateio_Quina, y = Rateio_Sena, alpha = Concurso), color = "red") +
  xlab("Ganho na quina") + ylab("Ganho na sena") +
  geom_smooth(method="lm", aes(x = Rateio_Quina, y = Rateio_Sena)) +
  theme(legend.position="") +
  stat_cor(label.y = 2e8) +
  stat_regline_equation(label.y = 1e8)

# GANHO QUINA X SENA (quina ~ sena/470 a sena/430)
library(ggpubr)
ggplot(mega_ajust[which(mega_ajust$Rateio_Sena==0 & mega_ajust$Concurso%%5!=0 &
                          mega_ajust$Estimativa_Premio>2e7),], aes(x = Rateio_Quina, y = Estimativa_Premio)) +
  geom_point(aes(x = Rateio_Quina, y = Estimativa_Premio, alpha = Concurso), color = "red") +
  xlab("Ganho na quina") + ylab("Ganho na sena") +
  geom_smooth(method="lm", aes(x = Rateio_Quina, y = Estimativa_Premio)) +
  theme(legend.position="") +
  stat_cor(label.y = 2e8) +
  stat_regline_equation(label.y = 1e8)
