# Probability of winning, based on type of game (aposta) and number of matches required (acerto)
prob <- function(aposta, acerto){
  choose(aposta,acerto)*choose(60-aposta,6-acerto)/choose(60, 6)
}
# Número de prêmios da quadra, para quem acertou a sena
funqua6 <- function(numb){
  ((numb-7)^2+(numb-7))*15/2
}
# Número de prêmios da quadra, para quem acertou a quina
funqua5 <- function(numb){
  ((numb-7)^2+3*(numb-7)+2)*5/2
}
# Número de prêmios da quadra, para quem acertou a quadra
funqua4 <- function(numb){
  (numb-5)*((numb-5)+1)/2
}
# Número de prêmios da quina, para quem acertou a sena
funqui6 <- function(numb){
  6*(numb-6)
}
# Número de prêmios da quina, para quem acertou a quina
funqui5 <- function(numb){
  numb-5
}
valor_aposta <- function(aposta){
  c(4.5, 31.5, 126, 378, 945, 2079, 4158, 7722, 13513.5, 22522.5)[aposta-5]
}
sena_fun <- function(numb = 6, 
                     premio = 50e6){
  prob_total <<- prob(numb,6) + prob(numb,5) + prob(numb,4)
  
  # Não há relação linear entre o ganho na mega-sena e o valor pago nas quinas/quadras
  # Contudo, pode-se tentar uma média por intervalos para deixar mais realista
  if(premio < 5e6){
    premio_quina <<- 27628.52
  } else {
    if(premio < 1e7){
      premio_quina <<- 30091.51
    } else {
      if(premio < 5e7){
        premio_quina <<- 29225.1
      } else {
        if(premio < 1e8){
          premio_quina <<- 30680.91
        } else {
          if(premio < 1.5e8){
            premio_quina <<- 41224.19
          } else {
            if(premio < 2e8){
              premio_quina <<- 43578.26
            } else {
              premio_quina <<- 23461.74
            }
          }
        }
      }
    }
  }
  
  # Apresenta perda média por aposta (R$)
  premio_quadra <- premio_quina*0.01887368
    # Bilhete
    (1 - prob_total) * (-valor_aposta(numb))  + 

    # Prob sena * ganho esperado (considerando o rateio)   
    prob(numb, 6) * (premio + funqui6(numb)*premio_quina + funqua6(numb)*premio_quadra) + 
    
    # Prob quina * ganho esperado (considerando o rateio)  
    prob(numb, 5) * (funqui5(numb)*premio_quina + funqua5(numb)*premio_quadra) +         
    
    # Prob quadra * ganho esperado (considerando o rateio)  
    prob(numb, 4) * (funqua5(numb)*premio_quadra)                               
}


# PLOT MODEL
mm <- seq(10e6, 300e6, length=100)
nn <- seq(6, 15, by = 1)
zz <- outer(nn, mm, sena_fun)
persp(nn, mm, zz, theta = 40, phi = 30, shade = 0.5,
      xlab = "Números apostados",
      ylab = "Valor da mega sena",
      zlab = "Ganho (R$) por jogo",
      ylim = c(1e6, 300e6))
