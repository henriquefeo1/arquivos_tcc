

# ------ Funções ------
# Função para converter o tipo de opção para numérico
# Obrigatório = 1
# Diferencial = 2
# Não Obrigatório = 0
op_to_number <- function(texto) {
  if (substr(texto, 1, 3) == "Obr") {
    return(1)
  } else if (texto == "Diferencial") {
    return(2)
  } else {
    return(0)
  }
}

# Função para converter "Sim" ou "Não" em 1 e 0
bol_to_number <- function(texto) {
  if (texto == "Sim") {
    return(1)
  } else {
    return(0)
  }
}

# Função para converter o periodo
func_periodo <- function(tipo, texto) {
  if(tipo == 1){
    if(texto == "Diurno"){
      return(1)
    } else{
      return(0)
    }
  } 
  else if (tipo == 2){
    if(texto == "Diurno"){
      return(0)
    } else{
      return(1)
    }
  }
}

# Função para converter o periodo
func_tipo_escola <- function(texto) {
  if (texto == "Federal") {
    return(1)
  } else if (texto == "Estadual") {
    return(2)
  } else if (texto == "Municipal") {
    return(3)
  } else {
    return(4)
  }
}

# Função para verifica obrigatoriedade, se é obrigatório e o input é diferente da base, descarta
func_filtro <- function(opcao, var1, var2) {
  if (opcao == 1 && var1 != var2) {
    return("fora")
  } else {
    return("ok")
  }
}

# Função que verifica se a opção é "diferencial" ou não e se for igual é busca, retorna 1
func_igual <- function(opcao, var1, var2) {
  if ((opcao == 1 || opcao == 2) && var1 == var2) {
    return(1)
  } else {
    return(0)
  }
}

# Função para verificar se o período a ser buscado está correto
func_igual_periodo <- function(opcao, diurno, noturno, periodo_busca) {
  if (periodo_busca == "Diurno"){
    if ((opcao == 1 || opcao == 2) && diurno == 1){
      return(1)
    } else {
      return(0)
    }
  }else if (periodo_busca == "Noturno"){
    if ((opcao == 1 || opcao == 2) && noturno == 1){
      return(1)
    } else {
      return(0)
    }
  }else{
    return(0)
  }
}

# Converte a opção para binário (obrigatório = 0, demais = 1)
func_considera <- function(opcao) {
  if (opcao == 1 || opcao == 2) {
    return(1)
  } else {
    return(0)
  }
}

# Calcula distancia knn (distância euclidiana)
func_calc_dist <- function(opcao, var1, var2){
    if (opcao == 2){
        return((var1 - var2)^2)
    }
    else {
      return(0)
    }
}


func_filtr <- function(row){

    filtro_final <- "ok"
    # Verifica obrigatoriedade
    if (func_filtro(row$op1, row$TP_DEPENDENCIA, row$tp_escola) == "fora" ||
        func_filtro(row$op2, row$IN_EXAME_SELECAO, row$exame_selec) == "fora" ||
        func_filtro(row$op3, row$IN_DIURNO, func_periodo(1, row$periodo)) == "fora" ||
        func_filtro(row$op3, row$IN_NOTURNO, func_periodo(2, row$periodo)) == "fora" ||
        func_filtro(row$op4, row$IN_EJA_MED, row$possui_eja) == "fora" ||
        func_filtro(row$op5, row$IN_BIBLIOTECA, row$biblioteca) == "fora" ||
        func_filtro(row$op6, row$IN_BIBLIOTECA_SALA_LEITURA, row$sala_leitura) == "fora" ||
        func_filtro(row$op7, row$IN_AUDITORIO, row$auditorio) == "fora" ||
        func_filtro(row$op8, row$IN_QUADRA_ESPORTES, row$quadra) == "fora" ||
        func_filtro(row$op9, row$IN_QUADRA_ESPORTES_COBERTA, row$quadra_coberta) == "fora" ||
        func_filtro(row$op10, row$IN_AREA_VERDE, row$area_verde) == "fora" ||
        func_filtro(row$op11, row$IN_LABORATORIO_CIENCIAS, row$lab_ciencias) == "fora" ||
        func_filtro(row$op12, row$IN_LABORATORIO_INFORMATICA, row$lab_informatica) == "fora" ||
        func_filtro(row$op13, row$IN_COZINHA, row$cozinha) == "fora" ||
        func_filtro(row$op14, row$IN_REFEITORIO, row$refeitorio) == "fora" ||
        func_filtro(row$op91, row$IN_PISCINA, row$piscina) == "fora" ||
        func_filtro(row$op92, row$IN_SALA_ATELIE_ARTES, row$atelie) == "fora" ||
        func_filtro(row$op15, row$IN_DESKTOP_ALUNO, row$desktop) == "fora" ||
        func_filtro(row$op16, row$IN_TABLET_ALUNO, row$tablet) == "fora" ||
        func_filtro(row$op17, row$IN_EQUIP_MULTIMIDIA, row$equip_mult) == "fora" ||
        func_filtro(row$op18, row$IN_EQUIP_COPIADORA, row$copiadora) == "fora" ||
        func_filtro(row$op19, row$IN_INTERNET_APRENDIZAGEM, row$internet) == "fora" ||
        func_filtro(row$op20, row$IN_ALIMENTACAO, row$alimentacao_local) == "fora" ||
        func_filtro(row$op21, row$TP_ATIVIDADE_COMPLEMENTAR, row$ativ_complementar) == "fora" ||
        func_filtro(row$op22, row$IN_BANHEIRO_PNE, row$banheiro_acess) == "fora" ||
        func_filtro(row$op23, row$IN_ACESSIBILIDADE_CORRIMAO, row$corrimao) == "fora" ||
        func_filtro(row$op24, row$IN_ACESSIBILIDADE_ELEVADOR, row$elevador) == "fora" ||
        func_filtro(row$op25, row$IN_ACESSIBILIDADE_SINAL_TATIL, row$sinal_tatil) == "fora" ||
        func_filtro(row$op26, row$IN_ACESSIBILIDADE_RAMPAS, row$rampas) == "fora" ||
        func_filtro(row$op27, row$IN_ACESSIBILIDADE_SINAL_SONORO, row$sinal_sonoro) == "fora") {
        filtro_final <- "fora"
    }
    return(filtro_final)
}

calc_knn <- function(row) {
    
    soma_dist <- 0
  
    # Verifica similaridade (calcula o KNN e contabiliza as características similares)
    soma_dist <- sqrt(func_calc_dist(row$op1, row$TP_DEPENDENCIA, row$tp_escola) +
        func_calc_dist(row$op2, row$IN_EXAME_SELECAO, row$exame_selec) +
        func_calc_dist(row$op3, row$IN_DIURNO, func_periodo(1, row$periodo)) +
        func_calc_dist(row$op3, row$IN_NOTURNO, func_periodo(2, row$periodo)) +
        func_calc_dist(row$op4, row$IN_EJA_MED, row$possui_eja) +
        func_calc_dist(row$op5, row$IN_BIBLIOTECA, row$biblioteca) +
        func_calc_dist(row$op6, row$IN_BIBLIOTECA_SALA_LEITURA, row$sala_leitura) +
        func_calc_dist(row$op7, row$IN_AUDITORIO, row$auditorio) +
        func_calc_dist(row$op8, row$IN_QUADRA_ESPORTES, row$quadra) +
        func_calc_dist(row$op9, row$IN_QUADRA_ESPORTES_COBERTA, row$quadra_coberta) +
        func_calc_dist(row$op10, row$IN_AREA_VERDE, row$area_verde) +
        func_calc_dist(row$op11, row$IN_LABORATORIO_CIENCIAS, row$lab_ciencias) +
        func_calc_dist(row$op12, row$IN_LABORATORIO_INFORMATICA, row$lab_informatica) +
        func_calc_dist(row$op13, row$IN_COZINHA, row$cozinha) +
        func_calc_dist(row$op14, row$IN_REFEITORIO, row$refeitorio) +
        func_calc_dist(row$op91, row$IN_PISCINA, row$piscina) +
        func_calc_dist(row$op92, row$IN_SALA_ATELIE_ARTES, row$atelie) +
        func_calc_dist(row$op15, row$IN_DESKTOP_ALUNO, row$desktop) +
        func_calc_dist(row$op16, row$IN_TABLET_ALUNO, row$tablet) +
        func_calc_dist(row$op17, row$IN_EQUIP_MULTIMIDIA, row$equip_mult) +
        func_calc_dist(row$op18, row$IN_EQUIP_COPIADORA, row$copiadora) +
        func_calc_dist(row$op19, row$IN_INTERNET_APRENDIZAGEM, row$internet) +
        func_calc_dist(row$op20, row$IN_ALIMENTACAO, row$alimentacao_local) +
        func_calc_dist(row$op21, row$TP_ATIVIDADE_COMPLEMENTAR, row$ativ_complementar) +
        func_calc_dist(row$op22, row$IN_BANHEIRO_PNE, row$banheiro_acess) +
        func_calc_dist(row$op23, row$IN_ACESSIBILIDADE_CORRIMAO, row$corrimao) +
        func_calc_dist(row$op24, row$IN_ACESSIBILIDADE_ELEVADOR, row$elevador) +
        func_calc_dist(row$op25, row$IN_ACESSIBILIDADE_SINAL_TATIL, row$sinal_tatil) +
        func_calc_dist(row$op26, row$IN_ACESSIBILIDADE_RAMPAS, row$rampas) +
        func_calc_dist(row$op27, row$IN_ACESSIBILIDADE_SINAL_SONORO, row$sinal_sonoro))

     return(soma_dist)
}


calc_indice <- function(row) {
      
    aux_calc <- func_igual(row$op1, row$TP_DEPENDENCIA, row$tp_escola) +
        func_igual(row$op2, row$IN_EXAME_SELECAO, row$exame_selec) +
        func_igual(row$op3, row$IN_DIURNO, row$diurno) +
        func_igual(row$op3, row$IN_NOTURNO, row$noturno) +
        func_igual(row$op4, row$IN_EJA_MED, row$possui_eja) +
        func_igual(row$op5, row$IN_BIBLIOTECA, row$biblioteca) +
        func_igual(row$op6, row$IN_BIBLIOTECA_SALA_LEITURA, row$sala_leitura) +
        func_igual(row$op7, row$IN_AUDITORIO, row$auditorio) +
        func_igual(row$op8, row$IN_QUADRA_ESPORTES, row$quadra) +
        func_igual(row$op9, row$IN_QUADRA_ESPORTES_COBERTA, row$quadra_coberta) +
        func_igual(row$op10, row$IN_AREA_VERDE, row$area_verde) +
        func_igual(row$op11, row$IN_LABORATORIO_CIENCIAS, row$lab_ciencias) +
        func_igual(row$op12, row$IN_LABORATORIO_INFORMATICA, row$lab_informatica) +
        func_igual(row$op13, row$IN_COZINHA, row$cozinha) +
        func_igual(row$op14, row$IN_REFEITORIO, row$refeitorio) +
        func_igual(row$op91, row$IN_PISCINA, row$piscina) +
        func_igual(row$op92, row$IN_SALA_ATELIE_ARTES, row$atelie) +
        func_igual(row$op15, row$IN_DESKTOP_ALUNO, row$desktop) +
        func_igual(row$op16, row$IN_TABLET_ALUNO, row$tablet) +
        func_igual(row$op17, row$IN_EQUIP_MULTIMIDIA, row$equip_mult) +
        func_igual(row$op18, row$IN_EQUIP_COPIADORA, row$copiadora) +
        func_igual(row$op19, row$IN_INTERNET_APRENDIZAGEM, row$internet) +
        func_igual(row$op20, row$IN_ALIMENTACAO, row$alimentacao_local) +
        func_igual(row$op21, row$TP_ATIVIDADE_COMPLEMENTAR, row$ativ_complementar) +
        func_igual(row$op22, row$IN_BANHEIRO_PNE, row$banheiro_acess) +
        func_igual(row$op23, row$IN_ACESSIBILIDADE_CORRIMAO, row$corrimao) +
        func_igual(row$op24, row$IN_ACESSIBILIDADE_ELEVADOR, row$elevador) +
        func_igual(row$op25, row$IN_ACESSIBILIDADE_SINAL_TATIL, row$sinal_tatil) +
        func_igual(row$op26, row$IN_ACESSIBILIDADE_RAMPAS, row$rampas) +
        func_igual(row$op27, row$IN_ACESSIBILIDADE_SINAL_SONORO, row$sinal_sonoro)
    
      tot_possibilidades <- func_considera(row$op1) + func_considera(row$op2) +
        func_considera(row$op3) + func_considera(row$op4) +
        func_considera(row$op5) + func_considera(row$op6) +
        func_considera(row$op7) + func_considera(row$op8) +
        func_considera(row$op9) + func_considera(row$op10) +
        func_considera(row$op11) + func_considera(row$op12) +
        func_considera(row$op13) + func_considera(row$op14) +
        func_considera(row$op15) + func_considera(row$op16) +
        func_considera(row$op17) + func_considera(row$op18) +
        func_considera(row$op19) + func_considera(row$op20) +
        func_considera(row$op21) + func_considera(row$op22) +
        func_considera(row$op23) + func_considera(row$op24) +
        func_considera(row$op25) + func_considera(row$op26) +
        func_considera(row$op27) +
        func_considera(row$op91) + func_considera(row$op92)

    if (tot_possibilidades == 0){
        indice_match <- 1
    }else{
        indice_match <- aux_calc/tot_possibilidades
    }

    return(indice_match)
}

# Função para calcular a distância haversina entre dois pares de coordenadas
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Raio médio da Terra em quilómetros
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  
  a <- sin(dlat/2) * sin(dlat/2) +
       cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * 
       sin(dlon/2) * sin(dlon/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  distance <- R * c
  return(distance)
}

# Função para converter binário para texto
bin_to_text <- function(variavel) {
  if (variavel == 1) {
    return("Sim")
  } else {
    return("Não")
  }
}


# Função para converter tipo de escola
val_to_escola <- function(variavel) {
  if (texto == 1) {
    return("Federal")
  } else if (texto == 2) {
    return("Estadual")
  } else if (texto == 3) {
    return("Municipal")
  } else {
    return("Privada")
  }
}


# Função para converter o periodo
get_periodo <- function(diurno, noturno) {
  if (diurno == 1 && noturno == 1) {
    return("Diurno e noturno")
  } else if (diurno == 1 && noturno == 0) {
    return("Diurno")
  } else if (diurno == 0 && noturno == 1) {
    return("Noturno")
  } else {
    return("Sem informação")
  }
}

# Função para agrupar o endereço




# Função para informar o número telefônico