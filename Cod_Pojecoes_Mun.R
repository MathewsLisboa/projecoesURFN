

#### cÓDIGO DE PROJEÇÃO POPULACIONAL ####

## autores Freire, F. H. M. de A., Gonzaga, M. R., & Gomes, M. M. F. (2020)



projetaDuchesne =
  function(DadosAreaMenor,DadosAreaMaior,TEFAreaMaior,L0AreaMaior,RSN,l0) {
    #Carregando pacote dplyr
    #if (!require(dplyr)) {
    # install.packages("dplyr")
    # library(dplyr)
    #}
    #Quantidade de Grupos Etarios eh igual ao numero de colunas subtraido
    # por 9 (colunas com outras informacoes), dividido por 2
    #Ordenando a base das áreas menores na ordem padrão para execução deste
    # algoritmo
   
     DadosAreaMenor =
      DadosAreaMenor[order(DadosAreaMenor$ARmaior,
                           DadosAreaMenor$Sexo,DadosAreaMenor$NomeMunic),]
    
    NGruposEtarios = (ncol(DadosAreaMenor)-9)/2
    
    ListaProjecoes = ProjecaoUmaArea = list()
    
    #Nomes das areas maiores e anos projetados
    AreasMaiores = as.character(unique(DadosAreaMenor[,2]))
    AnosProjetados = unique(DadosAreaMaior[,2])
    
    #Cria duas listas (uma para o K Original e outra para o K Bayesiano),
    # cada uma com a quantidade de anos projetados de matrizes
    
    ProjecaoKO = ProjecaoKB = 
     replicate(length(AnosProjetados),list(matrix(NA,nrow=0,ncol=(NGruposEtarios-1))))
    
    for (i in 1:length(AreasMaiores)) {
      #Projeta todas as areas menores de uma área maior
      ProjecaoUmaArea =
        projetaUmaAreaMaior(DadosAreaMenor[DadosAreaMenor[,2]==AreasMaiores[i],],
                            DadosAreaMaior[DadosAreaMaior[,1]==AreasMaiores[i],3:length(DadosAreaMaior)],
                            TEFAreaMaior[TEFAreaMaior$UF==AreasMaiores[i],3],
                            t(TEFAreaMaior[TEFAreaMaior$UF==AreasMaiores[i],4:10]),RSN,
                            L0AreaMaior[L0AreaMaior$UF==AreasMaiores[i],4],
                            L0AreaMaior[L0AreaMaior$UF==AreasMaiores[i],3],l0)
      
      #Separa as projecoes do K Orignal e a do K Bayesiano, separando tambem por ano
      for (ano in 1:length(AnosProjetados)) {
        ProjecaoKO[[ano]] = rbind(ProjecaoKO[[ano]],ProjecaoUmaArea[[1]]$Feminino[[1]][[ano]][,-1],
                                  ProjecaoUmaArea[[1]]$Masculino[[1]][[ano]][,-1])
        
        ProjecaoKB[[ano]] =rbind(ProjecaoKB[[ano]],
                                 ProjecaoUmaArea[[1]]$Feminino[[2]][[ano]][,-1],
                                 ProjecaoUmaArea[[1]]$Masculino[[2]][[ano]][,-1])
      }
    }
    
    #Separando população do primeiro e segundo censo e diminuindo um grupo etário

    PopCenso1 = DadosAreaMenor[,7:(7+NGruposEtarios-1)]
    
    PopCenso1 = cbind(PopCenso1[,1:(ncol(PopCenso1)-2)],PopCenso1[,(ncol(PopCenso1)-1)]+PopCenso1[,ncol(PopCenso1)])
    
    PopCenso2 = DadosAreaMenor[,(9+NGruposEtarios):(ncol(DadosAreaMenor)-1)]
    
    PopCenso2 = cbind(PopCenso2[,1:(ncol(PopCenso2)-2)],PopCenso2[,(ncol(PopCenso2)-1)]+
                        PopCenso2[,ncol(PopCenso2)])
    
    #Juntando todas as populações (dos censos e projetadas) em uma lista única
    
    
    PopulacaoKO = append(list(PopCenso1),list(PopCenso2))
    PopulacaoKO = append(PopulacaoKO,ProjecaoKO)
    PopulacaoKB = append(list(PopCenso1),list(PopCenso2))
    PopulacaoKB = append(PopulacaoKB,ProjecaoKB)
    
    
    #Interpolando os dados quinquenais para anuais
    PopulacaoAnualKO = list(as.matrix(PopulacaoKO[[1]]))
    PopulacaoAnualKB = list(as.matrix(PopulacaoKB[[1]]))
    for (i in 2:(length(PopulacaoKO)-1)) {
      PopulacaoAnualKO =
        append(PopulacaoAnualKO,list(as.matrix(PopulacaoKO[[i]])))
      PopulacaoAnualKB =
        append(PopulacaoAnualKB,list(as.matrix(PopulacaoKB[[i]])))
      
      #Calculando o fator de crescimento pelo método exponencial
      
      FatorCrescimentoKO = log(PopulacaoKO[[i+1]]/PopulacaoKO[[i]])
      FatorCrescimentoKB = log(PopulacaoKB[[i+1]]/PopulacaoKB[[i]])
      for (j in 1:4) {
        #Aplicando a interpolação utilizando o método exponencial e adicionando na lista
        
        PopulacaoAnualKO = append(PopulacaoAnualKO,
                                  list(as.matrix(PopulacaoKO[[i]]*exp(FatorCrescimentoKO*0.2*j))))
        
        PopulacaoAnualKB = append(PopulacaoAnualKB,
                                  list(as.matrix(PopulacaoKB[[i]]*exp(FatorCrescimentoKB*0.2*j))))
      }
    }
    
    #Acrescentando a última projeção na lista
    PopulacaoAnualKO = append(PopulacaoAnualKO,
                              list(as.matrix(PopulacaoKO[[length(PopulacaoKO)]] )))
    
    PopulacaoAnualKB = append(PopulacaoAnualKB,
                              list(as.matrix(PopulacaoKB[[length(PopulacaoKB)]] )))
    
    
    #Anos dos censos e anos projetados
    AnosTotais =  c(DadosAreaMenor[1,1],DadosAreaMenor[1,1]+10,
        (DadosAreaMenor[1,1]+11):AnosProjetados[length(AnosProjetados)])
    
    Ano = as.vector(t(replicate(nrow(DadosAreaMenor), AnosTotais)))
    
    #Organizando data frame dos dados das projeções para salvar em arquivo organizadamente
    
    AreaMenorProjetada = do.call(rbind, replicate(length(AnosTotais),
                                                  DadosAreaMenor[,2:6], simplify=FALSE))
    
    AreaMenorProjetada = cbind(Ano,AreaMenorProjetada)
    
    PopulacaoKO = do.call("rbind",PopulacaoAnualKO)
    
    PopulacaoKB = do.call("rbind",PopulacaoAnualKB)
    
    PopulacaoKO = cbind(PopulacaoKO,rowSums(PopulacaoKO))
    
    PopulacaoKB = cbind(PopulacaoKB,rowSums(PopulacaoKB))
    
    AreaMenorProjetadaKO = cbind(AreaMenorProjetada,PopulacaoKO)
    
    AreaMenorProjetadaKB = cbind(AreaMenorProjetada,PopulacaoKB)
    
    #Criando nomes das colunas dos grupos etarios
    
    GruposEtarios = seq(0,(NGruposEtarios-2)*5,5)
    NomesGruposEtarios = ""
    for (i in 1:(length(GruposEtarios)-1)){
      NomesGruposEtarios[i] = paste(as.character(GruposEtarios[i]),"a",as.character(GruposEtarios[i+1]))
    }
    NomesGruposEtarios[i+1]=paste(as.character(GruposEtarios[i+1]),"+",sep = "")
    
    #Nomeando as colunas
    names(AreaMenorProjetadaKO) = names(AreaMenorProjetadaKB) = c(names(AreaMenorProjetadaKO)[1:6],NomesGruposEtarios,"Total")
    
    #Definindo diretorio de saida dos resultados
    print("Digite o diretorio onde deseja que os resultados sejam salvos ou aperte enter para salvar no direterio padrao:")
    Diretorio = scan(what=character(),nlines=1)
    if (!identical(Diretorio,character(0))) {
      if (!dir.exists(Diretorio)) {
        print (paste("O diretorio",Diretorio,"nao existe."))
      } else setwd(Diretorio)
    }
    
    #Salvando em arquivo
    write.csv(AreaMenorProjetadaKO,"Projecoes com K Original.csv",row.names
              = FALSE)
    write.csv(AreaMenorProjetadaKB,"Projecoes com K
Bayesiano.csv",row.names = FALSE)
    print(paste("Resultados salvos em",getwd()))
    #return(AreaMenorProjetadaKB)
  }



# 1. Funções auxiliares
# 1.1 Função de projeção pelo Método Duchesne em cada área menor de uma UF



projetaUmaAreaMaior = function(entr,arma.proj,TFT,TEF,RSN,L0.f,L0.m,l0) {
  
  
  
  #
  #
  #### FUNÇÃO PARA CALCULAR PROJETAR A POPULAÇÃO POR SEXO E GRUPOS ETÁRIOS ####
  # QUINQUENAIS ATÉ 90+ PELO MÉTODO DA RELAÇÃO #
  # INTERCENSITÁRIA DE SOBREVIVÊNCIA DE DUCHESNE.
  #
  # #
  # #
  #
  #
  # Também será preciso colocar na área de trabalho as funções:
  # calculaFatoresCrescimento e calculaFatoresCrescimentoUltimoGrupo usada
  # p/Brasil c/ pop de entrada #
  # até 95+ p/ peq áreas#
  #
  # #
  
 
  #### INFORMAÇÕES IMPORTANTES SOBRE OS DADOS DE ENTRADA: ####
  
  # "entr" é a matriz de dados de entrada das populações por grupo etário
  # das pequenas áreas.
  # Cada linha da matriz "entr" deve ser um município ou área menor
  # (pequena área). Os dados devem estar empilhados. Todos os dados de um
  # sexo (de todas as peq áreas) e depois todos os dados do outro sexo. (1-Fem; 2-Masc, na coluna "sexo" de entr)
  
  # As colunas são pela ordem:
  # Ano_censo1;ARmaior;Meso;Sexo;ARmenor;NomeMunic;0 a 4 anos;5 a 9
  # anos;10 a 14 anos;15 a 19 anos;20 a 24 anos;25 a 29 anos;30 a 34 anos;35
  # a 39 anos;40 a 44 anos;
  # 45 a 49 anos;50 a 54 anos;55 a 59 anos;60 a 64 anos;65 a 69 anos;70 a
  # 74 anos;75 a 79 anos;80 a 84 anos;85 a 89 anos;90 a 94 anos;
  # 95+;Total1;Ano_censo2;0 a 4 anos;
  # 5 a 9 anos;10 a 14 anos;15 a 19 anos;20 a 24 anos;25 a 29 anos;30 a
  # 34 anos;35 a 39 anos;40 a 44 anos;45 a 49 anos;50 a 54 anos;55 a 59
  # anos;60 a 64 anos;
  # 65 a 69 anos;70 a 74 anos;75 a 79 anos;80 a 84 anos;85 a 89 anos;90 a
  # 94 anos; 95+;Total2
  
  
  # IMPORTANTE: essa função só roda se tiver até o grupo etário 95 e +.
  # Não funciona se tiver menos ou mais que isso. Para rodar com o último
  # grupo etário aberto diferente de 95+ é preciso fazer adaptações na função
  # nos locais indicados.
  # 
  
  # # ARmaior: essa coluna se refere a área maior. Pode ter qualquer outro
  # nome, mas tem que estar nessa posição (segunda coluna)
  # # Meso: não é usada mas tem que estar na matriz, mesmo que toda vazia,
  # só com o nome. Se quiser resultados da função por outra sub-agregação
  # geográfica pode adaptar a função, fazendo filtros usando essa coluna.
  # (terceira coluna)
  # # Sexo: Sexo: 1 é Feminino e 2 é Masculino. (quarta coluna)
  # # ARmenor: essa coluna se refere a área menor. Pode ter qualquer outro
  # nome, mas tem que estar nessa posição (quinta coluna)
  # 
  # # 
  # # # "arma.proj" é a matriz com dados da população da área maior projetada 
  # para o período seguinte. Cada coluna representa um grupo etário, de 0 a 4
  # até 90e+, ou seja tem que ter 19 colunas, exceto a 1a coluna que é o
  # sexo. Cada linha um ano de projeção.
  # # os dados em "arma.proj" também estão empilhados segundo o sexo, da
  # mesma forma que em "entr", primeiro os dados de um sexo depois do outro,
  # logo nas linhas abaixo (1-Fem; 2-Masc, na primeira coluna "sexo")
  # # Então, se queremos projetar a população de pequenas áreas pelo método
  # do Duchesne para 3 períodos à frente, "arma" terá 6 linhas (3 para
  #                                                             mulheres e 3 para homens em seguida), cada uma com os valores da pop
  # projetada para cada gr etário para a área maior para
  # # cada um desses períodos. Essa projeção da área maior deve ter sido
  # feita previamente ou obtida de alguma outra fonte de dados.
  # 
  # # TFT é um vetor com as TFTs da área maior projetada para os mesmos
  # períodos de arma.proj. Ex: se arma.proj é para 3 períodos a frente, TFT
  # será um vetor com 3 elementos
  # # TEF matriz com as TEF da área maior Projetadas. É uma matriz com 7
  # linhas, uma para cada gr etário da mulher, de 15-19 até 45-49. Cada
  # coluna um período de projeção.
  # # RSN é a razão de sexo ao nascer. Digite o número da razão de sexo do
  # último período observado
  # # L0.f e L0.m são os valores projetados de 5L0 de uma tábua feminina e
  # de uma tábua masculina. Para cada sexo entre com um vetor de 5L0. O
  # comprimento do vetor equivale a qtd de períodos projetados. Se vc vai
  # projetar 3 períodos a frente estes vetores terão 3 elementos.
  # # l0 é o valor da raíz das tabelas de sobrevivências, tanto para a
  # feminina quanto para a masculina. Digite o valor. Os valores mais comuns
  # são 1 ou 1.000.
  # 
  # 
  
  ### Função para compatibilização do tamanho da população da área maior ####
  # projetada pelo somatório das áreas menores projetadas
  
  distribuiPopulacaoProporcionalmente = function
  (PopulacaoAreaMenor,PopulacaoTotal) {
    PopulacaoTotal = as.matrix(PopulacaoTotal)
    
    DiferencaPopulacao = PopulacaoTotal - colSums(PopulacaoAreaMenor)
    
    DiferencaPopulacao =  matrix(rep(DiferencaPopulacao,
                                     nrow(PopulacaoAreaMenor)),
             nrow=nrow(PopulacaoAreaMenor),ncol=length(DiferencaPopulacao),byrow = TRUE)
    
    
    ProporcoesAreaMenor = apply(PopulacaoAreaMenor,2,prop.table)
    PopulacaoAreaMenorCompatibilizada = PopulacaoAreaMenor +
      ProporcoesAreaMenor*DiferencaPopulacao
    
    return(PopulacaoAreaMenorCompatibilizada)
  }
  
  
  ####
  ####
  #
  ### AGORA SIM, LIDA TODAS AS INSTRUÇÕES, INICIA-SE O CÓDIGO DA FUNÇÃO, ###
  # COM ALGUNS COMENTÁRIOS PARA AJUDAR NO ENTENDIMENTO:
  
    
  ###
  ###
  
  
  entr=as.data.frame(entr)
  arma.proj=as.data.frame(arma.proj)
  
  
  sex=unique(entr[,4])
  
  result=list()
  aux.arma=list()
  aux.korig=list()
  aux.kbay=list()
  rcm=list()
  
  mulherproj.orig=list()
  mulherproj.bay=list()
  nascimento.orig=list()
  nascimento.bay=list()
  
  
  arma.fem=arma.proj[arma.proj[,1]==1,]
  rcm.f=arma.fem[,2]/apply(arma.fem[,5:11],1,sum)
  rcm[[1]]=rcm.f
  
  arma.mas=arma.proj[arma.proj[,1]==2,]
  rcm.m=arma.mas[,2]/apply(arma.fem[,5:11],1,sum)
  rcm[[2]]=rcm.m
 ###
  ###
  ###
  
   

    
    ### CÁLCULOS PARA ESTIMAR 0 a 4 ANOS COM FECUNDIDADE DA ÁREA MAIORPROJETADA ###
  
  mulheres=entr[entr[,4]==1,]
  homens=entr[entr[,4]==2,]
  codmunic=mulheres[,5] # códigos e nomes das pequenas áreas
  areas=dim(mulheres)[1] # número de peq áreas
  anos=dim(arma.proj)[1]/2 # qtd de anos para projeção
  criancascenso2=mulheres[,29]+homens[,29]
  rcm.peqar=criancascenso2/apply(mulheres[,32:38],1,sum) #Aqui, diferente das rcm anteriores, calcula-se a rcm com nasc de ambos os sexos
                                                            # e só para o censo 2
  rcm.armaior=sum(criancascenso2)/sum(apply(mulheres[,32:38],1,sum)) # #Aqui, diferente das rcm anteriores, calcula-se a rcm com nasc de ambos
                                                                     # os sexos e só para o censo 2
                                                                            
                                                            
  IDFi=rcm.peqar/rcm.armaior
  TFTi=IDFi%*%t(TFT)
  TEFproporcional=sweep(TEF,2,TFT,"/") # TEF proporcional da área
  # maior. Cada coluna é um ano da projeção. Cada linha um grupo etário
  # (normalmente 7)
  TEFi=list()
  aux.TEFi=matrix(nrow=areas,ncol=7) # 7 é o número de gr etários
  # em idade reprodutiva
  
  for (j in 1:anos){
    for (i in 1:areas){
      
      aux.TEFi[i,]=TFTi[i,j]*TEFproporcional[,j]
    }
    TEFi[[j]]=aux.TEFi
  }
  
  
  
  
  
  
  ##
  #
    for (s in 1:length(sex)){
      
      dados=entr[entr[,4]==s,]
      #dados=filter(entr,entr[,4] == sex[s]) # Comando do pacote dplyr que
      # não precisa mais ser usado
      # 
      arma=arma.proj[arma.proj[,1]==s,]
      #arma=filter(arma.proj,arma.proj[,1] == sex[s]) # Comando do pacote
      # dplyr que não precisa mais ser usado
      arma=arma[,2:dim(arma.proj)[2]]
      
      
      nareas=dim(dados)[1] # Número de pequenas áreas
      aux=dados[,29:(dim(dados)[2]-1)] # Seleciona da matriz de dados de
      # entrada, só os dados do segundo censo, desde o gr 0 a 4 até o gr 95+,
      # excluindo a última coluna que é do total populacional do segundo censo.
      # Para mudar ult grupo etário, tem que adaptar: 
      #aux=dados[,XX:(dim(dados)[2]-1)]
      gret=(dim(aux)[2])-1 # Número de grupos etários da pop
      # projetada. É sempre 1 a menos que a população observada. Ex. se temos
      # dados das peq areas até 95+, a projeção das peq areas irá até 90+.
      anproj=dim(arma)[1] # Anos ou períodos de projeção. Se
      # "arma" tem 3 linhas, é porque queremos projetar para 3 períodos adiante.
      # Ex: Se e estamos em 2016 e queremos projeções para 2020, 2025 e 2030,
      # arma teria 3 linhas.
      
      
      
      
      k=calculaFatoresCrescimento(dados) # Lembrando que essas
      # # funções bem.duch4 e bemp.duchultimo4 só rodam para matriz que vai até
      # 85+. Para matrizes com menos grupos etários há outras funções (por ex: bemp.duch2 e bemp.duchultimo2)
      
      
      kult=calculaFatoresCrescimentoUltimoGrupo(dados)
      
      korig=cbind(k[[1]][,2],k[[1]][,2:dim(k[[1]])[2]],kult[[1]][,2]) 
      # Essa é a matriz com fatores K calculados da forma original proposta por
      # Duchesne
      kbay=cbind(k[[2]][,2],k[[2]][,2:dim(k[[2]])[2]],kult[[2]][,2]) 
      
      # Essa é a matriz com fatores K calculados com o estimador bayesiano
      # empírico
      # 
      
      # Cálculo da relação de sobrevivência da área maior.
      
      N90.mais=aux[,dim(aux)[2]-1]+aux[,dim(aux)[2]] # Cria população de
      # 90 e + nos dados do segundo censo
      aux.90=cbind(aux[,1:(dim(aux)[2]-2)],N90.mais) # Aqui cria uma
      # matriz aux terminando em 90+, diferente da 1a aux que ia até 95+. Esta
      # nova aux é útil para o cálculo do CR
      
      arma.aux=rbind(apply(aux.90,2,sum),arma)
      CR=matrix(nrow=anproj,ncol=dim(korig)[2])
      
      for (i in 1:anproj){
        
        for (j in 1:(dim(arma.aux)[2]-1)){
          
          if(j<=dim(arma.aux)[2]-2)
          {CR[i,j]=arma.aux[i+1,j+1]/arma.aux[i,j]}
          else
          {CR[i,j]=(arma.aux[i+1,j+1])/(arma.aux[i,j]+arma.aux[i,j+1])}
          
        }
      }
      
      
      # Cálculo da projeção dos gr etários de 5 a 9 a 90 e +:
      
      proj.korig=list()
      
      proj.kbay=list()
      
      aux2=cbind(aux[,1:(dim(aux)[2]-3)],
                 aux[,(dim(aux)[2]-2)]+aux[,(dim(aux)[2]-1)]+
                   aux[,(dim(aux)[2])]) # reformula a matriz
      # dos dados do censo 2 para terminar em 85+
      
        aux3=aux2 # reformula a matriz dos dados do censo 2 para terminar em 85+
        
        proj.orig = matrix(nrow=nareas,ncol = dim(korig)[2])
      proj.bay = matrix(nrow=nareas,ncol = dim(korig)[2])
      
      
      for (h in 1:anproj){
        
        for (i in 1:dim(korig)[2]){
          proj.orig[,i]=aux2[,i]*CR[h,i]*korig[,i]
          proj.bay[,i]=aux3[,i]*CR[h,i]*kbay[,i]
        }
        
        ### Cálculo da projeção do grupo 0 a 4 ###
        #
        # 
        # 
        
        if (s == 1) {
          mulherproj.orig[[h]]=proj.orig
        }
        
        if (h == 1){
          
          mulheresidfertil.orig=(mulheres[,32:38]+mulherproj.orig[[h]][,3:9])/2 #
          #dados[,32:38] seleciona as mulheres em período fértil no segundo Censo
          # para entradas até 95+ da pop da área menor. Outras entradas tem que mudar
          # dados[,xx:xx]
          
        } else {
          mulheresidfertil.orig=(mulherproj.orig[[h-1]][,3:9]+mulherproj.orig[[h]][,3:9])/2
        }
        
        nascimento.orig[[h]]=apply(mulheresidfertil.orig*TEFi[[h]],1,sum)*5
        
        
        
        
        if (s == 1) {
          mulherproj.bay[[h]]=proj.bay
        }
        
        if (h == 1) {
          
          mulheresidfertil.bay=(mulheres[,32:38]+mulherproj.bay[[h]][,3:9])/2
          # dados[,32:38] seleciona as mulheres em período fértil no segundo Censo
          # para entradas até 95+ da pop da área menor. Outras entradas tem que mudar
          # dados[,xx:xx]
        } else {
          mulheresidfertil.bay=(mulherproj.bay[[h-1]][,3:9]+mulherproj.bay[[h]][,3:9])/2
        }
        
        nascimento.bay[[h]]=apply(mulheresidfertil.bay*TEFi[[h]],1,sum)*5
        
        
        
        if (s == 1 ) {
          
          proj0a4.orig=nascimento.orig[[h]]*(1/(1+RSN))*(L0.f[h]/(5*l0))*korig[,1]^0.5
        } else {
          
          proj0a4.orig=nascimento.orig[[h]]*(RSN/(1+RSN))*(L0.m[h]/(5*l0))*korig[,1]^0.5
        }
        
        if (s == 1) {
          
          proj0a4.bay=nascimento.bay[[h]]*(1/(1+RSN))*(L0.f[h]/(5*l0))*kbay[,1]^0.5
        } else {
          
          proj0a4.bay=nascimento.bay[[h]]*(RSN/(1+RSN))*(L0.m[h]/(5*l0))*kbay[,1]^0.5
        }
        
        
        
        # 
        
        # if (s == 1)
        #
        # {proj0a4.orig=apply(proj.orig[,3:9],1,sum)*rcm[[s]][h]*korig[,1]}
        # else
        #
        # {proj0a4.orig=apply(result[[1]][[1]][[h]][,4:10],1,sum)*rcm[[s]][h]*korig
          # [,1]}
        #
        # if (s == 1)
        # {proj0a4.bay=apply(proj.bay[,3:9],1,sum)*rcm[[s]][h]*kbay[,1]}
        # else
        #
        # {proj0a4.bay=apply(result[[1]][[2]][[h]][,4:10],1,sum)*rcm[[s]][h]*kbay[,1]}
        
        
        
        proj.korig[[h]]=cbind(proj0a4.orig,proj.orig)
        proj.kbay[[h]]=cbind(proj0a4.bay,proj.bay)
        
        proj.korig[[h]] =
          distribuiPopulacaoProporcionalmente(proj.korig[[h]],arma[h,])
        
        proj.kbay[[h]] =
          distribuiPopulacaoProporcionalmente(proj.kbay[[h]],arma[h,])
        
        proj.korig[[h]]=cbind(codmunic,proj.korig[[h]])
        
        proj.kbay[[h]]=cbind(codmunic,proj.kbay[[h]])
        
        aux2=cbind(proj.korig[[h]][,2:(dim(proj.korig[[h]])[2]-2)],
                   proj.korig[[h]][,(dim(proj.korig[[h]])[2]-1)]+
                     proj.korig[[h]][,(dim(proj.korig[[h]])[2])]) # aux2 e aux3 terminam
        # em 85+ para entrar como "entrada" na proj do próximo ano
        aux3=cbind(proj.kbay[[h]][,2:(dim(proj.kbay[[h]])[2]-2)],proj.kbay[[h]][,(dim(proj.kbay[[h]])[2]-1)]+
                     proj.kbay[[h]][,(dim(proj.kbay[[h]])[2])])
        # aux2 e aux3 terminam em 85+ para entrar como "entrada" na proj do próximo ano
        
        proj.korig
        
        
      }
      
      
      result[[s]]=list(proj.korig,proj.kbay)
      aux.arma[[s]]=arma
      aux.korig[[s]]=korig
      aux.kbay[[s]]=kbay
      
      
    }
  
  names(result)=c("Feminino","Masculino")
  names(aux.arma)=c("Feminino","Masculino")
  names(aux.korig)=c("Feminino","Masculino")
  names(aux.kbay)=c("Feminino","Masculino")
  
  
  
  
  result.final=list(result,K_original=aux.korig,K_bayesiano=aux.kbay)
  names(result.final)= 
    c("Matriz de proj: 0-4 até 90+. segundo tipo de fator K (orig ou bayesiano), p/ cada ano da proj","Fator k original","Fator k bayesiano")
  
  result.final
}

# 1.2 Função de cálculo dos fatores diferenciais de crescimento do método Duchesne
# para os grupos etário quinquenais intermediários (segundo ao penúltimo grupo etário)
calculaFatoresCrescimento = function(dados) {
  # # ESSA FUNÇÃO TEM COMO ENTRADA AS POP DE GRUPOS ETÁRIOS QUINQUENAIS ATÉ
  # 95 e +.
  # # A DIFERENÇA DESTA FUNÇÃO PARA AQUELA DO ARQUIVO Função para
  # K_Duchesne.txt É QUE NESTE OUTRO A MATRIZ DE POPULAÇÃO DE ENTRADA
  # # VAI ATÉ 90 e +. ESSE DETALHE IMPLICA QUE NESTA FUNÇÃO DESTE ARQUIVO
  # CALCULA-SE O K ATÉ O K80. JÁ NA OUTRA FUNÇÃO
  # # DO OUTRO ARQUIVO (Função para K_Duchesne.txt) CALCULA-SE ATÉ O K75.
  # LEMBRANDO QUE HÁ UMA OUTRA FUNÇÃO PARA CALCULAR O K
  # # DO ÚLTIMO GRUPO ETÁRIO ABERTO, SEJA ELE O K70+, SEJA O K80+, ou como
  # nesse caso vamos precisar, o k85+.
  # #
  # Função para suavizar os fatores K do método de projeções
  # populacionais para pequenas áreas desenvolvido por Duchesne
  # # Suaviza os fatores 5K5 (de 5 a 9) até o fator 5K80 (80 a 84). O fator
  # 5K0 (de 0 a 4) considera-se igual ao 5K5.
  # # O Fator K85+ não está implementado nesta função. Também não está
  # implementado o fator Kb (do nascimento).
  # # Para o Fator K85+ use a outra função, chamada: bemp.duchultimo4 que
  # está no arquivo 'Função para K_Duchesne4-do ultimo grupo aberto-Brasil'
  # # Esta função está implementada para censos com intervalos de 10 anos
  # entre eles, mas a projeção sendo para grupos quinquenais de idade
  # 
  # # A matriz dados de entrada deve ter a seguinte estrutura, do contrário
  # a função não vai rodar corretamente.
  # # A matriz deve estar organizada de maneira que assim que acabe o
  # último grupo etário do primeiro censo coloque o total populacional do
  # censo 1
  # # e em seguida comece, na coluna seguinte, com o ano do segundo censo e
  # todas as colunas dos grupos etários e total do segundo censo. Ou seja,
  # # organize a base de dados com um censo do lado do outro.
  # 
  # # Cada linha da matriz "dados" deve ser um município ou pequena área em
  # geral.
  # 
  # # As colunas são pela ordem:
  # # Ano_censo1;UF;Meso;Micro;Munic;NomeMunic;0 a 4 anos;5 a 9 anos;10 a
  # 14 anos;15 a 19 anos;20 a 24 anos;25 a 29 anos;30 a 34 anos;35 a 39
  # anos;40 a 44 anos;
  # # 45 a 49 anos;50 a 54 anos;55 a 59 anos;60 a 64 anos;65 a 69 anos;70 a
  # 74 anos;75 a 79 anos;80 a 84; 85 a 89; 90 a 94; 95+;Total1;Ano_censo2;0 a
  # 4 anos;
  # # 5 a 9 anos;10 a 14 anos;15 a 19 anos;20 a 24 anos;25 a 29 anos;30 a
  # 34 anos;35 a 39 anos;40 a 44 anos;45 a 49 anos;50 a 54 anos;55 a 59
  # anos;60 a 64 anos;
  # # 65 a 69 anos;70 a 74 anos;75 a 79 anos;80 a 84; 85 a 89; 90 a 94;
  # 95+;Total2
  
  mat = as.data.frame(dados)
  N = dim(mat)[1]
  ki = matrix(rep(0, N * 17), nrow = N) # 17 é o número de gr etários
  # que precisa estimar o k nesta função, desde o 4K0 até o K80
  
  ki.bay = matrix(rep(0, N * 17), nrow = N) # 17 é o número de gr etários
  # que precisa estimar o k nesta função, desde o 4K0 até o K80
  
  ki[, 1] = mat[, 5]
  ki.bay[, 1] = mat[, 5]
  
  for (h in 8:23) {
    num1 = as.numeric(mat[, h + 23])
    den1 = mat[, h - 1] * (sum(mat[, h + 23]) / sum(mat[, h - 1]))
    num2 = as.numeric(mat[, h + 24])
    den2 = mat[, h] * (sum(mat[, h + 24]) / sum(mat[, h]))
    
    
    k1 = num1 / den1
    k2 = num2 / den2
    k = ((sqrt(k1)) + (sqrt(k2))) / 2
    
    T1 = sum(mat[, h - 1])
    T2 = sum(mat[, h])
    
    mbar1 = sum(num1) / sum(den1)
    mbar2 = sum(num2) / sum(den2)
    
    s2.1 = (sum(mat[, h - 1] * ((k1 - mbar1) ^ 2))) / T1
    s2.2 = (sum(mat[, h] * ((k2 - mbar2) ^ 2))) / T2
    
    A1 = s2.1 - (mbar1 * sum((mat[, h - 1] / T1) / den1))
    A2 = s2.2 - (mbar2 * sum((mat[, h] / T2) / den2))
    
    ci1 = NULL
    ci2 = NULL
    for (i in 1:N) {
      aux1 = A1 / (A1 + (mbar1 / den1[i]))
      if (A1 >= 0) {
        ci1[i] = aux1
      }
      else {
        ci1[i] = 0
      }
      
      aux2 = A2 / (A2 + (mbar2 / den2[i]))
      if (A2 >= 0) {
        ci2[i] = aux2
      }
      else {
        ci2[i] = 0
      }
    }
    
    ci1[is.na(ci1)] = 0
    ci2[is.na(ci2)] = 0
    
    # A seguir o cálculo do k bayesiano adaptado para intervalos de 10
    # anos entre censos, aqui chamado de teta.
    
    teta1 = mbar1 + (ci1 * (k1 - mbar1))
    teta2 = mbar2 + (ci2 * (k2 - mbar2))
    teta = ((sqrt(teta1)) + (sqrt(teta2))) / 2
    
    ki[, h - 6] = k
    ki.bay[, h - 6] = teta
  }
  result = list(ki = ki, ki.bay = ki.bay)
  result
}
# 1.3 Função de cálculo dos fatores diferenciais de crescimento do método Duchesne
# para o último grupo etário (grupo etário aberto em 90+)
calculaFatoresCrescimentoUltimoGrupo=function(dados){
  # Função para suavizar os fatores K do último grupo etário aberto do
  # método de projeções populacionais
  # # para pequenas áreas desenvolvido por Duchesne. Serve para 85 +. Para
  # outro grupo et aberto, precisa de pequenas adaptações na função
  # 
  # # Para os demais Fatores dos outros gr etáriosuse a outra função,
  # chamada: bemp.duch4 do arquivo feito para o Brasil
  # # Esta função está implementada para censos com intervalos de 10 anos
  # entre eles
  # # A matriz dados de entrada deve ter a seguinte estrutura, do contrário
  # a função não vai rodar corretamente.
  # # A matriz deve estar organizada de maneira que assim que acabe o
  # último grupo etário do primeiro censo coloque o total populacional do
  # censo 1
  # # e em seguida comece, na coluna seguinte, com o ano do segundo censo e
  # todas as colunas dos grupos etários e total do segundo censo. Ou seja,
  # # organize a base de dados com um censo do lado outro.
  # 
  # # Cada linha da matriz "dados" deve ser um município ou pequena área em
  # geral.
  # 
  # # As colunas são pela ordem:
  # # Ano_censo1;UF;Meso;Micro;Munic;NomeMunic;0 a 4 anos;5 a 9 anos;10 a
  # 14 anos;15 a 19 anos;20 a 24 anos;25 a 29 anos;30 a 34 anos;35 a 39
  # anos;40 a 44 anos;
  # # 45 a 49 anos;50 a 54 anos;55 a 59 anos;60 a 64 anos;65 a 69 anos;70 a
  # 74 anos;75 a 79 anos;80 a 84 anos;85 a 89 anos; 90 a 94 anos; 95e+;
  # Total1;Ano_censo2;0 a 4 anos;
  # # 5 a 9 anos;10 a 14 anos;15 a 19 anos;20 a 24 anos;25 a 29 anos;30 a
  # 34 anos;35 a 39 anos;40 a 44 anos;45 a 49 anos;50 a 54 anos;55 a 59
  # anos;60 a 64 anos;
  # # 65 a 69 anos;70 a 74 anos;75 a 79 anos;80 a 84 anos;85 a 89 anos; 90
  # a 94 anos; 95e+;Total2

  
  mat=as.data.frame(dados)
  N=dim(mat)[1]
  ki=matrix(rep(0,N*2),nrow=N)
  ki.bay=matrix(rep(0,N*2),nrow=N)
  ki[,1]=mat[,5]
  ki.bay[,1]=mat[,5]
  
  num1=as.numeric(mat[,47]+mat[,48])
  aux.den1=as.numeric(mat[,23]+mat[,24]+mat[,25]+mat[,26])
  den1=aux.den1*(sum(num1)/sum(aux.den1))
  
  num2=as.numeric(mat[,48])
  aux.den2=as.numeric(mat[,24]+mat[,25]+mat[,26])
  den2=aux.den2*(sum(num2)/sum(aux.den2))
  
  k1=num1/den1
  k2=num2/den2
  k=((sqrt(k1))+(sqrt(k2)))/2
  
  T1=sum(aux.den1)
  T2=sum(aux.den2)
  
  mbar1=sum(num1)/sum(den1)
  mbar2=sum(num2)/sum(den2)
  
  s2.1=(sum(aux.den1*((k1-mbar1)^2)))/T1
  s2.2=(sum(aux.den2*((k2-mbar2)^2)))/T2
  
  A1=s2.1-(mbar1*sum((aux.den1/T1)/den1))
  A2=s2.2-(mbar2*sum((aux.den2/T2)/den2))
  
  ci1=NULL
  ci2=NULL
  for (i in 1:N){
    aux1=A1/(A1+(mbar1/den1[i]))
    if (A1>=0) {ci1[i]=aux1}
    else {ci1[i]=0}
    
    aux2=A2/(A2+(mbar2/den2[i]))
    if (A2>=0) {ci2[i]=aux2}
    else {ci2[i]=0}
  }
  
  ci1[is.na(ci1)]=0
  ci2[is.na(ci2)]=0
  
  # A seguir o cálculo do k bayesiano do ult. grupo etário aberto
  # adaptado para intervalos de 10 anos entre censos, aqui chamado de teta.
  
  teta1=mbar1+(ci1*(k1-mbar1))
  teta2=mbar2+(ci2*(k2-mbar2))
  teta=((sqrt(teta1))+(sqrt(teta2)))/2
  
  ki[,2]=k
  ki.bay[,2]=teta
  
  result=list(ki.85mais=ki,ki.bay.85mais=ki.bay)
  result
}


