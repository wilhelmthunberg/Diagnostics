library(Diagnostics)
learn <- function(data){
  #-------------------------------------------
  #Function used to train from historical cases
  #---------------------------------------------
  #Use network provided in assignment to set up conditional probabilities
  
  #standard format for all prpbability matrices
  # Not all rows/cols used in every matrix 
  #col 1-9 are categorized same way as cases/hist - NA if not connected to variable of interest 
  #col 10 is prob for 0, given conditions in 1-9
  #col 11 is prob for 1, given conditions in 1-9
  #in p_Te: col10=mean, col11=sd  
  p_Pn=p_Te=p_VTB=p_TB=p_Sm=p_LC=p_Br=p_XR=p_Dy= matrix(nrow = 8, ncol=11) 
  
  #Probabilities for three parent nodes, Pn, VTB, Sm
  p_Pn[1,10] =1-sum(hist['Pn']) /length(hist[,1])
  p_Pn[1,11] =sum(hist['Pn']) /length(hist[,1])
  p_VTB[1,10] =1-sum(hist['VTB']) /length(hist[,1])
  p_VTB[1,11] =sum(hist['VTB']) /length(hist[,1])
  p_Sm[1,10] =1-sum(hist['Sm']) /length(hist[,1])
  p_Sm[1,11] =sum(hist['Sm']) /length(hist[,1])
  
    #set up conditional probability P(Te|Pn)
    p_Te[1,1]=0 
    index  = which(hist['Pn'][,1]==0)
    p_Te[1,10] = mean(hist['Te'][index,1])
    p_Te[1,11] = sd(hist['Te'][index,1])
    
    p_Te[2,1]=1 
    index  = which(hist['Pn'][,1]==1)
    p_Te[2,10] = mean(hist['Te'][index,1])
    p_Te[2,11] = sd(hist['Te'][index,1])
    
    
    #p(TB|VTB)
    p_TB[1,3]=0
    index  = which(hist['VTB'][,1]==0)
    p_TB[1,10] = length(which(hist['TB'][index,1] ==0)) / length(index)
    p_TB[1,11] = 1-p_TB[1,10]  #length(which([hist['TB'][index,1]] ==1)) / length(index)
    
    p_TB[2,3]=1
    index  = which(hist['VTB'][,1]==1)
    p_TB[2,10] = length(which(hist['TB'][index,1] ==0)) / length(index)
    p_TB[2,11] = 1-p_TB[2,10] 
    

    #set up conditional probabilities P(LC|Sm)
    p_LC[1,5]=0
    p_LC[2,5]=1
    index  = which(hist['Sm'][,1]==0)
    p_LC[1,10] = length(which(hist['LC'][index,1] ==0)) / length(index)
    p_LC[1,11] = 1-p_LC[1,10]  #length(which([hist['LC'][index,1]] ==1)) / length(index)
    index  = which(hist['Sm'][,1]==1)
    p_LC[2,10] = length(which(hist['LC'][index,1] ==0)) / length(index)
    p_LC[2,11] = 1-p_LC[2,10]
    
    
    #set up conditional probabilities P(Br|Sm)
    p_Br[1,5]=0
    p_Br[2,5]=1
    index  = which(hist['Sm'][,1]==0)
    p_Br[1,10] = length(which(hist['Br'][index,1] ==0)) / length(index)
    p_Br[1,11] = 1-p_Br[1,10] 
    index  = which(hist['Sm'][,1]==1)
    p_Br[2,10] = length(which(hist['Br'][index,1] ==0)) / length(index)
    p_Br[2,11] = 1-p_Br[2,10]
    
    
    #set up conditional probabilities P(Dy|LC, Br)
    p_Dy[1,6] = 0;  p_Dy[1,7] = 0;
    index  = which(hist['LC']==0 & hist['Br']==0)
    p_Dy[1,10] = length(which(hist['Dy'][index,1] ==0)) / length(index)
    p_Dy[1,11] = length(which(hist['Dy'][index,1] ==1)) / length(index)
    
    p_Dy[2,6] = 0;  p_Dy[2,7] = 1;
    index  = which(hist['LC']==0 & hist['Br']==1)
    p_Dy[2,10] = length(which(hist['Dy'][index,1] ==0)) / length(index)
    p_Dy[2,11] = length(which(hist['Dy'][index,1] ==1)) / length(index)
    
    p_Dy[3,6] = 1;  p_Dy[3,7] = 0;
    index  = which(hist['LC']==1 & hist['Br']==0)
    p_Dy[3,10] = length(which(hist['Dy'][index,1] ==0)) / length(index)
    p_Dy[3,11] = length(which(hist['Dy'][index,1] ==1)) / length(index)
    
    p_Dy[4,6] = 1;  p_Dy[4,7] = 1;
    index  = which(hist['LC']==1 & hist['Br']==1)
    p_Dy[4,10] = length(which(hist['Dy'][index,1] ==0)) / length(index)
    p_Dy[4,11] = length(which(hist['Dy'][index,1] ==1)) / length(index)
    
    
    #set up conditional probabilities P(XR|Pn, TB, LC)
    p_XR[1,1] = 0;  p_XR[1,4] = 0; p_XR[1,6] = 0; 
    index  = which(hist['Pn']==0 & hist['TB']==0 &hist['LC']==0)
    p_XR[1,10] = length(which(hist['XR'][index,1] ==0)) / length(index)
    p_XR[1,11] = length(which(hist['XR'][index,1] ==1)) / length(index)
    
    p_XR[2,1] = 0;  p_XR[2,4] = 0; p_XR[2,6] = 1
    index  = which(hist['Pn']==0 & hist['TB']==0 &hist['LC']==1)
    p_XR[2,10] = length(which(hist['XR'][index,1] ==0)) / length(index)
    p_XR[2,11] = length(which(hist['XR'][index,1] ==1)) / length(index)
    
    p_XR[3,1] = 0;  p_XR[3,4] = 1; p_XR[3,6] = 0; 
    index  = which(hist['Pn']==0 & hist['TB']==1 &hist['LC']==0)
    p_XR[3,10] = length(which(hist['XR'][index,1] ==0)) / length(index)
    p_XR[3,11] = length(which(hist['XR'][index,1] ==1)) / length(index)
    
    p_XR[4,1] = 1;  p_XR[4,4] = 0; p_XR[4,6] = 0; 
    index  = which(hist['Pn']==1 & hist['TB']==0 &hist['LC']==0)
    p_XR[4,10] = length(which(hist['XR'][index,1] ==0)) / length(index)
    p_XR[4,11] = length(which(hist['XR'][index,1] ==1)) / length(index)
    
    p_XR[5,1] = 0;  p_XR[5,4] = 1; p_XR[5,6] = 1; 
    index  = which(hist['Pn']==0 & hist['TB']==1 &hist['LC']==1)
    p_XR[5,10] = length(which(hist['XR'][index,1] ==0)) / length(index)
    p_XR[5,11] = length(which(hist['XR'][index,1] ==1)) / length(index)
    
    p_XR[6,1] = 1;  p_XR[6,4] = 0; p_XR[6,6] = 1; 
    index  = which(hist['Pn']==1 & hist['TB']==0 &hist['LC']==1)
    p_XR[6,10] = length(which(hist['XR'][index,1] ==0)) / length(index)
    p_XR[6,11] = length(which(hist['XR'][index,1] ==1)) / length(index)
    
    p_XR[7,1] = 1;  p_XR[7,4] = 1; p_XR[7,6] = 0; 
    index  = which(hist['Pn']==1 & hist['TB']==1 &hist['LC']==0)
    p_XR[7,10] = length(which(hist['XR'][index,1] ==0)) / length(index)
    p_XR[7,11] = length(which(hist['XR'][index,1] ==1)) / length(index)
    
    p_XR[8,1] = 1;  p_XR[8,4] = 1; p_XR[8,6] = 1; 
    index  = which(hist['Pn']==1 & hist['TB']==1 &hist['LC']==1)
    p_XR[8,10] = length(which(hist['XR'][index,1] ==0)) / length(index)
    p_XR[8,11] = length(which(hist['XR'][index,1] ==1)) / length(index)
    
  return(list(p_Pn,p_Te,p_VTB, p_TB, p_Sm, p_LC, p_Br, p_XR, p_Dy))
}


getProb <- function(col, conditions,network){ 
  #function to extract conditional probability 
  #for a specific variable that has a given column nr 
  # From column number - right prob Matrix is found in network
  #based on conditions, prob is found
 
  value=conditions[col]
  pMat = network[[col]]
  
  #Which conditional variables are of interest
  index = which(!is.na(pMat[1,1:9]))
  
  #if no conditoinal variables
  if (length(index)==0){
    p= c(pMat[1,10], pMat[1,11])
  }else{
  #What row in pMat matches the given conditions
  pRow = which(apply(pMat, 1, function(x) identical(x[index],conditions[index])))
  #probs (or mean/sd if temp) in last two columns
  p = c(pMat[pRow,10],pMat[pRow,11])
  }
  
  if (value>1){
    #if temperature
    prob=dnorm(value, mean=p[1], sd=p[2]) 
    }else if (value ==1){ 
      prob=p[2]
      }else{
      prob=p[1]
    }
  
  return(prob)
}

diagnose <- function(network, cases){
  res=matrix(nrow = 10,ncol = 4)
  
  for(i in 1:nrow(cases)) {
    case <- cases[i,]
    sLength=1000
    samples= matrix(nrow = sLength, ncol = 9)
    #make randomly generated sample1 based on case
    sample1 = as.numeric(case)
    rand = sample(c(0,1), size=4, replace=TRUE)
    sample1[1]=rand[1]; sample1[4]=rand[2];sample1[6]=rand[3];sample1[7]=rand[4];
    samples[1,]=sample1
    
    #make 1000 complete samples
      for(j in c(1:(nrow(samples)-1))){
        sample <- samples[j,]
        for (k in c(1,4,6,7)) {
          old = sample[k]
          new=1-old
          newSample = sample
          newSample[k]=new
          
          #calculate pNew and pOld usign getProb for each variable in sample and newSample
          pOld= 1
          pNew=1
          for (l in 1:9){
            pOld = pOld*getProb(l,sample,network)
            pNew = pNew*getProb(l,newSample,network)
          }
          
          #possibly flip variable k 
          r=runif(1)
          if(pNew>=pOld){
            sample[k]=new
          }else if(r < pNew/pOld){
            sample[k]=new
          }
        }
        samples[j+1,] = sample
      } 
    burn=round(sLength*0.1)
    res[i,1] = length(which(samples[(burn+1):sLength,1]==1))/(sLength-burn)
    res[i,2] = length(which(samples[(burn+1):sLength,4]==1))/(sLength-burn)
    res[i,3] = length(which(samples[(burn+1):sLength,6]==1))/(sLength-burn)
    res[i,4] = length(which(samples[(burn+1):sLength,7]==1))/(sLength-burn)
  }
  return(res)
}

network= learn(hist)

runDiagnostics(learn, diagnose,verbose = 2)