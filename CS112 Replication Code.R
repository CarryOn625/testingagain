#first step - set seed for replicability
set.seed(1009)
#install and load the necessary libraries 
library(MASS)
library(Matching)

#load the data set, downloaded into the work environment
#write out the formular for the covariates and assign it to model
model=formula(college ~ yPubAffZ + yNewspaperZ + yRadioZ + yMagazineZ + yFamTalkZ + yFrTalkZ + yAdultTalkZ + ySPIDZ + yGovtOpinionZ + yGovtCrookZ + yGovtWasteZ + yTrGovtZ + yGovtSmartZ + yGovt4AllZ + yLifeWishZ + yGLuckZ + yFPlansZ + yWinArgZ + yStrOpinionZ + yMChangeZ + yTrOthersZ + yOthHelpZ + yOthFairZ + yKnowledgeZ + yNextSchZ + yGPAZ + ySchOfficerZ + ySchPublishZ + yHobbyZ + ySchClubZ + yOccClubZ + yNeighClubZ + yRelClubZ + yYouthOrgZ + yClubLevZ + yPhoneZ + yGenZ + yRaceZ + pNewspaperZ + pRadioZ + pTVZ + pMagazineZ + pLifeWishZ + pGLuckZ + pFPlansZ + pWinArgZ + pStrOpinionZ + pMChangeZ + pTrOthersZ + pOthHelpZ + pOthFairZ + pSPIDZ + pVoteZ + pPersuadeZ + pRallyZ + pOthActZ + pPolClubZ + pButtonZ + pMoneyZ + pGovtOpinionZ + pGovtCrookZ + pGovtWasteZ + pTrGovtZ + pGovtSmartZ + pGovt4AllZ + pEmployZ + pEducHHZ + pChurchOrgZ + pFratOrgZ + pProOrgZ + pCivicOrgZ + pCLOrgZ + pNeighClubZ + pSportClubZ + pInfClubZ + pFarmGrZ + pWomenClubZ + pClubLevZ + pHHIncZ + pOwnHomeZ + pKnowledgeZ)

#Generate propennsity score for the observations
pscore=glm(model,family=binomial(link=logit))
etahat=pscore$fitted.values

# Propensity Score Matching
mout_r1 = Match(Y = yppnscal, Tr = college, X = etahat, estimand="ATT", M = 3) 

#summarize the result from the propensity score matching
summary(mout_r1)

#run the matching algorithm with the college as treatment 
#check balance
matbal_r1 = MatchBalance(college ~ etahat + yGPA + yGen + yBlack + yRep+ yKnowledge + yNextSch + pVote + pPersuade + pParticipate2 + pEmploy + pEducHH + pEducW + pHHInc + pOwnHome + pRep  + pKnowledge, match.out = mout_r1,nboots=1000)

mb1=percent.bal(matbal_r1)

#match
mout_r1D = Match(Y= yppnscal[-c(723,676,1061,337,595)], Tr = college [-c(723,676,1061,337,595) ],  X = etahat[-c(723,676,1061,337,595)], estimand="ATT", M = 3) 

#summarise the result from matching
summary(mout_r1D)

#Extension run a genetic matching
#specify the covariates
Xmat=cbind(etahat,out)
for(i in 2:ncol(Xmat)){
  Xmat[,i]=lm(Xmat[,i]~etahat)$residuals
}

#run genetic matching
genout_m1=GenMatch(Tr=college, X=Xmat, BalanceMatrix=out, estimand='ATT', M = 3, pop.size=1, max.generations=1, wait.generations=1, hard.generation.limit=F, nboots=0, ties=T, MemoryMatrix=F)	

mout_m1=Match(Y=yppnscal,Tr=college, X=Xmat, estimand='ATT', M = 3, ties=T, Weight.matrix=genout_m1) 

#summarise the outcome of the Genetic Matching
summary(mout_m1)
