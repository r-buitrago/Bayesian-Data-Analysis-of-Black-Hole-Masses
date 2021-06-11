Bayesian Data Analysis of Black Hole Masses

This is the repository for my proyect: "Determining the mass of a black hole from an upper bound observation, using a bayesian joint analysis of its host galaxy's variables". You can check it out in the pdf file "Thesis_BayesianAnalaysisOfBlackHoleMasses". The three most important files for this proyect are: 
 - BlackHoleMassSampling.txt: This file contains 15000 data points samplings for the joint posterior distributions of the black hole masses for 67 galaxies. Such sampling has been obtained with an innovative probability distribution that I have design, which takes into account information from 7 variables of the black holes' host galaxies. See my thesis for more details.
 - data_MBH.dat: This is the data file for the 67 galaxies with which I have done my analysis.
 - Histograms: They are a very visual tool to see how my joint fit model works, and what is the final shape of the probability distribution for each black hole.

I have also included the following code to analyse and visualize the data: 
 - individual_fit: the Stan code to perform the individual fits of the data. It contains a model to analyze data where same observations are upper bounds. I have tested this code with a toy model, and it worked very well.  Getting used to it might be useful in trying to understand joint_fit.stan
 - joint_fit.stan: this is the Stan code to perform the joint data analysis. The model includes the novel probability distribution that I designed and a method to incorporate upper bounds into the analysis.

