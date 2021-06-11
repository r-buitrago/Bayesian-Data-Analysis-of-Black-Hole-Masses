Bayesian Data Analysis of Black Hole Masses

This is the repository for my proyect: "Determining the mass of a black hole from an upper bound observation, using a bayesian joint analysis of its host galaxy's variables". You can check it out in the pdf file "Thesis_BayesianAnalaysisOfBlackHoleMasses". The three most important files for this proyect are: 
 - BlackHoleMassSampling.txt: This file contains 15000 data points samplings for the joint posterior distributions of the black hole masses for 67 galaxies. Such sampling has been obtained with an innovative probability distribution that I have design, which takes into account information from 7 variables of the black holes' host galaxies. See my thesis for more details.
 - data_MBH.dat: This is the data file for the 67 galaxies with which I have done my analysis.
 - histograms.zip: Histograms for the posterior distribution of the masses of black holes for all galaxies. They are a very visual tool to see how my joint fit model works, and what is the final shape of the probability distribution for each black hole.

I have also included the following code to analyse and visualize the data: 
 - individual_fit: the Stan code to perform the individual fits of the data. It contains a model to analyze data where same observations are upper bounds. I have tested this code with a toy model, and it worked very well.  Getting used to it might be useful in trying to understand joint_fit.stan
 - joint_fit.stan: this is the Stan code to perform the joint data analysis. The model includes the novel probability distribution that I designed and a method to incorporate upper bounds into the analysis.
 - IndividualJointPlots.R: this is a code that plots several graphs (X,Y), where Y is the black hole mass and X is one of the galaxy's variables. 
 - Sampling.R: This is a code to generate and save samplings for the posterior distributions of the black hole masses in a .txt file (comma separated values).
 - Type1_histogram.R and Type2_histograms.R: This is the code that generates the histograms in histograms.zip. You can use it to try histograms with another individual variables' fits, or to try your own model and compare it with mine.
 - upper_bound.stan: This is the code for my upper bound model when there are no observational errors in X. I use it to test my model with the code in UpperBoundToyModel.R.
 - GalaxiesFUVLkSigmaPlot.R: A plot of the real values for the black hole masses from the posterior of the individual and joint fits.
