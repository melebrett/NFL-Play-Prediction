# NFL Play Prediction 
# MSDS458 AI and Deep Learning 
# Final Project
# 6/19/2023

This project introduces methods to incorporate sequential dependencies within sets of offensive plays to aid in predicting the next play in an NFL game. The models presented in the jupyter notebooks leverage recurrent neural networks, specifically LSTMs, to predict the next play given a sequence of offensive team plays. Models are built for both dropback (pass v run) and standard play type (field goal, pass, punt, run) on offensive snaps (excluding PATs). This approach builds on exiting public play prediction models, which rely exclusively on information about the game state for each snap and effectively ignore play calls that happened earlier in the series, drive or game. 
