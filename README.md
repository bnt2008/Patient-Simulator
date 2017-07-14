# Patient-Simulator
An R based project for making a patient flow simulator for pharmaceutical forecasting

Pharmaceutical forecasting ustilizes a variety of models for predicting how patients will respond when new therapies emerge. This project creates a framework for creating models that allow such simulation.

The current model implemented is the simplest possible: Patients are created, each period of the model, patients are tested to see if they fail based on user inputs governing: initial success of the therapy, and average time on therapy. If a patient fails, a reuse penalty is applied to their current drug (so it will be less likely to be reused in the future) and a new drug is assigned based on probablilities input by the user.

run simulation.r is the body of the program

patient model.r and drug model.r contain the objects called by run simulation.r

view data.r generates output graphs and data from the results of the simulation

drugs_test.csv is an example of the user input file with the parameters that must be entered for each drug as well as the expected user variables. New drugs can be created in this file, but must have all the parameters associated with a drug. See the examples for the parameters.
