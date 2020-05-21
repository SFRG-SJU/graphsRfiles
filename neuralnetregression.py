
# may need to install keras from terminal using pip install keras

import pandas as pd
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasRegressor
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline

# load datasets
# congestion 1 has weights 1 & 5
df1 = pd.read_csv("congestion.csv")
data1 = df1.values

# congestion 2 has weights 1-10
df2 = pd.read_csv("congestion2.csv")
data2 = df2.values

# congestion 3 has weights 1 & 10
df3 = pd.read_csv("congestion3.csv")
data3 = df3.values

# split into input and output variables
X1 = data1[:,2:51]
Y1 = data1[:,70]

X2 = data2[:,2:51]
Y2 = data2[:,70]

X3 = data3[:,2:51]
Y3 = data3[:,70]
# define the model
def baseline_model():
	# create model
	model = Sequential()
	model.add(Dense(64, input_dim=49, kernel_initializer='normal', activation='relu'))
	model.add(Dense(32, kernel_initializer='normal', activation='relu'))
	model.add(Dense(1, kernel_initializer='normal'))
	# compile model
	model.compile(loss='mean_squared_error', optimizer='adam')
	return model
# standardize data
estimators = []
estimators.append(('standardize', StandardScaler()))
estimators.append(('mlp', KerasRegressor(build_fn=baseline_model, epochs=50, batch_size=5, verbose=0)))
pipeline = Pipeline(estimators)
kfold = KFold(n_splits=10)

#evalue model
results1 = cross_val_score(pipeline, X1, Y1, cv=kfold)
results2 = cross_val_score(pipeline, X2, Y2, cv=kfold)
results3 = cross_val_score(pipeline, X3, Y3, cv=kfold)
print("Congestion 1 Baseline: %.2f (%.2f) MSE" % (results1.mean(), results1.std()))
print("Congestion 2 Baseline: %.2f (%.2f) MSE" % (results2.mean(), results2.std()))
print("Congestion 3 Baseline: %.2f (%.2f) MSE" % (results3.mean(), results3.std()))


# create larger model
def larger_model():
	# create model
	model = Sequential()
	model.add(Dense(256, input_dim=49, kernel_initializer='normal', activation='relu'))
	model.add(Dense(128, kernel_initializer='normal', activation='relu'))
	model.add(Dense(64, kernel_initializer='normal', activation='relu'))
	model.add(Dense(32, kernel_initializer='normal', activation='relu'))
	model.add(Dense(1, kernel_initializer='normal'))
	# compile model
	model.compile(loss='mean_squared_error', optimizer='adam')
	return model

# standardize 
estimators2 = []
estimators2.append(('standardize', StandardScaler()))
estimators2.append(('mlp', KerasRegressor(build_fn=larger_model, epochs=50, batch_size=5, verbose=0)))
pipeline2 = Pipeline(estimators2)
kfold = KFold(n_splits=10)

# evaluate larger model
results4 = cross_val_score(pipeline2, X1, Y1, cv=kfold)
results5 = cross_val_score(pipeline2, X2, Y2, cv=kfold)
results6 = cross_val_score(pipeline2, X3, Y3, cv=kfold)
print("Congestion 1 Larger: %.2f (%.2f) MSE" % (results4.mean(), results4.std()))
print("Congestion 2 Larger: %.2f (%.2f) MSE" % (results5.mean(), results5.std()))
print("Congestion 3 Larger: %.2f (%.2f) MSE" % (results6.mean(), results6.std()))