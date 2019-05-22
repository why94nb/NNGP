
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from keras import backend as K
from keras import regularizers
from keras.models import Sequential
from keras.callbacks import EarlyStopping, ModelCheckpoint
from keras.layers import Dense, LSTM
from keras.preprocessing import sequence
from sklearn.model_selection import KFold
from sklearn.metrics import accuracy_score, confusion_matrix, f1_score, recall_score, mean_squared_error
from keras.models import load_model



np.random.seed(1000)
folder = "/Users/Ye/Dropbox/LSTM/education/cordillera/"
X = np.load(folder+"cordillera_X.npy")
y = np.load(folder + "cordillera_post.npy")
max_length = max([len(each) for each in X])
print "The longest sequence is " + str(max_length)
print "The number of student is " + str(len(y))


result = pd.DataFrame()
mse, rmse = [], []

neuron1 = 80
print "number of neuron: " + str(neuron1)
neuron2 = 10
print "number of neuron: " + str(neuron2)

print "input shape: ", X.shape


train_pred = []
train_actual = []
test_pred = []
test_actual = []
kf = KFold(n_splits=5, shuffle=True)

max_length = 0
tmp = []
print X.shape

for each in X:
    end = int(len(each) * ind)  # for early prediction
    max_length = max(max_length, end)
    tmp.append(each[:end])
    print "append"

tmp = np.array(tmp)
print "tmp shape: ", tmp.shape

for train_index, test_index in kf.split(X):
    X_train, X_test = tmp[train_index], tmp[test_index]
    y_train, y_test = y[train_index], y[test_index]

    X_train = sequence.pad_sequences(X_train, maxlen=max_length)
    X_test = sequence.pad_sequences(X_test, maxlen=max_length)

    X_train = np.reshape(X_train, (X_train.shape[0], max_length, X_train.shape[2]))
    X_test = np.reshape(X_test, (X_test.shape[0], max_length, X_train.shape[2]))

    model = Sequential()
    model.add(LSTM(neuron1, return_sequences=True, input_shape=(max_length, X_train.shape[2])))
    model.add(LSTM(neuron2))
    model.add(Dense(1, activation='linear'))
    model.compile(loss='mse', optimizer='adam')

    # Model saving callback
    ckpt_callback = ModelCheckpoint('keras_model', monitor='val_loss', verbose=1, save_best_only=True, mode='auto')
    model.fit(X_train, y_train, epochs=20, batch_size=60, validation_split=0.2, callbacks=[ckpt_callback])

    model = load_model('keras_model')
    predict = model.predict(X_train)
    for each in predict:
        train_pred.append(each)
    train_actual.extend(y_train)

    predict = model.predict(X_test)
    for each in predict:
        test_pred.append(each)
    test_actual.extend(y_test)

print model.summary()
print "   "
print "Training MSE:", mean_squared_error(train_actual, train_pred)
print "MSE: ", mean_squared_error(test_actual, test_pred)






