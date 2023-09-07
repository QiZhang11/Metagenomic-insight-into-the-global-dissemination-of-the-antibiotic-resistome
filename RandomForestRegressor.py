import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
import sklearn
from sklearn.ensemble import RandomForestRegressor
import pydot
import scipy.stats as stats
import matplotlib.pyplot as plt
from sklearn import metrics
from openpyxl import load_workbook
from sklearn.tree import export_graphviz
import joblib

mpl.rcParams['font.sans-serif'] = [u'SimHei']
mpl.rcParams['axes.unicode_minus'] = False

path = 'C:/Users/plant/Desktop/Table S2.xlsx'

k = 10
data = pd.read_excel(path) 
name = list(data.columns.values)
Data = np.array(data)
np.random.shuffle(Data)
num_val_sample = len(Data) // k
feature_num = 16
print(Data.shape)
score_list = []
best_forest = RandomForestRegressor()
best_score = 0
best_score_list = []

for i in range(k):
    x_test = Data[i * num_val_sample:(i + 1) * num_val_sample, 0:feature_num]
    y_test = Data[i * num_val_sample:(i + 1) * num_val_sample, feature_num:]
    x_train = np.concatenate([Data[:i * num_val_sample, 0:feature_num], Data[(i + 1) * num_val_sample:, 0:feature_num]],
                             axis=0)
    y_train = np.concatenate([Data[:i * num_val_sample, feature_num:], Data[(i + 1) * num_val_sample:, feature_num:]],
                             axis=0)

    random_seed = 100
    random_forest_seed = np.random.randint(low=1, high=230)

    forest = RandomForestRegressor(n_estimators=100, random_state=random_forest_seed)
    forest.fit(x_train, y_train)
    y_pred = forest.predict(x_test)
    score = sklearn.metrics.r2_score(y_test, y_pred)
    if best_score < score:
        best_score = score
        best_forest = forest
    score_list.append(score)
    joblib.dump(best_forest, "C:/Users/plant/Desktop/RF.pkl")
print("n_estimators:", forest.get_params()['n_estimators'])
print("max_depth:", forest.get_params()['max_depth'])
print("max_features:", forest.get_params()['max_features'])
print(score_list)
print(sum(score_list) / len(score_list))

print(best_score)
for i in range(k):
    x_test = Data[i * num_val_sample:(i + 1) * num_val_sample, 0:feature_num]
    y_test = Data[i * num_val_sample:(i + 1) * num_val_sample, feature_num:]
    x_train = np.concatenate([Data[:i * num_val_sample, 0:feature_num], Data[(i + 1) * num_val_sample:, 0:feature_num]],
                             axis=0)
    y_train = np.concatenate([Data[:i * num_val_sample, feature_num:], Data[(i + 1) * num_val_sample:, feature_num:]],
                             axis=0)
    y_pred = best_forest.predict(x_test)
    score = sklearn.metrics.r2_score(y_test, y_pred)
    best_score_list.append(score)
print(y_test)
print(y_pred)
print(best_score_list)
print(sum(best_score_list) / len(best_score_list))
score_all = []
for i in range(Data.shape[1]):
    score_all.append(best_score_list)

random_forest_error = y_pred-y_test

plt.figure(1)
plt.clf()
ax=plt.axes(aspect='equal')
plt.scatter(y_test,y_pred)
plt.xlabel('True Values')
plt.ylabel('Predictions')
Lims=[4,20000]
plt.xlim(Lims)
plt.ylim(Lims)
plt.plot(Lims,Lims)
plt.grid(False)
plt.show()

plt.figure(2)
plt.clf()
plt.hist(random_forest_error,bins=30)
plt.xlabel('Prediction Error')
plt.ylabel('Count')
plt.grid(False)
plt.show()

name1 = ["Longitude","Latitude","Sample collection","Disease","Age","Sex","Diet Structure","Average precipitation","PM2.5 air pollution","GNI","Urban population","Compulsory education","Education attainment","Population density","Antibiotic Use","Enterotype"]
random_forest_importance=list(forest.feature_importances_)
random_forest_feature_importance=[(feature,round(importance,16))
                                  for feature, importance in zip(name1,random_forest_importance)]
random_forest_feature_importance=sorted(random_forest_feature_importance,key=lambda x:x[1],reverse=True)
plt.figure(3)
plt.clf()
importance_plot_x_values=list(range(len(random_forest_importance)))
plt.bar(importance_plot_x_values,random_forest_importance,orientation='vertical')
plt.xticks(importance_plot_x_values,name1,rotation='vertical')
plt.xlabel('Variable')
plt.ylabel('Importance')
plt.title('Variable Importances')
plt.show()
print(random_forest_feature_importance)
print(random_forest_error)
