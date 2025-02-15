# Purpose: Random Forest model for predicting log odds of a word given its phonological properties

# -- loading stuff -- #

# load pandas numpy
import pandas as pd
import numpy as np
# load sklearn for preprocessing, model selection, and metrics, random forest
from sklearn import preprocessing
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.dummy import DummyRegressor
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import cross_val_score

d = pd.read_csv('dat/wide.tsv', sep='\t')

# -- preprocessing -- #

# list cols of d
d.columns
# filter for varies == true
d = d[d.varies == True]
# make new column which is 1 if column "lemma" ends in "ns" otherwise 0
d['ns'] = d.lemma.str.endswith('ns').astype(int)
# keep coda1, coda2, lv_log_odds, nsyl, neighbourhood_size, llfpm10, ns
d = d[['coda1', 'coda2', 'lv_log_odds', 'nsyl', 'neighbourhood_size', 'llfpm10', 'ns']]
# one hot encode coda1, coda2, drop coda1 coda2
d = pd.get_dummies(d, columns=['coda1', 'coda2'])

# -- set up training grid for random forest

# X_train is all columns except lv_log_odds
X_train = d.drop('lv_log_odds', axis=1)
# y_train is lv_log_odds
y_train = d['lv_log_odds']

param_grid = {
    'n_estimators': [50, 100, 500, 1000, 1500],
    'max_features': np.arange(1, 6, 1),
    'max_samples': [0.25, 0.5, 0.75, 1.0]
}

rf = RandomForestRegressor(random_state=42)

grid_search = GridSearchCV(
    estimator=rf, 
    param_grid=param_grid, 
    cv=3, 
    n_jobs=-1, 
    verbose=2
    )

grid_search.fit(X_train, y_train)

print("Best parameters:", grid_search.best_params_)
print("Best cross-validation score:", grid_search.best_score_)

best_rf = grid_search.best_estimator_

dummy = DummyRegressor(strategy='mean')
dummy_scores = cross_val_score(dummy, X_train, y_train, cv=3, scoring='r2')

varimp = pd.Series(best_rf.feature_importances_, index=X_train.columns).sort_values(ascending=False)

# save best_params_ and best_score_ and varimp to disk
best_params = grid_search.best_params_
best_score = grid_search.best_score_

# write best_params and best_score into txt
with open('dat/best_params.txt', 'w') as f:
    f.write(str(best_params))

with open('dat/best_score.txt', 'w') as f:
    f.write(str(best_score))

with open('dat/dummy_score.txt', 'w') as f:
    f.write(str(dummy_scores))

varimp.to_csv('dat/varimp.tsv', sep='\t')

