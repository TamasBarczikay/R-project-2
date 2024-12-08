## Import packages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from sklearn.model_selection import GridSearchCV

## Read in data
data = pd.read_csv("/Users/tamasbarczikay/Desktop/Innio/data.csv")

## Clean data
# Drop row that only had value in the oph column - non-imputable
data = data.dropna(subset=['pist_m'])
# Drop row that had an unlikely large value
data = data[data['oph'] != 1000000000]
# Drop op_set_x columns - No variation in 1 and 3, 2 is NA
data = data.drop(['op_set_1', 'op_set_2', 'op_set_3'], axis=1)

## Exclude uncorrelated vars
# Drop ng_imp numeric var, since it is sig level is over 10%
# Drop past_dmg cat var, since it is sig level is over 63%
data = data.drop(['ng_imp', 'past_dmg'], axis=1)

## Create binary vars for/instead of one-hot encoding
# issue_type --- typical_issue_type = 1, otherwise 0
data['typical_issue_type'] = data['issue_type'].apply(lambda x: 1 if str(x).lower() == 'typical' else 0)
data = data.drop(['issue_type'], axis=1)
# resting_analysis_results --- critical_rest_result = 1, otherwise 0 - there was only 4 critical cases
data['critical_rest_result'] = data['resting_analysis_results'].apply(lambda x: 1 if x == 2 else 0)
data = data.drop(['resting_analysis_results'], axis=1)
# number_tc --- number_tc_2 if two turbochargers were used = 1, otherwise 0 - there were  148 cases
data['number_tc_2'] = data['number_tc'].apply(lambda x: 1 if x == 2 else 0)
data = data.drop(['number_tc'], axis=1)


## Classification
# NOTE: THIS PART OF THE ANALYSIS ONLY USES VARS THAT WERE FOUND TO BE CORRELATED IN ADVANCE

# Normalization: oph, bmep, rpm_max, number_up
numeric_cols = ['oph', 'bmep', 'rpm_max', 'number_up']
# Set scaler
scaler = StandardScaler()
# Fit it to numeric cols
data[numeric_cols] = scaler.fit_transform(data[numeric_cols])


# Train test split
X = data.drop('high_breakdown_risk', axis=1)
y = data['high_breakdown_risk']

# Make the split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)


# Choosing models: logistic reg / SVM
# Instantiate models
# Define parameter grids for each model
logistic_param_grid = {'C': [0.001, 0.01, 0.1, 1, 10, 100]}
svc_param_grid = {'C': [0.001, 0.01, 0.1, 1, 10, 100], 'kernel': ['linear', 'rbf', 'poly'], 'degree': [2, 3, 4]}

# Initialize models
logistic_model = LogisticRegression()
svc_model = SVC()

# Grid search for logistic regression
scoring = ['accuracy', 'recall', 'precision', 'f1']

logistic_grid_search = GridSearchCV(logistic_model, logistic_param_grid, cv=10, scoring=scoring,  refit='accuracy')
logistic_grid_search.fit(X_train, y_train)

# grid search for SVM
svc_grid_search = GridSearchCV(svc_model, svc_param_grid, cv=10, scoring=scoring, refit='accuracy')
svc_grid_search.fit(X_train, y_train)

# Best models
best_logistic_model = logistic_grid_search.best_estimator_
best_svc_model = svc_grid_search.best_estimator_

# Model fit evaluation
logistic_predictions = best_logistic_model.predict(X_test)
svc_predictions = best_svc_model.predict(X_test)

# Metrics for these models
logistic_accuracy = accuracy_score(y_test, logistic_predictions)
logistic_recall = recall_score(y_test, logistic_predictions)
logistic_precision = precision_score(y_test, logistic_predictions)
logistic_f1 = f1_score(y_test, logistic_predictions)

svc_accuracy = accuracy_score(y_test, svc_predictions)
svc_recall = recall_score(y_test, svc_predictions)
svc_precision = precision_score(y_test, svc_predictions)
svc_f1 = f1_score(y_test, svc_predictions)

# Print metric values
print("Logistic Regression Metrics:")
print(f"Accuracy: {logistic_accuracy:.3f}")
print(f"Recall: {logistic_recall:.3f}")
print(f"Precision: {logistic_precision:.3f}")
print(f"F1 Score: {logistic_f1:.3f}")

print('\n')

print("SVC Metrics:")
print(f"Accuracy: {svc_accuracy:.3f}")
print(f"Recall: {svc_recall:.3f}")
print(f"Precision: {svc_precision:.3f}")
print(f"F1 Score: {svc_f1:.3f}")


# We choose the best logistic regression
# (it outperforms SVM in all measurements - except for recall that is the same)

print('\n')

coefficients = best_logistic_model.coef_[0]
intercept = best_logistic_model.intercept_
odds_ratios = np.exp(coefficients)

for feature, odds_ratio in zip(X_train.columns, odds_ratios):
    print(f"{feature}: {odds_ratio:.4f}")

#  DO NOT FORGET THAT THESE ARE ONE ST DEV INCREASES!

# Create a df with feature names and odds ratios
feature_stats = pd.DataFrame({
    'Feature': X_train.columns,
    'Odds Ratio': odds_ratios
})

# Sort the df in ascending order based on odds ratios
feature_stats_sorted = feature_stats.sort_values(by='Odds Ratio', ascending=True)

# Color binary vars
colors = ['red' if feature in ['typical_issue_type', 'critical_rest_result', 'number_tc_2', 'pist_m'] else 'skyblue' for feature in feature_stats_sorted['Feature']]

# PCreate bar chart
plt.figure(figsize=(10, 6))
plt.barh(feature_stats_sorted['Feature'], feature_stats_sorted['Odds Ratio'], color=colors)
plt.axvline(x=1, color='black', linestyle='--', linewidth=1.5)  # Add a vertical dashed line at x=1
plt.xlabel('Odds Ratio')
plt.title('Odds Ratios of Features')
plt.grid(axis='x', linestyle='--', alpha=0.6)

# Add explanation
plt.text(2, -0.5, "Red columns indicate binary variables.",
         color='black', ha='center', va='center', fontsize=10,
         bbox=dict(facecolor='white', edgecolor='none', boxstyle='round,pad=0.5'))

# PLot it
plt.show()




