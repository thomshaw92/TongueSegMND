import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df = pd.read_csv('results.csv')

df = df.dropna()

df = df[df['Label'].isin([1, 2, 3, 4])]

metrics = ['Volume']

# Plot histograms of each metric per dataset and label
for metric in metrics:
    for dataset in df['Dataset'].unique():
        df_subset = df[df['Dataset'] == dataset]
        g = sns.FacetGrid(df_subset, col="Label", margin_titles=True)
        g.map(sns.histplot, metric)
        plt.savefig(f'histogram_{metric}_{dataset}.png')
