import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Read the CSV file
df = pd.read_csv('/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/code/TongueSegMND/volume_curv_stats_extraction/results.csv')

# Drop rows with missing values
df = df.dropna()

# Filter the dataframe to include only labels 1-4
df = df[df['Label'].isin([1, 2, 3, 4])]

metrics = ['Volume']

# Plot histograms of each metric per dataset and label
for metric in metrics:
    for dataset in df['Dataset'].unique():  # Iterate over unique datasets
        # Create a subset of the dataframe for each dataset
        df_subset = df[df['Dataset'] == dataset]
        g = sns.FacetGrid(df_subset, col="Label", margin_titles=True)
        g.map(sns.histplot, metric)
        # Include the dataset name in the filename
        plt.savefig(f'histogram_{metric}_{dataset}.png')
