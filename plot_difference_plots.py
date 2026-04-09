# Script to plot LPJ-GUESS output data processed with FORWARDS_annual_meeting_risk_disturbance_as_faction_biomass_losses.R
# Feb 2026
# Annemarie Eckes-Shephard
# Claude 
# based on example script from Haoming Zhong.


import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import numpy as np
import os

print("Starting difference plot script...")

# Define what columns to plot
# Format: {plot_name: column_name}
plot_configs = {
    'ssp126': {
        'Vegetation_carbon': 'ssp126VegCdiff',
        'Ecosystem_carbon': 'ssp126EcosystemCdiff'
    },
    'ssp370': {
        'Vegetation_carbon': 'ssp3706VegCdiff',  # Note: typo in R code "3706" instead of "370"
        'Ecosystem_carbon': 'ssp370EcosystemCdiff'
    }
}

lon_min, lon_max = -11, 32
lat_min, lat_max = 35, 72

output_dir = '../Figures_and_reports/Annual_Meeting/'
os.makedirs(output_dir, exist_ok=True)
print(f"Output directory ready: {output_dir}")

# Read the single CSV file that contains all data
input_file = '../data/processed/Forest_and_ecosystem_carbon_loss_data.csv'
print(f"Reading {input_file}...")

try:
    df = pd.read_csv(input_file)
    print(f"CSV loaded successfully. Shape: {df.shape}")
    print(f"Columns: {df.columns.tolist()}")
except FileNotFoundError:
    print(f"ERROR: File {input_file} not found. Exiting.")
    exit(1)

total_maps = 0

# Create plots for each SSP and carbon type
for ssp, carbon_types in plot_configs.items():
    print(f"\n{'='*60}")
    print(f"Processing SSP: {ssp}")
    print(f"{'='*60}")

    for carbon_name, column_name in carbon_types.items():
        print(f"\n>>> Processing {carbon_name}...")

        # Check if column exists
        if column_name not in df.columns:
            print(f"     WARNING: Column '{column_name}' not found. Skipping.")
            print(f"     Available columns: {df.columns.tolist()}")
            continue

        filename = f'{output_dir}{carbon_name}_{ssp}_difference_2070-2025.png'

        if os.path.exists(filename):
            print(f"    File already exists, skipping")
            continue

        print(f"    Creating difference map...", end='', flush=True)

        # Prepare data
        df_plot = df[['Lon', 'Lat', column_name]].copy()
        df_plot.columns = ['Lon', 'Lat', 'diff']

        # Drop NA values
        df_plot = df_plot.dropna()

        if len(df_plot) == 0:
            print(f" SKIPPED (no valid data)")
            continue

        # Create map
        fig = plt.figure(figsize=(11, 8))
        ax = plt.axes(projection=ccrs.PlateCarree())
        ax.set_extent([lon_min, lon_max, lat_min, lat_max], crs=ccrs.PlateCarree())

        ax.add_feature(cfeature.COASTLINE.with_scale('50m'))
        ax.add_feature(cfeature.BORDERS.with_scale('50m'), linestyle=':')
        ax.add_feature(cfeature.LAND.with_scale('50m'), facecolor='lightgray', alpha=0.5)

        # Point sizes - uniform since we don't have forest fraction in this dataset
        point_size = 20

        # Use red-white-blue diverging colormap
        # Symmetric limits around zero (as in R code)
        vmax = 8
        vmin = -8

        # Scatter plot with diverging colormap
        sc = ax.scatter(df_plot['Lon'], df_plot['Lat'],
                       s=point_size,
                       c=df_plot['diff'],
                       cmap='RdBu',  # Red for negative (loss), Blue for positive (gain)
                       vmin=vmin,
                       vmax=vmax,
                       alpha=0.85,
                       edgecolor='k', linewidth=0.25)

        # Colorbar
        cbar = plt.colorbar(sc, ax=ax, orientation='vertical', pad=0.03, shrink=0.65,
                           extend='both')  # Show arrows for values beyond limits
        cbar.set_label('Δ Carbon (kgC/m²)', fontsize=12)

        # Title
        carbon_label = carbon_name.replace('_', ' ').title()
        ax.set_title(f'{carbon_label} Change (2070 - 2025) - {ssp.upper()}',
                   fontsize=16, fontweight='bold')

        # Save
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        plt.close()

        total_maps += 1
        print(f" DONE")

print(f"\n{'='*60}")
print(f"Script complete! Total difference maps created: {total_maps}")
print(f"{'='*60}")
