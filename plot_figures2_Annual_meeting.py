import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import numpy as np
import os

print("Starting script...")

# Define SSP scenarios to process
ssp_scenarios = ['ssp126', 'ssp370']

# Define plot types: 'cumulative' or 'absolute'
plot_types = ['cumulative', 'absolute']

# Define disturbance agents - base column names (without _cumsum suffix)
disturbance_agents = ['Fire', 'Storm', 'Insect']

lon_min, lon_max = -11, 32
lat_min, lat_max = 35, 72

output_dir = '../Figures_and_reports/disturbance_risk/'
os.makedirs(output_dir, exist_ok=True)
print(f"Output directory ready: {output_dir}")

# Calculate total number of maps
total_maps = 0
skipped_maps = 0

# Loop through SSP scenarios
for ssp in ssp_scenarios:
    print(f"\n{'='*60}")
    print(f"Processing SSP: {ssp}")
    print(f"{'='*60}")

    # Read the CSV for this SSP
    input_file = f'../data/processed/{ssp}_disturbance_all_years.csv'
    print(f"Reading {input_file}...")

    try:
        df_all = pd.read_csv(input_file)
        print(f"CSV loaded successfully. Shape: {df_all.shape}")
    except FileNotFoundError:
        print(f"WARNING: File {input_file} not found. Skipping {ssp}.")
        continue

    # Get unique years
    years = sorted(df_all['Year'].unique())
    print(f"Years to process: {years}")

    # Loop through plot types (cumulative vs absolute)
    for plot_type in plot_types:
        print(f"\n>>> Processing plot type: {plot_type}")

        # Loop through each disturbance agent
        for agent_name in disturbance_agents:

            # Determine column name based on plot type
            if plot_type == 'cumulative':
                loss_column = f'{agent_name}_loss_cumsum'
            else:  # absolute
                if agent_name == 'Storm':
                    loss_column = 'Storm_perc_rm'
                else:
                    loss_column = f'{agent_name}_loss'

            print(f"\n  >> Agent: {agent_name} (column: {loss_column})")

            # Check if column exists
            if loss_column not in df_all.columns:
                print(f"     WARNING: Column {loss_column} not found. Skipping.")
                continue

            cbar_max_label = None  # default: no label override
            # Calculate vmax based on plot type
            if plot_type == 'cumulative':
                # Use 50% of global maximum across all years
                max_loss = df_all[loss_column].max()
                vmax = max_loss * 0.5
                print(f"     Max cumulative: {max_loss:.2f}, Using 50%: {vmax:.2f}")
            else:
               # For absolute plots
               if agent_name == 'Storm':
                   # For Storm running mean: use 95th percentile across ALL years
                   vmax = df_all[loss_column].quantile(0.95)
                   print(f"     Using 95th percentile: {vmax:.2f}")

                   lim = np.max(np.abs(df_all[loss_column]))
                   vmin = -lim
                   norm = TwoSlopeNorm(vmin=vmin, vcenter=0, vmax=vmax)
               else:
                   # For Fire and Insect: calculate median of annual maxima
                   annual_maxima = []
                   for year in years:
                       year_data = df_all[df_all['Year'] == year][loss_column]
                       if len(year_data) > 0:
                           annual_maxima.append(year_data.max())
                   reference_max = np.median(annual_maxima)
                   vmax = reference_max * 0.9
                   print(f"     Annual maxima range: {min(annual_maxima):.2f} - {max(annual_maxima):.2f}")
                   print(f"     Median annual max: {reference_max:.2f}, Using 50%: {vmax:.2f}")
            #if plot_type == 'cumulative':
                # For cumulative: use 50% of global maximum across all years
            #    max_loss = df_all[loss_column].max()
            #    vmax = max_loss * 0.5
            #    print(f"     Max cumulative: {max_loss:.2f}, Using 50%: {vmax:.2f}")
            #else:
                # For absolute: calculate median of annual maxima, then use 50%
            #    annual_maxima = []
            #    for year in years:
            #        year_data = df_all[df_all['Year'] == year][loss_column]
        #            if len(year_data) > 0:
        #                annual_maxima.append(year_data.max())

                # Use median of annual maxima as reference
        #        reference_max = np.median(annual_maxima)
        #        vmax = reference_max * 0.9
        #        print(f"     Annual maxima range: {min(annual_maxima):.2f} - {max(annual_maxima):.2f}")
        #        print(f"     Median annual max: {reference_max:.2f}, Using 50%: {vmax:.2f}")

            # Loop through years
            for year in years:
                # Check if file already exists
                filename = f'{output_dir}{agent_name}_{ssp}_{plot_type}_{year}.png'

                if os.path.exists(filename):
                    print(f"    > {year}: ALREADY EXISTS, skipping")
                    skipped_maps += 1
                    continue

                print(f"    > Creating map for {year}...", end='', flush=True)

                df = df_all[df_all['Year'] == year].copy()

                if len(df) == 0:
                    print(f" SKIPPED (no data)")
                    continue

                # Aggregate by location
                df_sum = df.groupby(['Lon', 'Lat']).agg({
                    loss_column: 'sum',
                    'FOREST': 'mean'
                }).reset_index()

                df_sum.columns = ['Lon', 'Lat', 'loss', 'forest']
                df_sum = df_sum.dropna()

                if len(df_sum) == 0:
                    print(f" SKIPPED (no valid data)")
                    continue

                # Create map
                fig = plt.figure(figsize=(11, 8))
                ax = plt.axes(projection=ccrs.PlateCarree())
                ax.set_extent([lon_min, lon_max, lat_min, lat_max], crs=ccrs.PlateCarree())

                ax.add_feature(cfeature.COASTLINE.with_scale('50m'))
                ax.add_feature(cfeature.BORDERS.with_scale('50m'), linestyle=':')
                ax.add_feature(cfeature.LAND.with_scale('50m'), facecolor='lightgray', alpha=0.5)

                # Point sizes: scale forest fraction
                min_size = 5
                max_size = 30
                sizes = min_size + df_sum['forest'] * (max_size - min_size)


                # Scatter plot with vmax calculated above (consistent across years for each plot type)
                sc = ax.scatter(df_sum['Lon'], df_sum['Lat'],
                               s=sizes,
                               c=df_sum['loss'],
                               cmap='Reds',
                               vmin=0,
                               vmax=vmax,
                               alpha=0.85,
                               edgecolor='k', linewidth=0.25)

                # Colorbar label depends on plot type
                if plot_type == 'cumulative':
                    cbar_label = 'Cumulative Loss (kgC/m²)'
                else:
                    cbar_label = 'Annual Loss (kgC/m²)'

                cbar = plt.colorbar(sc, ax=ax, orientation='vertical', pad=0.03, shrink=0.65)
                cbar.set_label(cbar_label, fontsize=12)

                # Title includes SSP and plot type
                title_type = 'Cumulative' if plot_type == 'cumulative' else 'Annual'
                ax.set_title(f'{agent_name} {title_type} Disturbance Loss - {ssp.upper()} - {year}',
                           fontsize=16, fontweight='bold')

                # Size legend for forest fraction
                forest_raw = df_sum['forest'].values
                legend_vals = np.linspace(forest_raw.min(), forest_raw.max(), 4)
                legend_sizes = min_size + legend_vals * (max_size - min_size)

                for val, s in zip(legend_vals, legend_sizes):
                    ax.scatter([], [], s=s, color='gray', alpha=0.7, label=f'{val:.2f}')

                legend = ax.legend(
                    scatterpoints=1,
                    frameon=True,
                    title="Forest fraction",
                    loc='lower right',
                    fontsize=10
                )
                legend.get_title().set_fontsize(11)

                # Save with clear naming: agent_ssp_plottype_year.png
                plt.savefig(filename, dpi=300, bbox_inches='tight')
                plt.close()

                total_maps += 1
                print(f" DONE (created: {total_maps}, skipped: {skipped_maps})")

print(f"\n{'='*60}")
print(f"Script complete!")
print(f"Maps created: {total_maps}")
print(f"Maps skipped (already existed): {skipped_maps}")
print(f"Total: {total_maps + skipped_maps}")
print(f"{'='*60}")
