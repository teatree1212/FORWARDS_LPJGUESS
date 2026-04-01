import imageio
import os
from pathlib import Path
from PIL import Image

print("Starting GIF creation script...")

output_dir = '../Figures_and_reports/Annual_Meeting/'

# Disturbance agents
disturbance_agents = ['Fire', 'Storm', 'Insect']
ssp_scenarios = ['ssp126', 'ssp370']
plot_types = ['cumulative', 'absolute']

# Target duration for each animation
target_duration_seconds = 20

print(f"Looking for PNG files in: {output_dir}")
print(f"Target animation duration: {target_duration_seconds} seconds\n")

total_created = 0
total_skipped = 0

# Loop through all combinations
for agent_name in disturbance_agents:
    for ssp in ssp_scenarios:
        for plot_type in plot_types:

            # Create pattern to match files
            pattern = f"{agent_name}_{ssp}_{plot_type}_"

            # Define output filename
            gif_filename = f'{output_dir}{agent_name}_{ssp}_{plot_type}_animation.gif'

            # Check if GIF already exists
            if os.path.exists(gif_filename):
                print(f">>> {agent_name} - {ssp} - {plot_type}: ALREADY EXISTS, skipping...\n")
                total_skipped += 1
                continue

            print(f">>> Creating GIF for {agent_name} - {ssp} - {plot_type}...")

            # Get all PNG files for this combination, sorted by year
            png_files = sorted([f for f in os.listdir(output_dir)
                               if f.startswith(pattern) and f.endswith('.png')])

            if len(png_files) == 0:
                print(f"  No PNG files found, skipping...")
                continue

            print(f"  Found {len(png_files)} PNG files")

            # Calculate duration per frame in milliseconds
            duration_per_frame = int((target_duration_seconds * 1000) / len(png_files))
            print(f"  Duration per frame: {duration_per_frame}ms ({target_duration_seconds}s for {len(png_files)} frames)")

            # Read images in order using PIL
            print(f"  Reading images...", end='', flush=True)
            images = []
            for png_file in png_files:
                img_path = os.path.join(output_dir, png_file)
                img = Image.open(img_path)
                images.append(img)
                print(".", end='', flush=True)
            print(" Done!")

            # Create GIF with PIL (better compatibility)
            print(f"  Writing GIF to: {gif_filename}...", end='', flush=True)

            # Save as GIF using PIL with proper settings for PowerPoint
            images[0].save(
                gif_filename,
                save_all=True,
                append_images=images[1:],
                duration=duration_per_frame,
                loop=0,  # 0 = infinite loop
                optimize=False  # Don't optimize, can cause issues in PowerPoint
            )

            print(" Done!")

            # Get file size
            file_size_mb = os.path.getsize(gif_filename) / (1024 * 1024)
            print(f"  GIF created: {agent_name}_{ssp}_{plot_type}_animation.gif ({file_size_mb:.2f} MB)\n")

            total_created += 1

print("="*60)
print(f"All GIFs processed!")
print(f"Created: {total_created}")
print(f"Skipped (already existed): {total_skipped}")
print(f"Total: {total_created + total_skipped}")
print("="*60)
