# in-situ-monitoring-of-direct-ink-writing
 Code and sample data for in-situ monitoring of print stability during direct ink writing
 
 Additional video files are available at Mendeley Data: http://dx.doi.org/10.17632/2236m9683h.1

Mathematica package files (.wl) and aggregate data for the paper "In situ characterization of low-viscosity direct ink writing: stability, wetting, and rotational flows." Code describes image analysis techniques for analyzing stability, wetting, and rotational flows in videos of a direct ink writing process. An example video set is included. 

"gras" refers to the Grasshopper camera which was used to 
	capture videos from the side (stability/wetting)
"flea" refers to the Flea camera which was used to
	capture videos from the back (rotational flows).
--------------------
VIDEO ANALYSIS WORKFLOW

1. calibration.wl
	describe the nozzle geometry and substrate position for a video

2. extrusionBehaviors.wl
	detect and analyze fluid surfaces for each frame in a video
	(or 40-80 frames, for stable flows)

3. aggregation.wl
	summarize data across many frames, describing average values
	and characterizing periodicity of droplets

4. displacements.wl
	analyze xz videos to find particle displacements
--------------------
TOOLBOXES

fileManagement.wl
	handling this file naming convention

--------------------
DATA

summaries4.csv
	list of aggregate video data

thicknesstable.csv
	aggregate filament height data

dispsSummary.csv
	aggregate particle displacements for rotational flows

Example video
	flea_disps2_T25_H150_FDTS_film_vF1.5_vSfast_171024165320.csv
		displacements

	flea_T25_H150_FDTS_film_vF1.5_vSfast_171024165320.avi
		video from the back

	gras_T25_H150_FDTS_film_vF1.5_vSfast_171024165320.avi
		video from the side

	grasGeoA_T25_H150_FDTS_film_vF1.5_vSfast_171024165320.txt
		geometry stats about video

	grasHeights_T25_H150_FDTS_film_vF1.5_vSfast_171024165320.csv
		table of stage heights in um

	grasHeights_T25_H150_FDTS_film_vF1.5_vSfast_171024165320.tiff
		visual representation of tilt

	grasPositions3_linex_T25_H150_FDTS_film_vF1.5_vSfast_171024165320.csv
		list of fluid surface measurements over time
	
	grasSummary4_T25_H150_FDTS_film_vF1.5_vSfast_171024165320.csv
		summarized fluid surface measurements over time
