(* ::Package:: *)

dir = NotebookDirectory[];
file = FileNameJoin[{dir, "extrusionBehaviors.wl"}];
If[FileExistsQ[file], Import[file]];
Clear[dir, file];


(* ::Section:: *)
(*Detect image features*)


(* ::Subsection:: *)
(*summarizing*)


(* ::Subsubsection:: *)
(*initializeNozzle[video_, frames_, diag_, finaldiag_, export_]*)


(* ::Text:: *)
(*drawNozzle draws nozzle lines on an image*)
(*video = file name *)
(*frames = list of frames (list of ints or {} for automatic selection)*)
(*diag = true to print diagnostics (bool)*)
(*finaldiag = true to print the final results (bool)*)
(*OUTPUT: annotated image (graphics object)*)
(*NEEDS: staticNozzle, importFrame, adjustFrams*)


initializeNozzle[video_, frames_, diag_, finaldiag_, export_]:=Module[{noz, image, be, firstmid, ppmm, criticalPoints, newfilename, retval, measured, frams, fr},
Catch[
	If[!FileExistsQ[video], Throw[ConstantArray["File does not exist",6]]];
	If[Length[frames]>10
		,
		fr = Import[video, "FrameRate"];
		frams = Select[frames, #>4*fr&];
		frams = Partition[frams, Round[Length[frams]/6]][[;;,1]];
		,
		frams = frames;
	];
	frams = adjustFrams[video, frams];
	noz = staticNozzle[video, frams, diag];
		(*get lines*)
	ppmm = calibratePPMM[noz];
		(*get image scale*)
	firstmid = calibrateSubstrate[video, noz, frams[[1;;SUBSTRATEFRAMES+1]], ppmm, diag];
		(*get substrate*)
	image = importFrame[video, frams[[1]]];
		(*get first image*)
	be = botEdges[video, frams, noz, firstmid, diag];
		(*get bottom edge*)
	criticalPoints = calibrateEdges[noz, be];
		(*measure points and scale*)
	If[Length[noz]<2
		, Throw[HighlightImage[image, Text["Nozzle detection failed"]]]
	];
	
	measured = Column[{
					video
					, drawNozzleOnImage[image, noz, firstmid, be, ppmm, criticalPoints]
				}];
	retval = {video, noz, be, firstmid, criticalPoints, ppmm};
				
	If[export, (*export measurements*)
		newfilename = StringReplace[video, {"gras"->"grasGeometry", ".avi"->".csv"}];		
		Export[newfilename, retval];
		
		(*export visual representation of measurements*)
		newfilename = StringReplace[video, {"gras"->"grasGeometry", ".avi"->".tiff"}];
		Export[newfilename, measured];
	];
	
	If[finaldiag, Print[measured]];
			
	Return[retval]
]]


(* ::Subsubsection:: *)
(*calibratePPMM*)


(* ::Text:: *)
(*find the image scale in px/mm*)
(*noz = nozzle left/right edge list {{{x0_, y0_}, dxdyN_}, {{x0_, y0_}, dxdyN_}}*)
(*OUTPUT: scale in pixels per mm*)
(*NEEDS: pldist*)


calibratePPMM[noz_]:=Module[{nozzlewpx, ppmm},
nozzlewpx = pldist[noz[[1,1,1]],noz[[1,1,2]], -1/noz[[2,2]], noz[[2,1,2]]+1/noz[[2,2]]*noz[[2,1,1]]];
ppmm = nozzlewpx/NOZZLEWIDTH
]


(* ::Subsubsection:: *)
(*intersection[nozi_, be_]*)


(* ::Text:: *)
(*calculates the intersection point between a nozzle line and a bot edge line*)
(*{{x0_, y0_}, dxdyN_} = one nozzle line*)
(*{{x1_, y1_}, dydxB_} = bot edge line*)
(*OUTPUT = point in {x,y} coords*)
(*NEEDS:*)


intersection[{{x0_, y0_}, dxdyN_}, {{x1_, y1_}, dydxB_}]:=Module[{x},
x = (y0-1/dxdyN*x0 - y1 + dydxB*x1)/(dydxB - 1/dxdyN);
{x, y1+dydxB*x}]


(* ::Subsubsection:: *)
(*calibrateEdges[noz_, be_]*)


(* ::Text:: *)
(*find important intersection points between lines noz and be*)
(*noz = nozzle left/right edge list {{{x0_, y0_}, dxdyN_}, {{x0_, y0_}, dxdyN_}}*)
(*be = nozzle bottom edge {{x1_, y1_}, dydxB_} *)
(*OUTPUT: {left corner, right corner, channel} in {x,y} coords*)
(*NEEDS: intersection*)


calibrateEdges[noz_, be_]:=Module[{nozzlewpx, ppmm, intersections, nozzleExit},
If[Length[be]==2,
	intersections = intersection[#, be]&/@noz;
	nozzleExit = intersections[[1]] + NOZEXITF * (intersections[[2]] - intersections[[1]]);
	,
	intersections = {};
	nozzleExit = {};
];
Append[intersections, nozzleExit]
]


(* ::Subsection:: *)
(*nozzle detection*)


(* ::Text:: *)
(*NEEDS: file handling*)


(* ::Subsubsection:: *)
(*lineslope[line_]*)


(* ::Text:: *)
(*lineslope finds the slope of the line dx/dy*)
(*line = line consisting of two points {{x1, y1}, {x2, y2}}*)
(*OUTPUT: number dx/dy*)
(*NEEDS: *)


lineslope[line_]:=Module[{dy},
dy = (line[[2,2]] - line[[1,2]]);
If[Abs[dy]>0,
	(line[[2,1]] - line[[1,1]])/dy
	,
	10000
]]


(* ::Subsubsection:: *)
(*lineangle[line_]*)


(* ::Text:: *)
(*lineangle finds the angle of the line with the vertical edge*)
(*line = line consisting of two points {{x1, y1}, {x2, y2}}*)
(*OUTPUT: angle in degree*)
(*NEEDS: lineslope*)


lineangle[line_]:=ArcTan[lineslope[line]]/Degree;


(* ::Subsubsection:: *)
(*linedistances[lines_]*)


(* ::Text:: *)
(*linedistances outputs a table of distances and angles between lines*)
(*lines = list of lines, each in format {{x1, y1}, {x2, y2}}*)
(*OUTPUT: table of entries {line 1, line 2, x distance between lines, angle difference between lines}*)
(*NEEDS: lineangle*)


linedistances[lines_]:=Flatten[Table[{{lines[[i]]
										, lines[[j]]}
										, Abs[lines[[i,1,1]] - lines[[j,1,1]]]
										, Abs[lineangle[lines[[i]]] - lineangle[lines[[j]]]]}
								, {i, 2,Length[lines]}
								, {j, i-1}],1];


(* ::Subsubsection:: *)
(*ptpt2ptslope[line_, xshift_, yshift_]*)


(* ::Text:: *)
(*ptpt2ptslope converts a line represented by two points to a line represented by one point and a slope*)
(*line = line consisting of two points {{x1, y1}, {x2, y2}}*)
(*xshift, yshift = amount to shift the point (ints)*)
(*OUTPUT: {{x,y}, dx/dy}*)
(*NEEDS: lineslope*)


ptpt2ptslope[line_, xshift_, yshift_]:=Module[{sorted},
sorted = SortBy[line, #[[2]]&];
{sorted[[2]] + {xshift, yshift}, lineslope[sorted]}]


(* ::Subsubsection:: *)
(*nozzleEdges[image_, diag_]*)


(* ::Text:: *)
(*nozzleEdges determines the sides of the nozzle*)
(*image = input grayscale image*)
(*diag = bool whether or not to print diagnostics*)
(*OUTPUT: two lines expressed as {{x,y}, dx/dy}*)
(*NEEDS: lineangle, linedistances, ptpt2ptslope*)


nozzleEdges[image_, diag_]:=Module[{edge, lines, edgeradius, ld, linedist, im, id, botpad, leftpad},
Catch[
	If[!ImageQ[image], Return[0]];
			(*make sure inputs are in the right format*)
	lines = {};
			(*start with no lines*)
	edgeradius = 3; 
			(*distance to use for edge detection*)
	id = ImageDimensions[image];
	leftpad = id[[1]]/2;
	botpad = id[[2]]/5;
	im = ImagePad[image, {{-leftpad, 0}, {-botpad, 0}}]; 
			(*crop out left half and bottom fifth of image; nozzle won't be there*)
	While[Length[lines]<2 && edgeradius<6,
		edge = EdgeDetect[im, edgeradius];
			(*detect edges in image*)
		lines = ImageLines[edge];
			(*detect lines in image*)
		lines = Select[lines, 0>lineangle[#]>-3&];
			(*select vertical lines*)
		ld = linedistances[lines];
			(*find distances between lines*)
		linedist = Select[ld, EXPECTEDNOZWIDTH - EXPECTEDNOZVARIATION<#[[2]]<EXPECTEDNOZWIDTH + EXPECTEDNOZVARIATION&];
			(*select lines that are around 225 pixels apart: SPECIFIC TO THESE IMAGES*)
		linedist = MinimalBy[linedist, #[[3]]&];
			(*select the set of lines with the most similar angle*)
		lines = Flatten[linedist[[;;,1]], 1];
			(*get just the lines*)
		edgeradius = edgeradius + 1;
	];
	If[diag, Print[Show[HighlightImage[edge, Line/@lines], ImageSize->400]]];
	lines = SortBy[lines, #[[1,1]]&];
	lines = ptpt2ptslope[#, leftpad, botpad]&/@lines;
	lines
]]


(* ::Subsubsection:: *)
(*staticNozzle[video_, frames_, diag_]*)


(* ::Text:: *)
(*staticNozzle finds the nozzle for all given frames*)
(*video = file name *)
(*frames = list of frames (list of ints or {} for automatic selection)*)
(*diag = true to print diagnostics (bool)*)
(*OUTPUT: two lines expressed as {{x,y}, dx/dy}*)
(*NEEDS: adjustFrams, importFrame, nozzleEdges*)


staticNozzle[video_, frames_, diag_]:=Module[{images, candidateEdges, noz												
												, height, frams, errorRet, count},
Catch[
	errorRet = 0;
	If[!FileExistsQ[video], Throw[errorRet]]; 
		(*check that the video exists*)
	frams = adjustFrams[video, frames];
	frams = frams[[1;;Min[NOZFRAMES, Length[frams]]]];
		(*get list of frame numbers*)
	If[Length[frams]<1, Throw[errorRet]]; 
		(*check that the given frames exist*)
	candidateEdges = {};
	count = 0;
	While[Length[candidateEdges]<1 && count<10,
		images = importFrame[video, #]&/@frams; 
			(*get frames*)
		candidateEdges = nozzleEdges[#, diag]&/@images; 
			(*get nozzle edges for each frame*)
		candidateEdges = Select[candidateEdges, Length[#]>1&]; 
			(*get valid edges*)
		frams = (#+1)&/@frams;
			(*if we don't have any valid edges, try different frames*)
		count = count+1;
	];
	If[diag, Print[Grid[candidateEdges]]];
	noz = Table[Median[candidateEdges[[;;, i, j]]], {i, 1, 2}, {j, 1, 2}]; 
		(*select the median edges*)
	Return[noz];
]]


(* ::Subsection:: *)
(*lift detection*)


(* ::Text:: *)
(*NEEDS: file handling, nozzle input*)


(* ::Subsubsection:: *)
(*isreflect[edge_, noz_, diag_]*)


(* ::Text:: *)
(*isreflect determines if we can see the reflection on the substrate*)
(*image = binary edge image (image)*)
(*noz = nozzle lines (2*{{x,y}, dx/dy})*)
(*OUTPUT: true if we can see the reflection (bool)*)
(*NEEDS: getPointsFromBW, pldist*)


isreflect[edge_, noz_, diag_]:=Module[{mb, pts, distances, goodpoints, leftpad, toppad, id, im, critpoints, goodpointsleft, goodpointsright},
	id = ImageDimensions[edge];
	leftpad = id[[1]]/2;
	toppad = id[[2]]*EXPECTEDSUBSTRATEFRACTION;
	im = ImagePad[edge, {{-leftpad, 0}, {0, -toppad}}]; 
		(*take just the bottom right segment of the image*)
	pts = getPointsFromBW[im];
		(*get list of edge points*)
	pts = (# + {leftpad, 0})&/@pts;
		(*modify points to adjust for cropping*)
	goodpoints = pointsnearnozzle[pts, noz, ALLOWABLEREFLECTIONRADIUS];
	goodpointsleft = Length@Select[goodpoints, #[[1]]<Mean[MinMax[goodpoints[[;;,1]]]]&];
	goodpointsright = Length@Select[goodpoints, #[[1]]>Mean[MinMax[goodpoints[[;;,1]]]]&];
	critpoints = (*0.1*(id[[2]] - toppad);*)CRITREFLECTIONPOINTS;
	If[(goodpointsleft>critpoints && goodpointsright>critpoints) || goodpointsleft>5*critpoints || goodpointsright>5*critpoints
		, Return[True]
		, If[diag, Print["ERROR: No reflection, ", goodpointsleft, "/", critpoints, " left points, ", goodpointsright, "/", critpoints, " right points."]];
		  Return[False]
	];
]


(* ::Subsubsection:: *)
(*pointsnearnozzle[pts_, noz_, radius_]*)


(* ::Text:: *)
(*pointsnearnozzle finds the list of points near the nozzle*)
(*pts = list of points {x,y}*)
(*noz = nozzle lines (2*{{x,y}, dx/dy})*)
(*radius = critical distance away from the nozzle line*)
(*OUTPUT: list of points {x,y}*)
(*NEEDS: pldist*)


pointsnearnozzle[pts_, noz_, radius_]:=Module[{mb, distances, goodpoints},
	mb = Table[{1/noz[[i,2]],noz[[i,1,2]] - 1/noz[[i,2]]*noz[[i,1,1]]}, {i, 2}];
		(*slopes and intercepts of nozzle edges: y = mx+b*)
	distances = Flatten[Table[{p, pldist[p[[1]], p[[2]], mb[[i,1]], mb[[i,2]]]}, {p, pts}, {i, 2}],1];
		(*distances from edge points to nozzle lines*)
	goodpoints = Select[distances, #[[2]]<radius&][[;;, 1]];
		(*select points within 5 pixels of nozzle lines*)
	goodpoints
]


(* ::Subsection:: *)
(*substrate detection*)


(* ::Text:: *)
(*NEEDS: file handling, nozzle input, lift detection*)


(* ::Subsubsection:: *)
(*mirrorPlane[image_, noz_, midsuggest_, be_, ppmm_, criticalPoints_, diag_]*)


(* ::Text:: *)
(*mirrorPlane determines the location of the top-bottom mirror plane in an image*)
(*image = input grayscale image (image)*)
(*noz = nozzle lines (2*{{x,y}, dx/dy})*)
(*midsuggest = mid position MEASURED FROM BOTTOM (int)*)
(*be = bot edge line {{x,y}, {dy/dx}}*)
(*ppmm = pixels per mm (int)*)
(*criticalPoints = list of {x,y} points on bottom of nozzle {left, right, mid}*)
(*diag = true to output diagnostic data (bool)*)
(*OUTPUT: {mirror plane distance FROM BOTTOM (int), standoff distance (int)}*)
(*NEEDS: isreflect, tbdifferences, adji, mpdiagprint*)


mirrorPlane[image_, noz_, midsuggest_, be_, ppmm_, criticalPoints_, diag_]:=Module[{errorRet, imdims, heightHist, edgedistance, baseline
											, denominator, weightedaverage, ymid, ms, standoffDistance, iminold, imaxold
											, imin, imax, loop, differencestable, mirrorloc, edge, mirrorlocBottom, icrit},
Catch[ 
	errorRet = {0, 1000};
		If[!ImageQ[image], 
			If[diag, Print["ERROR: Image is not an image!"]];
			Throw[errorRet]
		];
	edge = EdgeDetect[image, 3];
		If[!isreflect[edge, noz, diag], 
			If[diag, mpdiagprint[edge,0,0,errorRet,{0,0,0,0},{},{},image, noz, be, ppmm, criticalPoints, 1000]];
			Throw[errorRet]
		];
	imdims = ImageDimensions[image];
	If[midsuggest>0
		,{baseline, weightedaverage, heightHist} = {0, imdims[[2]]-midsuggest, {}};
		, ms = lmsuggest[edge, imdims, midsuggest];
		 If[ms>0
		    ,{baseline, weightedaverage, heightHist} = {0, ms, {}};
			,{baseline, weightedaverage, heightHist} = midwa[edge, imdims, errorRet, diag];
		 ];
	];
	ymid = imdims[[2]]/2; 
		(*y midpoint*)
	loop = True;
	mirrorloc = errorRet;
	differencestable = {{}};
	{imin, imax, iminold, imaxold, icrit} = ilimits[weightedaverage, ymid, criticalPoints];
	
	While[loop && imin>-ymid && imax<=icrit && imin<imax,
		(*calculate the intensity in common for each cropping distance, loop until we're confident*)
		differencestable = Join[Table[tbdifferences[i, edge, imdims, ymid], {i, imin, iminold, 2}]
								,differencestable
								,Table[tbdifferences[i, edge, imdims, ymid], {i, imaxold, imax, 2}]
								];
		differencestable = DeleteDuplicates[Select[differencestable, Length[#]>0&]];
		mirrorloc = MaximalBy[differencestable, #[[2]]&][[1]]; (*find reflection axis with most pixels in common*)
		{iminold, imaxold} = {imin, imax};
		{imin, imax, loop} = adji[mirrorloc, differencestable, imax, imin, midsuggest, diag];
	];
	If[Length[mirrorloc]>0
		, mirrorlocBottom = imdims[[2]] - mirrorloc[[1]];
		  If[Length[criticalPoints]>2
			, standoffDistance = 1000*(criticalPoints[[3,2]] - mirrorlocBottom)/ppmm;
			, standoffDistance = 0;
		  ];	
			(*calculate standoff distance as difference between mirror loc and channel exit, in micron*)
		  If[diag, mpdiagprint[edge,baseline,weightedaverage,mirrorloc,{imin, imax, icrit, ymid},differencestable,heightHist,image, noz, be, ppmm, criticalPoints, standoffDistance]];
		  Return[{mirrorlocBottom, standoffDistance}]
		, Return[errorRet]
	];
]]


(* ::Subsubsection:: *)
(*ilimits[weightedaverage_, ymid_, criticalPoints_]*)


(* ::Text:: *)
(*determines the limits in i to search*)
(*weightedaverage = y location of weighted average FROM THE BOTTOM*)
(*ymid = y location of image midpoint*)
(*criticalPoints = list of nozzle bottom points*)
(*OUTPUT = list of important looping parameters for mirrorPlane*)
(*NEEDS: SEARCHRADIUS*)


ilimits[weightedaverage_, ymid_, criticalPoints_]:=Module[{ishift, imin, imax, iminold, imaxold, icrit},
	ishift = (weightedaverage-ymid);
	imin = Round[Max[-2*(ishift + 2*SEARCHRADIUS), -ymid+2],2];
	imax = Round[Min[-2*(ishift - 2*SEARCHRADIUS), ymid-2],2];
	iminold = imaxold = Mean[{imin, imax}];
	iminold = Floor[iminold];
	imaxold = Ceiling[imaxold];
	If[Length[criticalPoints]==3 && Length[criticalPoints[[1]]]==2
		,icrit = 2*(-ymid + Min[criticalPoints[[;;,2]]]);
		,icrit = ymid;
	];
	imax = Min[imax, icrit];
	{imin, imax, iminold, imaxold, icrit}]


(* ::Subsubsection:: *)
(*lmsuggest*)


(* ::Text:: *)
(*lmsuggest determines an estimate for the midplane FROM THE TOP *)
(*edge = edge image*)
(*imdims = image dimensions {width, height} in px*)
(*msuggest = suggested midplane in px*)
(*OUTPUT: midplane location in px from the top*)
(*NEEDS: lineangle*)


lmsuggest[edge_, imdims_, msuggest_]:=Module[{lines, msug},
Catch[
lines = ImageLines[edge, MaxFeatures->2];
	(*detect lines on the whole edge image to determine if there is a continuous filament*)
If[Length[lines]<2, Return[msuggest]];
If[Abs[lines[[1,2,1]] - lines[[1,1,1]]]==imdims[[1]] && Abs[lines[[1,2,1]] - lines[[1,1,1]]]==imdims[[1]] && lineangle[lines[[1]]]>80 && lineangle[lines[[2]]]>80
	, Return[imdims[[2]] - Mean[{lines[[1,1,2]], lines[[2,1,2]]}]];
		(*if the lines are horizontal, return the space halfway between the lines*)
	, Return[msuggest]
];
]]


(* ::Subsubsection:: *)
(*midwa*)


(* ::Text:: *)
(*midwa determines an estimate for the midplane FROM THE TOP based on a center of mass of pixels*)
(*edge = edge image*)
(*imdims = image dimensions*)
(*errorRet = value to return in case of error*)
(*diag = bool true to print diagnostics*)
(*OUTPUT: {list of background points, center of gravity, reflection intensity histogram}*)
(*NEEDS; *)


midwa[edge_, imdims_, errorRet_, diag_]:=Module[{heightHist, edgedistance, baseline, denominator, weightedaverage},
	heightHist = Total/@ImageData[edge]; 
		(*histogram along image height*)
	edgedistance = Round[imdims[[2]]*BASELINERATIO]; 
		(*how far from the edge to use for baseline measurement*)
	baseline = 5*Median[Join[heightHist[[1;;edgedistance]],heightHist[[-edgedistance;;]]]]; 
		(*background noise*)
	denominator = Sum[If[heightHist[[i]]>baseline, heightHist[[i]], 0], {i, Length[heightHist]}];
		(*total intensity*)
		If[denominator==0, 
			If[diag, Print["ERROR: No peaks in intensity histogram"]];
			Throw[errorRet]
		];
	weightedaverage = Round[Sum[i*(If[heightHist[[i]]>baseline, heightHist[[i]], 0])
									, {i, Length[heightHist]}]  /denominator]; 
		(*center of gravity*)
	{baseline, weightedaverage, heightHist}
]


(* ::Subsubsection:: *)
(*tbdifferences*)


(* ::Text:: *)
(*tbdifferences is a subfunction of mirrorPlane*)
(*tbdifferences determines a midplane location FROM THE TOP and an intensity value representing the goodness of that location*)
(*i = number of pixels from middle to offset midplane*)
(*edge = edge image*)
(*imdims = image dimensions*)
(*ymid = pixel location of middle of image*)
(*OUTPUT: midplane location, intensity of reflected image*)
(*NEEDS: imageSum*)


tbdifferences[i_, edge_, imdims_, ymid_]:=Module[{croppededge, partitioned, difference, differenceval},
croppededge = ImagePad[edge, {{0,0}, {0,i}}]; (*remove or add pixels on top of the image*)
partitioned = ImagePartition[croppededge,{imdims[[1]], (ymid + i/2)}][[;;,1]]; (*split in half*)
difference = ImageMultiply[ImageReflect[#[[2]]], #[[1]]]&@partitioned; (*find pixels in common*)
differenceval = imageSum[difference]; (*count pixels in common*)
{(ymid - i/2), differenceval}
];


(* ::Subsubsection:: *)
(*adji*)


(* ::Text:: *)
(*adji adjust the bounds of the search radius for mirrorPlane*)
(*mirrorloc = location of mirrorplane*)
(*differencestable = list of {plane location, intensity}*)
(*{imin, imax} = initial search radius*)
(*midsuggest = initial midplane*)
(*diag = bool true to print diagnostics*)
(*OUTPUT: {new search min, new search max, bool to continue looping}*)
(*NEEDS: *)


adji[mirrorloc_, differencestable_, imax_, imin_, midsuggest_, diag_]:=Module[{max, min, loop},
	max = imax;
	min = imin;
	loop = False;
(*	Print[differencestable];*)
(*	If[diag, Print[differencestable[[-1]], mirrorloc, differencestable[[1]]]];*)
	If[mirrorloc==differencestable[[1]](*if the maximum is on the each of the search area, expand*)	
		, max = imin + 2; min = imin - SEARCHRADIUS; loop = True; 	
		,If[mirrorloc==differencestable[[-1]]
			, min = imax - 2; max = imax + SEARCHRADIUS; loop = True;
			, If[mirrorloc[[2]]<CRITMIRRORPOINTS && Abs[mirrorloc[[1]] - midsuggest]>2,
						(*HARD CUTOFF: SPECIFIC TO THESE IMAGES. Need at least 200 shared points to keep this result, otherwise expand the search*)
				min = imin-3; max = imax+3; loop = True;
			];
		];
	];
	{min, max, loop}
];


(* ::Subsubsection:: *)
(*mpdiagprint*)


(* ::Text:: *)
(*mpdiagprint is a subfunction of mirrorPlane*)
(*mirrorloc = height FROM TOP (int)*)


mpdiagprint[edge_, baseline_, weightedaverage_, mirrorloc_, {imin_, imax_, icrit_, ymid_}, differencestable_, heightHist_, image_, noz_, be_, ppmm_, criticalPoints_, stdi_]:=Module[{toprint},
toprint = If[Length[differencestable]>0,
			If[Length[heightHist]>0,
				{differencestable, heightHist, {{weightedaverage, 0}, mirrorloc}}
				,
				{differencestable, {{weightedaverage, 0}, mirrorloc}}
			]
			,
			If[Length[heightHist]>0,
				{heightHist, {{weightedaverage, 0}, mirrorloc}}
				,
				{{{weightedaverage, 0}, mirrorloc}}
			]
		];
Print[Show[ColorNegate[edge], ImageSize->{Automatic, 150}]
			, "\t"
			, Grid[{{"baseline", baseline}
				, {"weighted average", weightedaverage}
				, {"mirror loc", N[mirrorloc]}
				, {"standoff distance", stdi}
				, {"i min", imin}
				, {"i max", imax}
				, {"i crit", icrit}
				, {"ymid", ymid}
				}]
			, "\t"
			,ListPlot[toprint
					, Frame->True
					, AspectRatio->1/2
					, PlotStyle->{Black, Red, Green}
					, PlotMarkers->{\[FilledCircle], \[FilledCircle], {\[FilledCircle], 10}}
					, LabelStyle->Directive[12, Black]
					, ImageSize->{Automatic, 150}
					, PlotRange->All
					, FrameStyle->Black
					, FrameLabel->{"Distance from top", "Intensity"}
				]
			,"\t"
			, drawNozzleOnImage[image, noz, If[Length[mirrorloc]>0, ImageDimensions[image][[2]]-mirrorloc[[1]], 0], be, ppmm, criticalPoints]
];]


(* ::Subsubsection:: *)
(*calibrateSubstrate[video_, noz_, frames_, ppmm_, diag_]*)


(* ::Text:: *)
(*calibrateSubstrate determines a substrate estimate for a video*)
(*video = file name (string)*)
(*numsamples = number of frames to use in estimate (int)*)
(*noz = nozzle lines (2*{{x,y}, dx/dy})*)
(*diag = true to output diagnostic data (bool)*)
(*OUTPUT: plane location FROM THE BOTTOM (int)*)
(*NEEDS: mirrorPlane, importFrame*)


calibrateSubstrate[video_, noz_, frames_, ppmm_, diag_]:=Module[{firstmid, fr, frams, stdist},
firstmid = ConstantArray[0, Length[frames]-1];
frams = adjustFrams[video, frames];
Do[
	fr = frams[[i]];
	While[firstmid[[i]]==0 && fr<frams[[i+1]],
		{firstmid[[i]], stdist} = mirrorPlane[importFrame[video, fr], noz, 0, {}, ppmm, {}, diag];
		fr = fr+100;
	];
,{i, Length[frames]-1}];
firstmid = Median[firstmid];
firstmid
]


(* ::Subsection:: *)
(*nozzle bottom detection*)


(* ::Subsubsection:: *)
(*frontPoint[p0_, noz_, firstmid_]*)


(* ::Text:: *)
(*frontPoint is a subfunction of botEdge*)
(*p0 = list of all points in nozzle region*)
(*noz = nozzle lines*)
(*firstmid = y location of substrate, measured from the bottom*)
(*OUTPUT: point {x,y}*)
(*NEEDS: pointsnearnozzle*)


frontPoint[p0_, noz_, firstmid_]:=Module[{frontBot, pnn},
	frontBot = {0, 0};
	pnn = pointsnearnozzle[p0, noz, ALLOWABLEREFLECTIONRADIUS];
	If[Length[pnn]>0,
		pnn = Select[pnn, #[[1]]>Mean[MinMax[pnn[[;;,1]]]]&& #[[2]]>firstmid+20&];
		If[Length[pnn]>0,
			frontBot = MinimalBy[pnn,#[[2]]&][[1]];
	]];
	frontBot
	]	


(* ::Subsubsection:: *)
(*rm2[list_]*)


(* ::Text:: *)
(*rm2 finds the 2nd lowest point in y in a list, or the lowest point in a list of 1*)
(*list = list of {x,y} points*)
(*OUTPUT: y value (int)*)
(*NEEDS:*)


rm2[list_]:=If[Length[list]>1, RankedMin[list[[;;,2]],2], list[[1,2]]]


(* ::Subsubsection:: *)
(*rm3[list_]*)


(* ::Text:: *)
(*rm3 finds the midpoint between the 1st and 2nd lowest point in y in a list, or the lowest point in a list of 1*)
(*list = list of {x,y} points*)
(*OUTPUT: y value (int)*)
(*NEEDS:*)


rm3[list_]:=If[Length[list]>1
				, Mean[{Min[list[[;;,2]]]
						, RankedMin[list[[;;,2]],2]
					}]
				, list[[1,2]]
			]


(* ::Subsubsection:: *)
(*fitBottomEdge[p0_, noz_, frontBot_, firstmid_, lr_]*)


(* ::Text:: *)
(*fitBottomEdge is a subfunction of botEdge*)
(*p0 = list of all points in the nozzle region*)
(*noz = nozzle lines*)
(*frontBot = bottom front point of nozzle*)
(*firstmid = y location of substrate, measured from the bottom*)
(*lr = 1 to fit a line to the left half of the nozzle, 2 to fit a line to the right half*)
(*OUTPUT: {bot edge line, list of points used in fit, R^2 value of fit)*)
(*NEEDS: rm2*)


fitBottomEdge[p0_, noz_, frontBot_, firstmid_, lr_]:=Module[{botleftpoints, med, stdev, line, lmf, m, pts, leftEdge, rightEdge, f},
	If[lr==1
		,leftEdge = noz[[1,1,1]]+EXPECTEDNOZVARIATION;
		 rightEdge = Mean[noz[[;;,1,1]]];
		,leftEdge = Mean[noz[[;;,1,1]]];
		 rightEdge = noz[[2,1,1]] - EXPECTEDNOZVARIATION;
	];
	pts = Select[p0, leftEdge < #[[1]] < rightEdge &];
	If[lr>1,
		pts = Select[pts, #[[2]]> firstmid + 10 &];
	];
	
	Switch[lr
	,1, (*left side, choose highest point*)
		f[l_] := Max[l[[;;,2]]];
	,2, (*right side, choose second lowest point above midline*)
		f[l_] := rm2[l];
	,3, (*right side, split the difference between bottom bloop and next points*)
		f[l_] := rm3[l];	
	];
	
	botleftpoints = {#[[1,1]], f[#]}&/@GatherBy[pts, #[[1]]&];
	
(*	Print[ListPlot[{p0, pts, botleftpoints}]];*)
	med = Median[botleftpoints[[;;,2]]];
	stdev = 5;
	botleftpoints = Select[botleftpoints, med-stdev<#[[2]]<med+stdev&];
		(*select points within 5 px of the median y*)
	If[Length[botleftpoints]>10 && Length[frontBot]==2,
		lmf = LinearModelFit[(# - frontBot)&/@botleftpoints, x,x, IncludeConstantBasis->False];
			(*fix the line to the nozzle front point and fit slope to left points*)
		m = D[lmf[x],x];
			(*slope of line*)
		line = {{0, frontBot[[2]]-m*frontBot[[1]]}, m};
			(*start line at left edge of image*)
		{line, botleftpoints, lmf["RSquared"]}
			(*report R^2 of fit*)
		,
		{{}, botleftpoints, 0}
	]
]


(* ::Subsubsection:: *)
(*bePrint[frontBot_, line_, botleftpoints_, p0_, noz_, r2_]*)


(* ::Text:: *)
(*bePrint is a subfunction of botEdge*)


bePrint[frontBot_, line_, botleftpoints_, p0_, noz_, r2_]:=Print[frontBot
						, "\t"
						, If[Length[line]>1, Abs[frontBot[[2]] - (line[[1,2]] + line[[2]]*frontBot[[1]])], ""]
						,"\t"
						,r2
						,"\t"
						,line
						,"\t"
						, Show[ListPlot[If[Length[botleftpoints]>0, {p0, botleftpoints, {frontBot}},{p0, {frontBot}}]
									, PlotStyle->{Black, Black, Red}
									, PlotMarkers->{{\[FilledCircle], 2}, {\[FilledCircle],8}, {\[FilledCircle], 10}}
									, Frame->None
									, FrameStyle->Black
									, LabelStyle->Directive[10, Black]]
							,If[Length[line]==2
								 ,Plot[line[[1,2]]+line[[2]]*x, {x, noz[[1,1,1]], noz[[2,1,1]]}, PlotStyle->Red]
								 ,Graphics[]
								]
						, ImageSize->300] 
					]


(* ::Subsubsection:: *)
(*botEdge[frame_, noz_]*)


(* ::Text:: *)
(*botEdge fits a line to the bottom edge of the nozzle*)
(*frame = image*)
(*noz = nozzle lines*)
(*firstmid = y location of substrate, measured FROM THE BOTTOM (int)*)
(*diag = true to print diagnostics (bool)*)
(*OUTPUT = {line, 1-r^2}*)
(*NEEDS: crop2nozzle, getPointsFromBW, frontPoint, fitBottomEdge, bePrint*)


botEdge[frame_, noz_, firstmid_, diag_]:=Module[{padding, p0, line, pts, frontBot, botleftpoints, cropped, darkedges, accuracy, r2, lr, fm},
Catch[
	If[!ImageQ[frame], Return[0]];
	padding = 4*EXPECTEDNOZVARIATION;
	cropped = crop2nozzle[frame, noz, padding];
	darkedges = EdgeDetect[Blur[cropped,4]];
	p0 = getPointsFromBW[darkedges];
		(*all points in the nozzle region*)
	p0 =  ({ noz[[1,1,1]] - padding, 0}+#)&/@p0;
		(*adjust coordinates to fix padding*)
	fm = firstmid;
	
	If[Length[p0]<10,
		Return[0]
	];
	
	frontBot = frontPoint[p0, noz, fm];
		(*determine the front bottom edge point*)	

	lr = 1;
		(*start by using silicon side of nozzle. if this fails, use the glass side*)
	While[lr<4,
		{line, botleftpoints, r2} = fitBottomEdge[p0, noz, frontBot, fm, lr];
		(*fit bottom of front to line*)
		If[diag
			, bePrint[frontBot, line, botleftpoints, p0, noz, r2]
		];
		If[Length[line]==2 && -0.5<ArcTan[line[[2]]]/Degree<6
			, (*good fit*) 			  
			  accuracy = 1-r2;
			  lr=10;
			, (*bad fit*)
			  lr = lr+1;
			  accuracy = 1;
			  line = 0;
		];
	];
	
	Return[{line, accuracy}];
]]


(* ::Subsubsection:: *)
(*botEdges[video_, frames_, noz_, firstmid_, diag_]*)


(* ::Text:: *)
(*botEdges finds the median bottom edge assuming that the bottom edge of the nozzle is flat*)
(*video = file name (string)*)
(*frames = list of frame numbers (int) or {}*)
(*noz = nozzle lines (2*{{x,y}, dx/dy})*)
(*firstmid = location of estimated midpoint FROM THE BOTTOM (int)*)
(*diag = true to print diagnostics (bool)*)
(*OUTPUT: bottom edge line {{x,y}, dy/dx}*)
(*NEEDS: adjustFrams, botEdge, importFrame*)


botEdges[video_, frames_, noz_, firstmid_, diag_]:=Module[{bes, frams, count},
frams = adjustFrams[video, frames];
frams = frams[[1;;Min[BOTFRAMES, Length[frams]]]];
bes = {};
count = 0;
While[Length[bes]<1 && count<3,
	bes = botEdge[importFrame[video, #], noz, firstmid, diag]&/@frams;
	If[diag, Print[Column[bes]]];
	bes = Select[bes, Length[#]>0 && Length[#[[1]]]>0&];
	frams = (#+10)&/@frams;
	count = count+1;
];
If[Length[bes]>0,
	MinimalBy[bes, #[[2]]&][[1,1]]
	,
	0
]
]


(* ::Section:: *)
(*tilt*)


(* ::Subsection:: *)
(*file-level operations*)


(* ::Subsubsection:: *)
(*fixedStats[video_]*)


(* ::Text:: *)
(*imports image geometries*)


fixedStats[video_]:=Module[{fn, fs},
	fn = StringReplace[video, {"gras"->"grasGeometry", ".avi"->".csv"}];
	If[FileExistsQ[fn]
		,fs = Import[fn];
		 fs[[2;;]] = ToExpression[fs[[2;;]]];
			(*info is stored in brackets, so is imported as strings*)
		 fs[[1]] = fs[[1,1]];
			(*get string for video*)
		 fs[[4]] = fs[[4,1]];
			(*get int for firstmid*)
		 fs[[6]] = fs[[6,1]];
			(*get int for scale*)
		,fs = initializeNozzle[video, {}, False, False, True]
	];
	fs
];


(* ::Subsection:: *)
(*consolidate plot, and export data*)


(* ::Subsubsection:: *)
(*subtableplot1[subtable_, segmented_, isd_, speeds_, beginpoints_, endpoints_]*)


(* ::Text:: *)
(*subtableplot1 is a plot of h as a function of time*)
(*subtable = list {time, position, h}*)
(*segmented = list of h, split by line {t, h}*)
(*isd = intended standoff distance (int)*)
(*beginpoints = list of beginnings of lines (int time)*)
(*endpoints = list of ends of lines (int time)*)
(*OUTPUT: plot*)
(*NEEDS:*)


subtableplot1[subtable_, segmented_, isd_, speeds_, beginpoints_, endpoints_]:=Module[{gridlines, lines, plot1},
gridlines = Flatten[{{#, (*Red*)Black}&/@endpoints, {#, (*Darker[Green]*)Gray}&/@beginpoints},1];
	(*gridlines to plot*)
lines = {LinearModelFit[#, x,x], Min[#[[;;, 1]]], Max[#[[;;, 1]]]}&/@segmented;
plot1 = Show[ListPlot[subtable[[;;, {1, 3}]]
			, Frame->True
			, FrameLabel->{"Time (s)", "Standoff distance (\[Mu]m)"}
			, FrameStyle->Black
			, LabelStyle->Directive[14, Black]
			, ImageSize->1200
			, PlotStyle->{{Black, PointSize[0.005]},{Red, PointSize[0.005]}}
			, AspectRatio->1/4
			, Epilog->{Line[{{0, isd}, {Max[subtable[[;;, 1]]]*1.1, isd}}]
							, Style[Text[#[[1]], #[[2]]], Medium]&/@speeds}
			, GridLines->{gridlines, None}
			, PlotRange->{{0, All}, {0, Max[Flatten[segmented,1][[;;,2]]]+30}}
			]
		,
	Plot[#[[1]][x], {x, #[[2]], #[[3]]}, PlotStyle->Black]&/@lines
];
plot1
]


(* ::Text:: *)
(*plot stand-off distance vs. time with no regressions*)


subtableplot1naked[subtable_]:=
ListPlot[{subtable[[;;, {1, 3}]]}
			, Frame->True
			, FrameLabel->{"Time (s)", "Standoff distance (\[Mu]m)"}
			, FrameStyle->Black
			, LabelStyle->Directive[14, Black]
			, ImageSize->1200
			, PlotStyle->{{Black, PointSize[0.005]},{Red, PointSize[0.005]}}
			, AspectRatio->1/4
			, PlotRange->All
			];


subtableplot1fromvid[video_]:=Module[{stats, beginnings, ends, subtable, segmented, speeds, plot1, f1},
Catch[
stats = grasGeoAssoc[video];
beginnings = stats["beginnings"];
ends = stats["ends"];
f1 = StringReplace[video, {"gras"->"grasSubs", ".avi"->".csv"}];
If[!FileExistsQ[f1], Throw[Graphics[]]];
subtable = Import[f1];
segmented = Table[
				Select[subtable[[;;, {1,3}]], beginnings[[i]]<#[[1]]<ends[[i]] && #[[2]]<1000&]
			, {i, 9}];
segmented = Table[Select[segmented[[i]], Abs[#[[2]] - Median[segmented[[i,;;, 2]]]] < 3*StandardDeviation[segmented[[i,;;,2]]]&]
			, {i, 9}];
speeds = stats["speeds"];
speeds = Table[
				{speeds[[i]], {Mean[{beginnings[[i]], ends[[i]]}], 20}}
			, {i,9}];
plot1 = subtableplot1[subtable, segmented, stats["h"], speeds, beginnings, ends]
]]


(* ::Subsubsection:: *)
(*nozzleGraphics*)


(* ::Text:: *)
(*a 3D rendering of the nozzle with its tip at point {x,y,z}*)


nozzleGraphics[x_, y_, z_]:=Module[{angle, w, tsi, tboro, sibackx, channelbackx, channelfrontx, borofrontx, botfrontedgey, botbackedgey, width, chwidth, slantheight, totalheight},
chwidth = 0.35;
sibackx = x - 0.525;
channelbackx = x - 0.15;
channelfrontx = x;
borofrontx = x+0.5;

botfrontedgey = y;
botbackedgey = y - chwidth;
angle = 5*Degree;
width = 8;

slantheight = 10*width/(2*Tan[angle]);
totalheight = 10*60;

Graphics3D[{Cuboid[{channelbackx, botfrontedgey, 0}, {channelfrontx, botbackedgey, totalheight}]
				, Polygon[{{sibackx, botfrontedgey, 0},{borofrontx, botfrontedgey, 0}, {borofrontx, botfrontedgey+width/2, slantheight},{sibackx, botfrontedgey+width/2, slantheight}}]
				, Polygon[{{sibackx, botbackedgey, 0},{borofrontx, botbackedgey, 0}, {borofrontx, botbackedgey-width/2, slantheight},{sibackx, botbackedgey-width/2, slantheight}}]
				, Polygon[{{borofrontx, botfrontedgey+width/2, slantheight},{sibackx, botfrontedgey+width/2, slantheight},{sibackx, botfrontedgey+width/2, totalheight}, {borofrontx, botfrontedgey+width/2, totalheight}}]
				, Polygon[{{borofrontx, botbackedgey-width/2, slantheight},{sibackx, botbackedgey-width/2, slantheight}, {sibackx, botbackedgey-width/2, totalheight}, {borofrontx, botbackedgey-width/2, totalheight}}]
	}]
];


(* ::Subsubsection:: *)
(*subtableplot2[speeds_, segmented_]*)


(* ::Text:: *)
(*subtableplot2 is a 3D plot of the surface, with a nozzle and the stage arm*)
(*speeds = list of speeds {speed, other garbage}*)
(*segmented = list of times and heights, segmented by line {time, h}*)
(*OUTPUT: {plot, list of points {x,y,h}}*)
(*NEEDS:nozzleGraphics*)


subtableplot2[speeds_, segmented_]:=Module[{seg, surface, zmin, plot2},
seg = segmented;
If[Length[seg]==9,
Do[
	seg[[i, ;;, 1]] = speeds[[i, 1]]*(seg[[i, ;;, 1]] - seg[[i, 1, 1]]);
	seg[[i]] = {#[[1]],  -5*i, -#[[2]]}&/@seg[[i]];
,{i, Length[segmented]}];
seg = Flatten[seg,1];
];	
plot3dfrompoints[seg]
]


plot3dfrompoints[seg_]:=Module[{surface, zmin, plot2, plot3},
{plot2, surface} = singleplot3dfrompoints[seg];
plot3 = Row[{Column[{plot2
					, Show[plot2, ViewPoint->{-Infinity, 0,0}, AxesLabel->{"", "y (mm)", "Standoff distance (\[Mu]m)"}]}]
			, Show[plot2,ViewPoint->{0, 0,Infinity}, ImageSize->300, AxesLabel->{"x (mm)", "y (mm)", ""}]}];
{plot2, seg, Normal[surface]}
]


singleplot3dfrompoints[seg_]:=Module[{surface, zmin, plot2},
surface = LinearModelFit[seg, {x,y}, {x,y}];
zmin = Min[Flatten[Table[surface[x,y], {x, {0,70}}, {y, {-45, -5}}]]];
plot2 = Show[ListPointPlot3D[seg
			, AxesLabel->{"x (mm)", "", "Standoff distance (\[Mu]m)"}
			, LabelStyle->Directive[14, Black]
			, ImageSize->900
			, PlotStyle->Black
			, ViewPoint->{0, -Infinity, 0}
			]
	,Plot3D[surface[x,y], {x, -10, 80},{y, -50, 0}, PlotStyle->Directive[Opacity[0.5], White]]
	,Graphics3D[Cuboid[{30, -20, zmin},{40, 10, zmin-5}]]
	,nozzleGraphics[35, -20, 0]
	,PlotRange->{{-10, 80},{-55, 10}, {zmin-10, 100}}
	,BoxRatios->{90, 65, 10}
];
{plot2, surface}
]


(* ::Subsubsection:: *)
(*justTheExportPart[video_, stpval_, subtable_, diag_]*)


(* ::Text:: *)
(*exports stats on stage height*)
(*video = file name (string)*)
(*stpval = list of stats {plot, substrate, speeds, points, beginnings, ends}*)
(*subtable = list of substrate values {time, position, h}i*)
(*diag = bool true to print diagnostics (bool)*)
(*OUTPUT = exports and prints, returns null*)
(*NEEDS: fixedStats*)


justTheExportPart[video_, stpval_, subtable_, diag_]:=Module[{plot, substrate, speeds, points,v, noz, be, firstmid, criticalPoints, ppmm, fns, framerate, beginnings, ends},
	If[Length[stpval]!=6, Throw[{stpval, subtable[[;;, 1;;3]]}]];
	{plot, substrate, speeds, points, beginnings, ends} = stpval;
	If[diag, Print[plot]];
	{v, noz, be, firstmid, criticalPoints, ppmm} = fixedStats[video];
		(*import the nozzle geometry*)
	framerate = Import[video, "FrameRate"];
	Export[StringReplace[video, {"gras"->"grasHeights", ".avi"->".tiff"}], plot];
		(*export plot of diagnostics*)
	Export[StringReplace[video, {"gras"->"grasHeights", ".avi"->".csv"}], points];
		(*export list of stand-off distances as a function of stage position*)

	fns = FileNameSplit[video];
	Export[StringReplace[video, {"gras"->"grasGeo", ".avi"->".csv"}],
						 {video
						 , noz
						 , be
						 , firstmid
						 , criticalPoints
						 , ppmm
						 , substrate
						 , beginnings
						 , ends
						 , StringTake[fns[[1]], 2;;]
						 , StringTake[fns[[2]], 2;;]
						 , fns[[3]]
						 , fns[[4]]
						 , StringTake[fns[[5]], 3;;]
						 , speeds
						 , framerate
		}];
		(*export nozzle geometry with substrate tilt added*)
		Print["Exported:\n\t", StringReplace[video, {"gras"->"grasHeights", ".avi"->".tiff"}]
								, "\n\t", StringReplace[video, {"gras"->"grasHeights", ".avi"->".csv"}]
								, "\n\t", StringReplace[video, {"gras"->"grasGeo", ".avi"->".csv"}]
		];
	];


(* ::Subsubsection:: *)
(*predictSpeeds[broken_]*)


(* ::Text:: *)
(*get the list of stage speeds from the split file name*)
(*broken = FileNameSplit[video] (list of strings)*)
(*OUTPUT: list of 9 speeds*)
(*NEEDS:*)


predictSpeeds[broken_]:=Module[{speeds},
speeds = Flatten[ConstantArray[#, 3]&/@Switch[broken[[1]]
				,"T35slow", Switch[broken[[6]]
						,"vSslow",{1.5, 1.7, 1.9}
						,"vSfast", {2.1, 2.3, 2.5}
						,"vSturbo", {2.7, 2.9, 3.1}
						,"vSveryfast", {3.3, 3.5, 3.7}
						,"vSextremefast", {3.9, 4.1, 4.3}]
				,_,Switch[broken[[6]]
						,"vSslow", {1.5, 2.5, 3.5}
						,"vSfast", {4.5, 5.5, 6.5}
						, "vSturbo", {7.5, 8.5, 9.5}
]]];
speeds
]


(* ::Subsubsection:: *)
(*nearestLift[subtable_, intended_, spacing_, diag_]*)


(* ::Text:: *)
(*detects the nearest lift points to an estimated time*)
(*subtable = list {time, position, h}i*)
(*intended = estimated time (double in seconds)*)
(*spacing = spacing between points in original detection (s)*)
(*diag = true to print diagnostics (bool)*)
(*OUTPUT: list of points in lift cluster (time, h)i*)
(*NEEDS: *)


nearestLift[subtable_, intended_, spacing_, diag_]:=Module[{candidateRegion, lifts, lift, max, range},
Catch[
If[intended>Max[subtable[[;;,1]]], Return[{{intended, 1000}}]];
lift = {};
range = 2;
While[Length[lift]<1 && range<10,
	candidateRegion = Select[subtable[[;;, {1,3}]],intended - range<#[[1]]<intended + range&];
	If[Length[candidateRegion]>1,
		(*max = Max[candidateRegion[[;;,2]]];*)
		max = 1000;
		If[max==1000 || max>Mean[candidateRegion[[;;,2]]] +2*StandardDeviation[candidateRegion[[;;,2]]]
			,
			lifts = Select[candidateRegion, #[[2]]==max&];
			If[Length[lifts]>0,
				lifts = Split[lifts, #2[[1]]-#1[[1]]<spacing*2&];
				lift = MaximalBy[lifts, Length];
				lift = MinimalBy[lift, Abs[#[[1,1]] - intended]&][[1]];
				,
				range = range+1;
			];
			,
			range = range + 1;
		];
	,
	range = range+1;
	];
];
(*If[diag, Print[ListPlot[If[Length[lift]>0, {candidateRegion, lift}, candidateRegion]
					, Frame\[Rule]True, FrameStyle\[Rule]Black, LabelStyle\[Rule]Directive[12, Black]
					, FrameLabel\[Rule]{"Time (s)", "h (\[Mu]m)"}, ImageSize\[Rule]300, AspectRatio\[Rule]1/2
					, PlotStyle\[Rule]{Black, Red}, PlotRange\[Rule]All]
		, lift]];*)
lift
]];


(* ::Subsubsection:: *)
(*enhance[subtable_ , intended_, video_, diag_]*)


(* ::Text:: *)
(*do a deeper search on an area*)
(*subtable = list of points {time, position, h}i*)
(*intended = estimated time (double seconds)*)
(*video = file name (string)*)
(*diag = true to print diagnostics (bool)*)
(*OUTPUT: list of points in this lift*)
(*NEEDS: importFrame, isreflect, fixedStats*)


enhance[subtable_ , intended_, video_, diag_]:=Module[{candidateRegion, fr, firstFrame, lastFrame, f, found, noz, lift1, span, originalSpacing},
Catch[
span = 10;
candidateRegion = {};
While[Length[candidateRegion]<1 && span<20,
	candidateRegion = Select[subtable[[;;, {1,3}]],intended -span<#[[1]]<intended +span&];
	span = span+1;
];
fr = Import[video, "FrameRate"];
firstFrame = Round[candidateRegion[[1, 1]]*fr];
lastFrame = Round[candidateRegion[[-1, 1]]*fr];
f = firstFrame;
found = 0;
noz = fixedStats[video][[2]];
originalSpacing = Round[(candidateRegion[[2,1]] - candidateRegion[[1,1]])*fr];
If[diag, Print["Time ", firstFrame/fr, " to ", lastFrame/fr, ". Expected = ", intended]];
lift1 = Reap[While[f<lastFrame && found<2,
	If[!MemberQ[candidateRegion[[;;,1]], f/fr],
		If[!isreflect[EdgeDetect[importFrame[video, f], 3], noz, diag],
			Sow[{f/fr, 0, 1000}];
			found = 1;
			,
			If[found==1, found=2];
		];
	];
	f = f+Round[originalSpacing/2];
	]][[2]];
If[Length[lift1]>0
	,lift1 = lift1[[1]]
	,lift1 = {{intended, 1000}}
];
Return[lift1]
]]


(* ::Subsubsection:: *)
(*beginningRun[subtable_, video_, diag_]*)


(* ::Text:: *)
(*takes in table of substrate heights and analyzes*)
(*subtable = list of substrate values {time, position, h}i*)
(*video = file name (string)*)
(*diag = true to print diagnostics (bool)*)
(*OUTPUT: exports, prints, returns segmented list of points*)
(*NEEDS: predictSpeeds, nearestLift, enhance, subtableplot1, subtableplot1naked, subtableplot2, justTheExportPart*)


beginningRun[subtable_, video_, diag_]:=Module[{broken, speeds,  spacing, thislift, lifts, nl, beginnings, ends, segmented, intended, plot, plot1, plot2, substrate, points},
If[diag, Print[subtableplot1naked[subtable]]];
broken = FileNameSplit[video];
speeds = predictSpeeds[broken];
spacing = subtable[[2,1]] - subtable[[1,1]];
thislift = 3;
lifts = Prepend[Table[
				intended = thislift + 70/speeds[[i]];
				nl = nearestLift[subtable, intended, spacing, diag];
				If[Length[nl]<1,
					(*If[diag, Print[subtableplot1naked[subtable]];];*)
					nl = enhance[subtable, intended, video, diag];
				];
				thislift = nl[[-1,1]];
		(*		If[diag, Print[i, nl]];*)
				nl
		,{i, 9}], {{3, 1000}}];
beginnings = (#[[-1,1]])&/@lifts[[1;;9]];
ends = (#[[1,1]])&/@lifts[[2;;10]];
If[diag, Print[Grid[SetAccuracy[{beginnings, ends},2]]]];
segmented = Table[
				Select[subtable[[;;, {1,3}]], beginnings[[i]]<#[[1]]<ends[[i]] && #[[2]]<1000&]
			, {i, 9}];
segmented = Table[
				Select[segmented[[i]], Abs[#[[2]] - Median[segmented[[i,;;, 2]]]] < 3*StandardDeviation[segmented[[i,;;,2]]]&]
			, {i, 9}];
speeds = Table[
				{speeds[[i]], {Mean[{beginnings[[i]], ends[[i]]}], 20}}
			, {i,9}];
plot1 = subtableplot1[subtable, segmented, ToExpression[StringTake[broken[[2]], 2;;]], speeds, beginnings, ends];
{plot2, points, substrate} = subtableplot2[speeds, segmented];
plot = Column[{plot1, plot2}];
justTheExportPart[video, {plot, substrate, speeds, points, beginnings, ends}, subtable, diag];
segmented
]


(* ::Subsubsection:: *)
(*exportSubTableStats[video_, subtable_, noz_, be_, firstmid_, criticalPoints_, ppmm_, framerate_, diag_]*)


(* ::Text:: *)
(*starting with list of substrate values, analyze and export*)
(*video = file name (string)*)
(*subtable = list of {time, position, h}*)
(*noz = nozzle lines*)
(*be = bottom edge line*)
(*firstmid = suggested midpoint FROM THE BOTTOM (int px)*)
(*criticalPoints = list of points*)
(*ppmm = scale (double)*)
(*framerate = frame rate (double)*)
(*diag = bool true to print diagnostics (bool)*)
(*OUTPUT: exports files, prints file names, returns given table and analysis values*)
(*NEEDS: beginningRun, justTheExportPart*)


exportSubTableStats[video_, subtable_, diag_]:=Module[{stpval, plot, substrate, speeds, points, fns},
Catch[
	stpval = beginningRun[subtable[[;;,1;;3]], video, diag];
		(*calibrate substrate tilt*)
	Export[StringReplace[video, {"gras"->"grasSubs", ".avi"->".csv"}], subtable[[;;, 1;;3]]];
		(*export list of stand-off distances as a function of time*)
	Print["Exported ", StringReplace[video, {"gras"->"grasSubs", ".avi"->".csv"}]];
	Return[{stpval, subtable}];
	]];


(* ::Subsection:: *)
(*series of frames*)


(* ::Subsubsection:: *)
(*mirrorFrameSeries[video_, frames_, export_, diag_]*)


(* ::Text:: *)
(*mirrorFrameSeries finds standoff distances as a function of time*)
(*video = file name*)
(*frames = list of frame numbers (list of ints)*)
(*export = true to analyze and export table (bool)*)
(*diag = true to print diagnostics (bool)*)
(*OUTPUT: table of substrate locations and standoff distances*)
(*NEEDS: adjustFrams, fixedStats, mirrorDiagnostic, importFrame*)


mirrorFrameSeries[video_, frames_, export_, reinitialize_, diag_]:=Module[{frams, noz, be, firstmid, criticalPoints, ppmm, v, subtable, framerate, intendedSD, plot, substrate, speeds, points, fns, stpval, count, st, diag1, suggest, report},
	frams = adjustFrams[video, frames];
		(*get a list of frame numbers*)
	Print[video, "\t", MinMax@frams];
	framerate = Import[video, "FrameRate"];
	{v, noz, be, firstmid, criticalPoints, ppmm} = If[reinitialize 
		,initializeNozzle[video, frams, diag, diag, False]
		,fixedStats[video]
	];
		(*import the nozzle geometry*)
	count = Round[Length[frams]/10];
	suggest = If[frams[[1]]<100, {0,Min[criticalPoints[[;;,2]]],0}, {0, firstmid, 0}];
	subtable = Table[
		If[count==0 || diag, Print[f]; count = Round[Length[frams]/10];, count = count-1];
			(*print out the frame number 10 times in the run*)
		diag1 = f==frams[[Min[5, Length[frams]]]] || diag;
			(*print out the fit if we're on the 5th requested frame*)
		report = Prepend[mirrorPlane[importFrame[video, f],noz, If[suggest[[2]]>0, suggest[[2]], firstmid],be,ppmm, criticalPoints, diag1], f/framerate];
			(*here we use the previous frame as a suggestion for this midplane in this frame.*)
		If[Abs[report[[2]] - firstmid]>15 && f>300, suggest = {0, firstmid, 0}, suggest = report];			
			(* If that fit was far from the global suggestion or before movement began we keep the data but don't use it as a suggestion next time, instead using the global suggestion*)
		report
			(*return the fit to the table*)
	,{f, frams}];
	If[export, 
		{stpval, st} = exportSubTableStats[video, subtable, diag];
		If[Length[stpval]!=6,
			Print["Fit failed"];
		];
	];
	subtable[[;;,1;;3]]
]


(* ::Subsubsection:: *)
(*mirrorSeries[video_, dense_, diag_]*)


(* ::Text:: *)
(*collects a series of mirror planes*)
(*video = file name (string)*)
(*dense = true to collect many frames and analyze and export them to calibrate the substrate tilt,*)
(*	false to collect a few frames and print them out to check that midplane detection is working (bool)*)
(*OUTPUT: table of planes*)
(*NEEDS: mirrorFrameSeries, fixedStats, mirrorPlane, importFrame*)


mirrorSeries[video_, dense_, diag_]:=Module[{count, fr, subtable, noz, be, firstmid, criticalPoints, ppmm, v},
count = Import[video, "FrameCount"];
fr =  Import[video, "FrameRate"];
If[dense
	, (*run a lot of frames*)
	subtable = mirrorFrameSeries[video, Range[1, count, Min[Round[count/450], 20]], True, False, diag];
	Export[StringReplace[video, {"gras"->"grasSubsRaw", "avi"->"csv"}], subtable[[;;, 1;;3]]];
	Print["\t", StringReplace[video, {"gras"->"grasSubsRaw", "avi"->"csv"}]];
	, (*run a few frames*)
	{v, noz, be, firstmid, criticalPoints, ppmm} = fixedStats[video];
		(*pull out previously gathered video geometries, or if you haven't initialized it, initialize it*)
	 subtable = (Print[#]; mirrorPlane[importFrame[video,#], noz, firstmid, be, ppmm, criticalPoints, diag])&/@Range[10, count, 1000];
];
subtable	
]


(* ::Subsubsection:: *)
(*howBadIsTheTilt[video_]*)


(* ::Text:: *)
(*analyze planar fit to determine tilt on stage*)
(*video = file name (string)*)
(*OUTPUT: {video, {{xslope, yslope} in um/mm, {xchange, ychange} over the series of the print, {xangle, yangle}}}*)
(*NEEDS: grasGeoFile*)


howBadIsTheTilt[video_]:=Module[{equation, xslope, yslope, xchange, ychange, xangle, yangle},
equation = ToExpression[grasGeoFile[video][[7,1]]];
xslope = D[equation, x];
yslope = D[equation, y];
xchange = xslope*70;
ychange = yslope*40;
xangle = ArcTan[xslope*10^-3]/Degree;
yangle = ArcTan[yslope*10^-3]/Degree;
{video, {{xslope, yslope}, {xchange, ychange}, {xangle, yangle}}}]


(* ::Subsubsection:: *)
(*middleDistance[video_]*)


(* ::Text:: *)
(*standoff distance, measured at the corner of the slide where the height was initialized*)
(*video = file name (string)*)
(*OUTPUT: {time, intended h, actual h, difference}*)
(*NEEDS: grasGeoFile, extractTimeStamp*)


middleDistance[video_]:=Module[{actual, intended},
intended = ToExpression[StringTake[FileNameSplit[video][[2]], 2;;]];
actual = -ToExpression[grasGeoFile[video][[7,1]]]/.{x->0, y->0};
{extractTimeStamp[video], intended, actual, actual-intended}]
