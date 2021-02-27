(* ::Package:: *)

(* ::Title:: *)
(*Functions for measuring extrusion behaviors*)


dir = NotebookDirectory[];
file = FileNameJoin[{dir, "fileManagement.wl"}];
If[FileExistsQ[file], Import[file]];
Clear[dir, file];


(* ::Section:: *)
(*color management*)


getColors[n_]:=Module[{f, ni}, Catch[
If[Length[n]>1
	, 
	ni = n[[1]];
	If[OddQ[ni]
		, f = n[[2]][#, 40]&;
		, f = n[[2]][#, 20]&;
	];
	, 
	f = COLORrainbow[#, 0]&;
	ni = n;
	If[ni==4, Throw[RGBColor[#/256]&/@{{190,40,60}, {203,154,97}, {102,181,120},{32,97,136}}]] 
];
	If[ni>2
		, f/@(Range[0,ni-1]/(ni-1))
		, If[ni==2
			, {Black, (*Darker[Red, 0.2]*)Gray}
			, Black
		]
	]
	]];


COLORtealorange[f_, cg_]:=Module[{teal, orange, gray},
teal = {30, 163, 201};
orange = {220, 90, 58};
gray = COLORgray[cg];
RGBColor[If[f<=0.5, (f*2*gray+(1-f*2)*teal)/256, (2*(f-0.5)*orange+(1-2*(f-0.5))*gray)/256]]
]


COLORrainbow[f_, cg_]:=ColorData["DarkRainbow"][f]


COLORredblue[f_, cg_]:=Module[{red, blue, gray},
red = {202,0,32};
blue = {5,113,176};
gray = COLORgray[cg];
RGBColor[If[f<=0.5, (f*2*gray+(1-f*2)*blue)/256, (2*(f-0.5)*red+(1-2*(f-0.5))*gray)/256]]
]


COLORgray[s_]:=ConstantArray[256-s,3];


COLORredblack[f_, cg_]:=Module[{red, blue, gray},
red = {165, 18, 53};
blue = {0,0,0};
gray = COLORgray[cg];
RGBColor[If[f<=0.5, (f*2*gray+(1-f*2)*blue)/256, (2*(f-0.5)*red+(1-2*(f-0.5))*gray)/256]]
]


(* ::Section:: *)
(*Constants*)


(*nozzle detection*)
EXPECTEDNOZWIDTH = 225; 
	(*expected width of nozzle in pixels*)
EXPECTEDNOZVARIATION = 5; 
	(*allowable variation in nozzle width, in pixels*)
NOZFRAMES = 2; 
	(*number of frames to use for nozzle detection*)

(*substrate detection*)
EXPECTEDSUBSTRATEFRACTION = 0.6; 
	(*distance from the top of the frame, as a ratio*)
ALLOWABLEREFLECTIONRADIUS = 5; 
	(*pixels from the nozzle edges allowable for inclusion as a nozzle edge*)
CRITMIRRORPOINTS = 100; 
	(*critical number of points in common between image and reflection for a mirror plane to be considered accurate*)
BASELINERATIO = 0.2; 
	(*fraction of image height to use on top and bottom for baseline intensity measurement*)
SEARCHRADIUS = 6;
	(*number of points to use on each search step of mirror plane detection*)
SUBSTRATEFRAMES = 3;
CRITREFLECTIONPOINTS = 10;
	
(*bottom detection*)
BOTFRAMES = 3;

(*real dimensions*)
NOZZLEWIDTH = 1.025;
	(*actual nozzle width in mm*)
NOZEXITF = (0.525 - 0.15/2)/(0.525 + 0.5);
	(*middle of nozzle, expressed as a fraction of the nozzle depth, measured from the bottom*)
	
INFINITERADIUS = 10^20;


ERRORCURVEPOINT =<|"x"->"", "y"-> "", "r"-> "", "dn"-> "", "ds"-> "", "dSi"-> ""|>;


Needs["ErrorBarPlots`"] (*need to run this line to make plots with error bars*)


SetOptions[ErrorListPlot, Frame->True, FrameStyle->Black, LabelStyle->Directive[12, Black], Axes->None, AspectRatio->1];
SetOptions[ListPlot, Frame->True, FrameStyle->Black, LabelStyle->Directive[12, Black], Axes->None, AspectRatio->1];
SetOptions[ListLinePlot, Frame->True, FrameStyle->Black, LabelStyle->Directive[12, Black], Axes->None, AspectRatio->1];
SetOptions[Plot, Frame->True, FrameStyle->Black, LabelStyle->Directive[12, Black], Axes->None, AspectRatio->1];


totalrange[list_]:=Module[{d}, d = MinMax[list]; d[[2]] - d[[1]]];


(* ::Subsubsection:: *)
(*SMHEADER2*)


SMHEADER2 = Join[
			{"frame #", "time (s)", "h (mm)"}
			, Flatten[Table[i<>" "<>j<>" "<>k<>" dist (mm)", {i, {"up", "down"}}, {j, {"right", "left", "mid"}}, {k, {"noz", "sub", "Si"}}]]
			, Flatten[Table[i<>" "<>j<>" "<>k, {i, {"up", "down"}}, {j, {"right", "left", "mid"}}, {k, {"outer radius (mm)", "inner radius (mm)", "Laplace pressure (Pa)"}}]]									
			, Flatten[Table[i<>" "<>j[[1]]<>" to "<>j[[2]]<>" pressure (Pa)", {i, {"up", "down"}}, {j,  Subsets[{"right", "left", "mid"}, {2}]}]]
			, {"SL contact angle (deg)", "BL contact angle (deg)", "SiL contact angle (deg)"}
			, {"SL contact point channel dist (mm)", "SiL contact point nozzle dist (mm)", "Laplace pressure upstream to downstream"}
		];


(* ::Section:: *)
(*measuring distances to reference geometries and getting lists of points*)


(* ::Subsection:: *)
(*distances*)


(* ::Subsubsection:: *)
(*pldist[x_, y_, m_, b_]*)


(* ::Text:: *)
(*pldist is the distance from a point to a line*)
(*(x,y) = point*)
(*y = mx+b = line*)
(*OUTPUT: distance in px*)
(*NEEDS:*)


pldist[x_, y_, m_, b_]:=Abs[b + m*x - y]/Sqrt[1+m^2];


(* ::Subsubsection:: *)
(*dpbe[x_, y_, be_]*)


(* ::Text:: *)
(*distance between point and bottom edge*)
(*{x,y} = point*)
(*be = bottom edge*)
(*OUTPUT: distance in px*)
(*NEEDS: pldist*)


dpbe[x_, y_, be_]:=Module[{m, b},
m = be[[2]];
b = be[[1,2]] - m*be[[1,1]];
pldist[x,y,m,b]
]


(* ::Subsection:: *)
(*selecting points relative to nozzle*)


(* ::Subsubsection:: *)
(*leftofnozzle[pts_, noz_, range_]*)


(* ::Text:: *)
(*list of points left of the nozzle*)
(*pts = list of points (x,y)*)
(*noz = left and right nozzle edges {{x,y}, {dx/dy}}*)
(*range = pixel range within which to exclude points*)
(*OUTPUT: list of points*)
(*NEEDS:*)


leftofnozzle[pts_, noz_, range_]:=Module[{lnoz},
lnoz = noz[[1]];
Select[pts, #[[1]]< lnoz[[1,1]] - lnoz[[2]]*(lnoz[[1,2]] - #[[2]])-range&]
]


(* ::Subsubsection:: *)
(*belownozzle[pts_, be_, range_, midplane_]*)


(* ::Text:: *)
(*belownozzle selects points between the nozzle and the point halfway between the midplane and its reflection*)
(*pts = list of points (x,y)*)
(*be = bottom edge {{x,y}, dy/dx}*)
(*midplane = location of midplane FROM THE BOTTOM (px)*)
(*OUTPUT: list of points (x,y)*)
(*NEEDS:*)


belownozzle[pts_, be_, midplane_]:=Select[pts, midplane - 0.5*(be[[1,2]] + be[[2]]*(#[[1]] - be[[1,1]]) - midplane) < #[[2]]<be[[1,2]] + be[[2]]*(#[[1]] - be[[1,1]])&]


(* ::Subsection:: *)
(*selecting points relative to frame*)


(* ::Subsubsection:: *)
(*topedgelist[list_]*)


(* ::Text:: *)
(*points on the top edge of the list*)
(*list: list of xy points*)
(*OUTPUT: list of xy points*)
(*NEEDS:*)


topedgelist[list_]:=MaximalBy[#, #[[2]]&][[1]]&/@GatherBy[list, #[[1]]&]


(* ::Subsubsection:: *)
(*leftedgelist[list_]*)


(* ::Text:: *)
(*points on the left edge of the list*)
(*list: list of xy points*)
(*OUTPUT: list of xy points*)
(*NEEDS:*)


leftedgelist[list_]:=MinimalBy[#, #[[1]]&][[1]]&/@GatherBy[list, #[[2]]&]


(* ::Section:: *)
(*getting points*)


(* ::Subsection:: *)
(*downstream points*)


(* ::Subsubsection:: *)
(*dspdiagplot[dsp_, lon_]*)


(* ::Text:: *)
(*diagnostic plot for downstream point collection*)
(*dsp = list of collected points*)
(*lon = list of original points*)
(*OUTPUT: plot*)
(*NEEDS: *)


dspdiagplot[dsp_, lon_]:=Module[{pr, pts}, 
pr = (({-10, 10}+MinMax[#])&/@Transpose[lon]); 
(*ListPlot[dsp, ImageSize->300, AspectRatio->(pr[[2]]/pr[[1]]), PlotRange->pr, PlotStyle->Black, Frame->None]*)
pts = dsp;
pts[[;;,1]] = pts[[;;,1]] - pr[[1,1]];
pts[[;;,2]] = pr[[2,2]] - pts[[;;,2]];
pts = Reverse/@pts;
Show[Image[SparseArray[(#->0)&/@pts,{pr[[2,2]] - pr[[2,1]], pr[[1,2]] - pr[[1,1]]},1]], ImageSize->300]
]


(* ::Subsubsection:: *)
(*shortDownstream[stats_, lon_, mp_, pts_, superdiag_]*)


(* ::Text:: *)
(*gets a list of points if the leftmost point hits the substrate or the x-span is short*)
(*stats = assoc*)
(*lon = list of points left of nozzle*)
(*mp = midplane in px FROM THE BOTTOM*)
(*pts = original list of all edge points*)
(*superdiag = bool true to print*)
(*OUTPUT: list of points*)
(*NEEDS: leftedgelist, topedgelist, dspdiagplot*)


shortDownstream[stats_, lon_, mp_, pts_, superdiag_]:=Module[{nozbot, ma, mi, dsp, final},
(*if the leftmost point hits the substrate or the x-span is short*)
	nozbot = stats["criticalPoints"][[1,2]];
	If[Min[lon[[;;,2]]]> mp + 10 ||
		 Length[Select[DeleteDuplicates[lon[[;;,2]]], #<nozbot&]] < 0.8 * (nozbot - mp)
		, 
		ma = Max[lon[[;;,1]]];
		mi = Min[MaximalBy[Split[SortBy[lon, #[[2]]&], Abs[#1[[2]] - #2[[2]]]<5&], Length][[1,;;,2]]];
		dsp = Join[lon, Select[pts, ma<#[[1]] && mp<#[[2]]<mi&]];
			(*if the points don't reach the substrate, add more points between here and the substrate, to the right
			this is for extreme dropletting behaviors where the filament retracts all the way to the nozzle*)
		, 
		dsp = lon;
	];
	dsp = Select[dsp, #[[2]]>=mp&];
		(*points above the substrate*)
	final = DeleteDuplicates[Sort[Join[leftedgelist[dsp], Select[topedgelist[dsp], #[[1]]<MaximalBy[dsp, #[[2]]&][[1, 1]]&]]]];
		(*left and top edges, but only include top edges for points to the left of the contact point*)
	final = MinimalBy[Split[SortBy[final, #[[2]]&], Abs[#1[[2]] - #2[[2]]]<10&], #[[1,2]]&][[1]];
		(*remove any stray y-clusters up on the nozzle*)
	If[superdiag, 
		Print[Grid[{{"left of nozzle", "expanded", "edges"}
					,{dspdiagplot[lon, pts], dspdiagplot[dsp, pts], dspdiagplot[final, pts]}}]]
	];
	If[Max[final[[;;,2]]]<stats["criticalPoints"][[1,2]] ||
		 Max[final[[;;,1]]]>stats["criticalPoints"][[1,1]]+(stats["criticalPoints"][[1,2]] - mp)
		 , final = 1
		 (*if the topmost point is below the nozzle or the rightmost point
		  is far in front of the back edge, throw error*)
	];
	Throw[final]

]


(* ::Subsubsection:: *)
(*downstreampoints[pts_, noz_, range_]*)


(* ::Text:: *)
(*downstreampoints finds points on the downstream edge of the nozzle*)
(*pts = list of points (x,y)*)
(*noz = left and right nozzle edges {{x,y}, {dx/dy}}*)
(*range = pixel range within which to exclude points*)
(*OUTPUT: list of points (x,y)*)
(*NEEDS: leftofnozzle, totalrange, shortDownstream, dspdiagplot, pldist*)


downstreampoints[pts_, stats_, range_, mp_, superdiag_]:=Module[{lon, dsp, i, lastpoint, extrapoints, critspacing, line, dspnear, print1, print2, print3, print4, rightmost, print5, drl, splitos, noz, toadd, ma, mi, final, nozbot, SD},
Catch[
SD = True; (*False to suppress output*)
noz = stats["noz"];
lon = leftofnozzle[pts, noz, range];
	(*all points left of the nozzle*)
lon = Select[lon, #[[2]]>mp-10&];
	(*points left of the nozzle and above the midplane, with a little wiggle*)
lon = MaximalBy[Split[Sort[lon], Abs[#1[[1]] - #2[[1]]]<15&], #[[1,1]]&][[1]];
	(*rightmost x-cluster of points*)
If[MinimalBy[lon, #[[1]]&][[1,2]]<mp+10 && totalrange[lon[[;;,1]]] < 0.5*totalrange[pts[[;;,1]]],
	shortDownstream[stats, lon, mp, pts, superdiag];
];
If[superdiag, print1 = {"left of nozzle ", dspdiagplot[lon, lon]}];

dsp = topedgelist[lon];
	(*top edge*)
i = 1;
critspacing = 10;
If[superdiag, print2 = {"top edge ", dspdiagplot[dsp, lon]}; DSP1 = dsp;];
splitos = Select[Split[dsp, EuclideanDistance[#1, #2]<critspacing &], Length[#]>10&];
	(*group points using gaps*)
If[Length[splitos]<1, Throw[1]];
dsp = MaximalBy[splitos, #[[1,1]]&][[1]];
	(*take the rightmost group*)
toadd = Select[splitos, EuclideanDistance[#[[-1]], dsp[[1]]]<critspacing&];
While[Length[toadd]>0,
	dsp = Join[toadd[[1]], dsp];
	toadd = Select[Complement[splitos, toadd[[1]]], EuclideanDistance[#[[-1]], dsp[[1]]]<critspacing&];
];
	(*add other nearby groups to the left*)
If[superdiag,
	print4 = {"longest stretch  ", dspdiagplot[dsp, lon]};
	If[Abs[Length[dsp] - Length[DSP1]]>10, SD = True;];
	DSP1 = dsp;
];
rightmost = MaximalBy[dsp, #[[1]]&][[1]];
	(*rightmost point*)
drl = pldist[rightmost[[1]],rightmost[[2]], -1/noz[[1,2]], noz[[1,1,2]]+1/noz[[1,2]]*noz[[1,1,1]]];
	(*distance between rightmost point and nozzle*)
dsp = Join[dsp, Select[
						Select[lon
						, #[[1]]>rightmost[[1]] && #[[2]]>rightmost[[2]]&]
				, EuclideanDistance[#, rightmost]<drl*2&]
	];
	(*add points between the rightmostpoint and the nozzle*)
dsp = DeleteDuplicates[Join[topedgelist[dsp], leftedgelist[dsp]]];
	(*just take the top and left edge*)
dsp = MaximalBy[Split[Sort[dsp], EuclideanDistance[#1, #2]<10&], Length][[1]];
	(*select the largest continuous segment*)
rightmost = MaximalBy[dsp, #[[1]]&][[1]];
drl = pldist[rightmost[[1]],rightmost[[2]], -1/noz[[1,2]], noz[[1,1,2]]+1/noz[[1,2]]*noz[[1,1,1]]];
If[Abs[Length[dsp] - Length[DSP1]]>10, SD = True;];
If[superdiag && SD,
	print5 = {"grown  ", dspdiagplot[dsp, lon]};
	Print[Grid[Transpose[{print1, print2, (*print3, *)print4, print5, {"distance", drl}}]]];
];
If[drl>8 || Max[dsp[[;;,2]]]<stats["criticalPoints"][[1,2]], dsp = 1];
	(*if the points don't reach the nozzle or the contact point is below the nozzle, throw an error*)
dsp]]


(* ::Subsection:: *)
(*upstream points*)


(* ::Subsubsection:: *)
(*upstreampoints[pts_, be_, midplane_, printeverything_]*)


(* ::Text:: *)
(*upstreampoints selects a list of points on the upstream edge*)
(*pts = list of points (x,y)*)
(*be = bottom edge {{x,y}, dy/dx}*)
(*midplane = location of midplane FROM THE BOTTOM (px)*)
(*OUTPUT: list of points*)
(*NEEDS: belownozzle, dspdiagplot*)


upstreampoints[pts_, stats_, midplane_, printeverything_]:=Module[{errorRet, pts1, pts2, leftmost, critspacing, i, be
																		, bottomloc, print1, print2, print3, print4, print5, print6
																		, split, nozbottom, itspans,f, substratepoint
																		, rightmost, frontpoint, pts2sort, count, minx, xvals, SD},
Catch[
SD = True; (*false to suppress output*)
errorRet = 1;
be = stats["be"];
pts1 = belownozzle[pts, be, midplane];
	(*points between the nozzle and the midplane*)
pts2 = pts1;
	If[printeverything, print1 = {"below nozzle ", dspdiagplot[pts2, pts1]}];
pts2 = MaximalBy[#, #[[1]]][[1]]&/@GatherBy[pts1, #[[2]]&];
	(*rightmost points*)
	If[printeverything, print2 = {"rightmost ", dspdiagplot[pts2, pts1]}];
	print2 = {"", ""};
pts2sort = Sort[pts2];(*sort by x*)
leftmost = pts2sort[[1]]; (*leftmost point*)
count = 2;
While[count<10 && (Abs[leftmost[[2]] - midplane]>10 || leftmost[[2]]==Min[pts2sort[[;;,2]]] || leftmost[[2]]==Max[pts2sort[[;;,2]]]),
	(*while the leftmost point is the lowest or highest point or it is well above the substrate, drop the leftmost point*)
	pts2sort = pts2sort[[2;;]];
	leftmost = pts2sort[[1]];
	count = count+1;
]; 
	If[count>=10,
		(*if the leftmost point is well above the substrate or if it's the lowest point, throw an error*)
		If[printeverything, Print[Grid[Transpose[{print1, print2}]]]]; 
		Throw[errorRet];
	];
pts2 = Select[pts1, #[[1]]>leftmost[[1]] && #[[2]]>leftmost[[2]]&];
	(*all points to the right and above the leftmost point*)
	If[printeverything, print3 = {"above left edge ", dspdiagplot[pts2, pts1]}];
pts2 = GatherBy[pts2, #[[1]]&];
pts2 = (MinimalBy[#, #[[2]]&][[1]])&/@pts2;
	(*lowest point on each x*)
	If[Length[pts2]<10, 
		If[printeverything, Print[Grid[Transpose[{print1, print2, print3}]]]]; 
		Throw[errorRet]
	];
	If[printeverything, print4 = {"lowest per x ", dspdiagplot[pts2, pts1]}];
pts2 = Sort[pts2];
	(*sort by x*)
split = Select[Split[pts2, EuclideanDistance[#1, #2]<10&], Length[#]>10&];
	(*longest continuous segments*)
nozbottom = stats["criticalPoints"][[3,2]];
itspans = Select[split, Min[#[[;;,2]]] - midplane < 10 && nozbottom - Max[#[[;;,2]]] < 10&];
	(*segments that span the whole gap*)
If[Length[itspans]>0
	, pts2 = MaximalBy[itspans, #[[1,1]]&][[1]]
		(*if there is a segment that spans the whole gap, keep it*)
	, pts2 = Flatten[split,1]
		(*otherwise, keep all the segments*)
];
substratepoint = MinimalBy[pts2, #[[2]]&][[1]];
pts2 = Select[pts2, #[[1]]>=substratepoint[[1]]&];
	(*take the contact point to be the lowest point, and only take points to the right of it*)
rightmost = MaximalBy[pts2, #[[1]]&][[1]];
frontpoint = stats["criticalPoints"][[2]];
leftmost = MinimalBy[pts2, #[[1]]&][[1]];
pts2 = DeleteDuplicates[Sort[Join[pts2 
					,Select[pts1, rightmost[[1]]<=#[[1]]<=frontpoint[[1]] && rightmost[[2]]<=#[[2]]<=frontpoint[[2]]&] (*points at the nozzle surface*)
					,Select[Table[expansionPoint[pts1, z, leftmost[[1]] - (leftmost[[2]]-midplane), leftmost[[1]], leftmost]
								, {z, Round[midplane], leftmost[[2]]}]
					, Length[#]>0&] (*points at the midplane*)
			]]];
	(*expand pts2 to include points between existing points and the substrate and the front nozzle point*)
	If[printeverything && SD
		, print5 = {"expanded", dspdiagplot[pts2, pts1]};
		 Print[Grid[Transpose[{print1, print2, print3, print4, print5}]]]
	];
(*xvals = Sort[DeleteDuplicates[pts2[[;;,1]]]];
minx = Position[Differences[xvals], z_/;z>5][[;;,1]];
If[Length[minx]>0 && minx<0.2*Length[xvals]
	, pts2 = Select[pts2, #[[1]]>xvals[[Max[minx]]]&]
];
	(*eliminate large jumps in x*)

pts2 = Select[pts2, #[[2]]>=midplane&];

If[printeverything, 
If[Abs[Length[pts2]-expandedlength]>5, 
PRINTIT = True;
	print6 = {"cleaned ", dspdiagplot[pts2, pts1]};
	Print[Grid[Transpose[{print1, print2, print3, print4, print5, print6}]]];
	];
];*)
If[Min[pts2[[;;,2]]]>midplane+10 || Mean[pts2[[;;10,2]]]>midplane+20, pts2 = {};];
		(*if the bottom doesn't reach the substrate, try again*)
If[Max[pts2[[;;,2]]]<nozbottom - 10 || Mean[pts2[[-10;;,2]]]<nozbottom-20, pts2 = {};];
		(*if the top doesn't reach the nozzle, try again*)
pts2
]]



expansionPoint[pts1_, z_, left_, right_, point_]:=Module[{p},
p = Select[pts1, #[[2]]==z && left<=#[[1]]<=right&];
If[Length[p]>0,
	If[Length[p]>1,
		MinimalBy[p, EuclideanDistance[#, point]&][[1]]
		,
		p[[1]]
	]
	,
	{}
]]


(* ::Subsection:: *)
(*getting midplane*)


(* ::Subsubsection:: *)
(*ptslope2ptpt[line_, height_]*)


(* ::Text:: *)
(*ptslope2ptpt converts a point and slope to a point and point*)
(*line = {{x,y}, dy/dx}*)
(*height = height of image (int)*)
(*OUTPUT: {{x1,y1},{x2,y2}}*)
(*NEEDS: *)


ptslope2ptpt[line_, height_]:={line[[1]], {line[[1,1]] - line[[2]]*height, 0}}


xptslope2ptpt[line_, width_]:={line[[1]], {width, line[[1,2]] + line[[2]]*width}}


(* ::Subsubsection:: *)
(*drawNozzleOnImage[image_, noz_, firstmid_, be_, ppmm_, criticalPoints_]*)


(* ::Text:: *)
(*ptslope2ptpt converts a point and slope to a point and point*)
(*image = any image to be annotated (image)*)
(*noz = two nozzle edges {{x,y},dx/dy}*)
(*firstmid = mid position MEASURED FROM BOTTOM (int)*)
(*be = bot edge line {{x,y}, {dy/dx}}*)
(*ppmm = pixels per mm (int)*)
(*criticalPoints = list of {x,y} points on bottom of nozzle {left, right, mid}*)
(*OUTPUT: annotated image (graphics object)*)
(*NEEDS: ptslope2ptpt, xptslope2ptpt*)


drawNozzleOnImage[image_, noz_, firstmid_, be_, ppmm_, criticalPoints_]:=Module[{id, width, height, nozzle, midline, cps, edges},
id = ImageDimensions[image];
width = id[[1]];
height = id[[2]];
nozzle = If[Length[noz]==2 && Length[noz[[1]]]==2 && Length[noz[[2]]]==2
			, If[Length[criticalPoints]==3
				, {Line[{noz[[1,1]], criticalPoints[[1]]}], Line[{noz[[2,1]], criticalPoints[[2]]}], Line[{criticalPoints[[1]], criticalPoints[[2]]}]}
				, edges = {Line[ptslope2ptpt[noz[[1]], height]], Line[ptslope2ptpt[noz[[2]], height]]};
				If[Length[be]>0, {edges, Line[xptslope2ptpt[be, width]]}, edges]
			]
			, Graphics[]
		];
midline = If[firstmid>0
			, Line[{{0, firstmid}, {width, firstmid}}]
			, Graphics[]
		];
cps = If[Length[criticalPoints]>0
		, If[Length[#]==2
				, Point[#]
				, Graphics[]
			]&/@criticalPoints[[{3}]]
		, Graphics[]
	];
Show[HighlightImage[image
						, {White, nozzle
							, midline
							(*, Red
							, Style[Text[ToString[ppmm]<>" px/mm", Scaled[{0.1, 0.9}]], Medium]*)
							, White
							, PointSize[0.01]
							, cps
						}]
		  , ImageSize->600]
]


dnoi[video_, frame_]:=Module[{stats, fr, mp},
stats = grasGeoAssoc[video]; 
fr = importFrame[video, frame]; 
mp = framenum2midplane[video, frame, stats];
drawNozzleOnImage[fr, stats["noz"], mp, stats["be"], stats["ppmm"], stats["criticalPoints"]]
]


(* ::Subsubsection:: *)
(*framenum2h[video_, frame_]*)


(* ::Text:: *)
(*converts a frame number to a standoff distance in micron*)
(*video = file name (string) or list of parameters {substrate, beginnings, speeds, fr, ends}*)
(*frame = frame number (int)*)
(*OUTPUT = {time (s), x (mm), y (mm), h (um)}*)
(*NEEDS: grasGeoAssoc*)


framenum2h[video_, frame_]:=Module[{plane, beginnings, speeds, fr, time, line, yy, xx, h, ends},
Catch[
If[Length[video]==0 && FileExistsQ[video]
	,{plane, beginnings, speeds, fr, ends} = grasGeoAssoc[video]/@{"substrate", "beginnings", "speeds", "fr", "ends"};
	,If[Length[video]==5
		,{plane, beginnings, speeds, fr, ends} = video
		,Return[{"2nd input must be video name or {plane, beginnings, speeds, fr, ends}", 0,0,0}]
	];
];
time = frame/fr;
If[time>ends[[-1]], Return[{time, 0,0,0}]];
line = FirstPosition[beginnings, n_/;n>time, {10}][[1]]-1;
If[line>0,
	yy = line*-5;
	xx = speeds[[line]]*(time - beginnings[[line]]);
	h =  -(plane/.{x->xx, y->yy});
	{time, xx, yy, h}
	,
	{time, 0,0,0}
]
]]


(* ::Subsubsection:: *)
(*framenum2midplane[video_, frame_, stats_]*)


(* ::Text:: *)
(*gets the midplane location for a frame*)
(*video = file name (string)*)
(*frame = frame number (int)*)
(*statsinput = grasGeoAssoc (association) or literally anything else*)
(*OUTPUT = position in px from the bottom*)
(*NEEDS: framenum2h, grasGeoAssoc*)


framenum2midplane[video_, frame_, statsinput_]:=Module[{stats, nozheight, ppmm},
If[AssociationQ[statsinput]
	,stats = statsinput;
	,stats = grasGeoAssoc[video];
];
nozheight = stats["criticalPoints"][[3,2]];
ppmm = stats["ppmm"];
nozheight - framenum2h[stats/@{"substrate", "beginnings", "speeds", "fr", "ends"}, frame][[4]]*0.001*ppmm
]


(* ::Subsubsection:: *)
(*plotSubstrate[video_, frame_, stats_]*)


(* ::Text:: *)
(*plot substrate on image*)
(*video = file name (string)*)
(*frame = frame number (int)*)
(*stats = grasGeoAssociation (association) or literally anything else*)
(*OUTPUT: plot*)
(*NEEDS: grasGeoAssoc, framenum2midplane, drawNozzleOnImage*)


plotSubstrate[video_, frame_, statsinput_]:=Module[{stats, mp, im, width},
If[!AssociationQ[statsinput]
	,stats = grasGeoAssoc[video];
	,stats = statsinput
];
mp = framenum2midplane[video, frame, stats];
im = importFrame[video, frame];
width = ImageDimensions[im][[1]];
drawNozzleOnImage[im, stats["noz"], mp, stats["be"], stats["ppmm"], stats["criticalPoints"]]
]


(* ::Subsection:: *)
(*points together*)


(* ::Subsubsection:: *)
(*streampointsPlot*)


streampointsPlot[pts_, upstreampts_, downstreampts_, origframe_, stats_, mp_]:=Module[{id, plot1, showthepoints, pts2plot, ptstyles, markers},
showthepoints = 0; (*0 for no points, 1 for surfaces, 2 for surfaces and edge points*)
Switch[showthepoints
	, 0, 
		pts2plot = {0,0}; ptstyles = White; markers = {\[FilledCircle], 2};
	, 1, 
		If[Length[upstreampts]>0
			, If[Length[downstreampts]>0
				, pts2plot = {upstreampts, downstreampts}; ptstyles = {White, White}; markers = {{\[FilledCircle], 4},{\[FilledCircle], 4}}
				, pts2plot = upstreampts; ptstyles = White; markers = {\[FilledCircle], 4};
			];
			, If[Length[downstreampts]>0
				, pts2plot = downstreampts; ptstyles = White; markers = {\[FilledCircle], 4};
				, pts2plot = {0,0}; ptstyles = White; markers = {\[FilledCircle], 2};
			];
		];
	, 2, 
		If[Length[pts]>0
			, If[Length[upstreampts]>0
				, If[Length[downstreampts]>0
					, pts2plot = {pts, upstreampts, downstreampts}; ptstyles = {White, Red, Red}; markers = {{\[FilledCircle], 2},{\[FilledCircle], 4},{\[FilledCircle], 4}};
					, pts2plot = {pts, upstreampts}; ptstyles = {White, Red}; markers = {{\[FilledCircle], 2},{\[FilledCircle], 4}};
				];
				, If[Length[downstreampts]>0
					, pts2plot = {pts, downstreampts}; ptstyles = {White, Red}; markers = {{\[FilledCircle], 2},{\[FilledCircle], 4}};
					, pts2plot = pts; ptstyles = White; markers = {\[FilledCircle], 2};
				];
			];
			, If[Length[upstreampts]>0
				, If[Length[downstreampts]>0
					, pts2plot = {upstreampts, downstreampts}; ptstyles = {White, White}; markers = {{\[FilledCircle], 4},{\[FilledCircle], 4}}
					, pts2plot = upstreampts; ptstyles = White; markers = {\[FilledCircle], 4};
				];
				, If[Length[downstreampts]>0
					, pts2plot = downstreampts; ptstyles = White; markers = {\[FilledCircle], 4};
					, pts2plot = {0,0}; ptstyles = White; markers = {\[FilledCircle], 4};
				];
			];
		];
];
	id = ImageDimensions[origframe];
	plot1 = ListPlot[pts2plot
			, PlotRange->{{0, id[[1]]}, {0, id[[2]]}}
			, AspectRatio->id[[2]]/id[[1]]
			, ImageSize->id[[1]]
			, PlotStyle->ptstyles
			, PlotMarkers->markers
			, Frame->None
			, Prolog->{
							Texture[SetAlphaChannel[drawNozzleOnImage[origframe, stats["noz"], mp, stats["be"]
														, stats["ppmm"], stats["criticalPoints"]]
													, 1]]
							,
							Polygon[{Scaled[{0, 0}], Scaled[{1, 0}], Scaled[{1, 1}], Scaled[{0, 1}]},
								VertexTextureCoordinates -> {{0, 0}, {1, 0}, {1, 1}, {0, 1}}]
						}];
	plot1
]


(* ::Subsubsection:: *)
(*streampoints[video_, frame_, statsinput_, diag_, superdiag_]*)


(* ::Text:: *)
(*list of upstream and downstream points from frame*)
(*video = file name (string)*)
(*frame = frame number (int)*)
(*statsinput = grasGeoAssociation (association) or literally anything else*)
(*diag = bool true to print*)
(*superdiag = bool true to print tons of shit*)
(*OUTPUT: upstream points, downstream points, plot, midplane location*)
(*NEEDS: grasGeoAssoc, importFrame, upstreampoints, downstreampoints, getPointsFromBW, framenum2midplane, streamPointsPlot*)


streampoints[video_, frame_, statsinput_, diag_, superdiag_]:=Module[{mp, stats, edgeim, pts, downstreampts, upstreampts, id, origframe, blurradius, plot1, time, ends, beginnings, errorRet, critpoints, adjustbadup, SD},
Catch[
	SD = True;
	errorRet = {};
	If[AssociationQ[statsinput], stats = statsinput, stats = grasGeoAssoc[video]];
		(*set up meta stats*)
	time = frame/stats["fr"];
	beginnings = stats["beginnings"];
	ends = stats["ends"];
	Do[If[ends[[i]]<time<beginnings[[i+1]], 
		Throw["Time = "<>ToString[time]<>", after line "<>ToString[i]<>"("<>ToString[ends[[i]]]<>","<>ToString[beginnings[[i+1]]]<>")"]
		]
	, {i, 8}];
	If[time>ends[[9]], Throw["Time = "<>ToString[time]<>", after line 9, ("<>ToString[ends[[9]]]<>")"]];
		(*check that this frame is in a print line*)
	origframe = importFrame[video, frame];
	If[superdiag && SD, Print[origframe]];
	blurradius = 3;
	upstreampts = {};
	downstreampts = {};
	mp = framenum2midplane[video, frame, stats];
	critpoints = 100;
	While[(Length[upstreampts]<1 || Length[downstreampts]<1) &&  blurradius>=0 && critpoints >=10,
		adjustbadup = True;
		edgeim = DeleteSmallComponents[EdgeDetect[Blur[origframe,blurradius]], critpoints];
			(*get edge image, throw out junk*)
		If[superdiag && SD, Print[ColorNegate@edgeim]];
		pts = getPointsFromBW[edgeim];
			(*get list of points from edge image*)
		If[Length[downstreampts]<1,
			downstreampts = downstreampoints[pts, stats, 2, mp, superdiag];
			If[Length[downstreampts]==0 && downstreampts == 1,
					blurradius = blurradius + 1;
					critpoints = Round[critpoints/2];
					downstreampts = {};
					adjustbadup = False;
			];
		];
			(*collect downstream points*)
		If[Length[upstreampts]<1,
			upstreampts = upstreampoints[pts, stats, mp, superdiag];
			If[Length[upstreampts]==0 && upstreampts == 1 && adjustbadup,
					blurradius = blurradius + 1;
					critpoints = Round[critpoints/2];
					upstreampts = {};
			];
			If[Max[upstreampts[[;;,1]]]<Max[downstreampts[[;;,1]]],
				upstreampts = {}
			];
		];
			(*collect upstream points, zero out if they don't span the midplane to the nozzle*)
		blurradius = blurradius - 1;
			(*loop by blurring the original image more if collection of either set of points failed*)
			
	];
	If[diag || superdiag, plot1 = streampointsPlot[pts, upstreampts, downstreampts, origframe, stats, mp], plot1 = {}];
		(*plot points on image*)
	If[superdiag && SD, Print[plot1]];
	{upstreampts, downstreampts, plot1, mp}
]]


(* ::Section:: *)
(*fitting points*)


(* ::Subsection:: *)
(*interpolating*)


(* ::Subsubsection:: *)
(*pixelGaussian[list_]*)


(* ::Text:: *)
(*uses a moving gaussian to smooth pixels in a list of points*)
(*list = list of (x,y) points (px)*)
(*OUTPUT = list of (x,y) points (px)*)
(*NEEDS: pixelGaussian1D*)


pixelGaussian[list_]:=Module[{minusx, plusx, xnew, minusy, plusy, ynew},
Table[pixelGaussian1D[list, i, xy], {i, Length[list]},{xy, {1,2}} ]
]


(* ::Subsubsection:: *)
(*pixelGaussian1D[list_, i_, xy_]*)


(* ::Text:: *)
(*uses a moving gaussian to smooth pixels in a list of points*)
(*list = list of (x,y) points (px)*)
(*i = index of point to smooth*)
(*xy = smoothing in x direction (1) or y direction (2)*)
(*OUTPUT = list of (x,y) points (px)*)
(*NEEDS: *)


pixelGaussian1D[list_, i_, xy_]:=Module[{listtemp, minus, plus, new, widest, g, gminus, gplus, radius},
radius = 3;
listtemp = Reverse[Abs[list[[i,xy]] - #]&/@list[[;;i, xy]]];
minus = Min[FirstPosition[listtemp, z_/;z>radius, {i}][[1]]-1, Min[i-1, 50]];
If[Abs[list[[i-minus, xy]] - list[[i, xy]]]>radius, minus = Max[minus - 1, 1]];
listtemp = Abs[list[[i,xy]] - #]&/@list[[i;;, xy]];
plus =  Min[FirstPosition[listtemp, z_/;z>radius, {(Length[list]-i+2)}][[1]]-1, Min[Length[list]-i, 50]];
If[Abs[list[[i+plus, xy]] - list[[i, xy]]]>radius, plus = Max[plus - 1, 1]];
gminus =  GaussianMatrix[minus][[minus+1, 1;;minus+1]];
gplus = GaussianMatrix[plus][[plus+1, plus+1;;2*plus+1]];
new =(gminus.list[[i-minus;;i, xy]] + gplus.list[[i;;i+plus, xy]])/(Total[gminus] + Total[gplus])
]


(* ::Subsubsection:: *)
(*ybackground[data_]*)


ybackground[dinterpdata_]:=Transpose[{dinterpdata[[;;,1]],EstimatedBackground[dinterpdata[[;;,2]]]}]


(* ::Subsubsection:: *)
(*ygaussianfilter[data_, range_]*)


(* ::Text:: *)
(*apply a gaussian filter across points*)
(*data = list of points (x,y) (px)*)
(*range = range across which to blur (int px)*)
(*OUTPUT: list of points (x,y) (px)*)
(*NEEDS:*)


ygaussianfilter[data_, range_]:=Transpose[{GaussianFilter[data[[;;,1]], range], GaussianFilter[data[[;;,2]], range]}]


(* ::Subsubsection:: *)
(*derivs[usp_, pg1_, range_, neg_, diag_]*)


(* ::Text:: *)
(*gets derivatives of a list of pixels*)
(*usp = original list of pixels (x,y) (px)*)
(*pg1 = smoothed list of pixels  (x,y) (px) or {} to ask the function to smooth it for you*)
(*range = smoothing range to use on 1st and 2nd derivatives*)
(*neg = 1 to take the background from the bottom, -1 to take the background from the top*)
(*diag = bool true to print diagnostics*)
(*OUTPUT: four functions and a plot*)
(*NEEDS: ybackground, ygaussianfilter, pixelGaussian*)


derivs[usp_, pg1_, range_, neg_, diag_]:=Module[{pg, interp, dinterpdata, dinterp, ddinterp
										, xmin, xmax, fmm, dfmm, ddfmm, ddinterpdata
										, plot, curvaturedata, curvatureinterp, cmm, shave, errorRet, showranges},
Catch[
errorRet = {0,0,0,0,ListPlot[usp, ImageSize->800, AxesStyle->Black, LabelStyle->Directive[12, Black]]};
If[Length[pg1]<30
	,If[Length[usp]<30
		,Throw[errorRet];
		,(*pg = pixelGaussian[usp];*)
		pg = ygaussianfilter[({1, neg}*#)&/@ybackground[({1, neg}*#)&/@usp], 10]; 
	];
	,pg = pg1
];

shave = 2;
pg = GatherBy[pg, #[[1]]&][[;;,1]];
	(*make sure there are no duplicate x points*)
interp = Interpolation[pg];
	(*interpolate function*)
{xmin, xmax} = interp["Domain"][[1]];
dinterpdata = Table[{x,interp'[x]}, {x, xmin+shave, xmax-shave, 0.1}];
	(*first derivative, cut off boundary pixels b/c less reliable*)
If[Length[dinterpdata]<30, Throw[errorRet]];
dinterp  = Interpolation[ygaussianfilter[dinterpdata, range]];
	(*smooth first derivative*)	 
{xmin, xmax} = dinterp["Domain"][[1]];
ddinterpdata = Table[{x,dinterp'[x]}, {x, xmin+2*shave, xmax-2*shave, 0.1}];
	(*2nd derivative, cut off boundary pixels b/c less reliable*)
If[Length[ddinterpdata]<30, Throw[errorRet]];
ddinterp  = Interpolation[ygaussianfilter[ddinterpdata, range/2]];
	(*smooth 2nd derivative*)
{xmin, xmax} = ddinterp["Domain"][[1]];	
curvaturedata = Table[{x,ddinterp[x]/(1 + dinterp[x]^2)^(3/2)}, {x, xmin+(*2**)shave, xmax-(*2**)shave, 0.1}];
If[Length[curvaturedata]<30, Throw[errorRet]];
curvatureinterp = Interpolation[ygaussianfilter[curvaturedata, range/2]];
{xmin, xmax} = curvatureinterp["Domain"][[1]];	
fmm = MinMax[(*Select[*)pg(*, xmin<=#[[1]]<=xmax&]*)[[;;,2]]];
dfmm = MinMax[(*Select[*)dinterpdata(*, xmin<=#[[1]]<=xmax&]*)[[;;,2]]];
ddfmm = MinMax[(*Select[*)ddinterpdata(*, xmin<=#[[1]]<=xmax&]*)[[;;,2]]];
cmm = MinMax[(*Select[*)curvaturedata(*, xmin<=#[[1]]<=xmax&]*)[[;;,2]]];
	(*normalize for plotting*)
If[diag, 
showranges = False;
plot = Show[ListPlot[{{#[[1]], (#[[2]] - fmm[[1]])/(fmm[[2]]-fmm[[1]])}&/@usp
				, {#[[1]], (#[[2]] - dfmm[[1]])/(dfmm[[2]]-dfmm[[1]])}&/@dinterpdata
				, {#[[1]], (#[[2]]-ddfmm[[1]])/(ddfmm[[2]]-ddfmm[[1]])}&/@ddinterpdata
				, {#[[1]], (#[[2]]-cmm[[1]])/(cmm[[2]]-cmm[[1]])}&/@curvaturedata
				}
			(*, Joined->True*)
			, PlotStyle->(Append[Table[{GrayLevel[i], Dashed}, {i,{0, 0.25, 0.5}}], {Red, Dashed}])
			, PlotLabels->{"y "<>If[showranges, ToString[FortranForm/@SetAccuracy[fmm,4]], ""]
								, Style["y' "<>If[showranges, ToString[FortranForm/@SetAccuracy[dfmm,4]], ""], GrayLevel[0.25]]
								, Style["y'' "<>If[showranges, ToString[FortranForm/@SetAccuracy[ddfmm,4]], ""], GrayLevel[0.5]]
								, Style["\[Kappa] "<>If[showranges, ToString[FortranForm/@SetAccuracy[cmm,4]], ""], Red]}
			, PlotRange->All]
	, Plot[{(interp[x] - fmm[[1]])/(fmm[[2]]-fmm[[1]])
			, (dinterp[x]-dfmm[[1]])/(dfmm[[2]]-dfmm[[1]])
			, (ddinterp[x]-ddfmm[[1]])/(ddfmm[[2]]-ddfmm[[1]])
			, (curvatureinterp[x]-cmm[[1]])/(cmm[[2]]-cmm[[1]])
			}
		, {x, xmin, xmax}
		, PlotStyle->Append[Table[GrayLevel[i], {i,{0, 0.25, 0.5}}], Red]	
		, PlotRange->All]
	, ImageSize->150*100/72
	, ImagePadding->{{22, 22}, {20, 5}}
	, PlotRange->{All, {-0.15, 1.15}}
	, AxesStyle->Black
	, LabelStyle->Directive[8*100/72, Black]
	, FrameLabel->{"x (px)", None}
	, AspectRatio->1/2
	, PlotRangePadding->None
	];
	,
	plot = ""
];
{interp, dinterp, ddinterp, curvatureinterp, plot}
]];


(* ::Subsection:: *)
(*extracting metrics out of fits*)


(* ::Subsubsection:: *)
(*dinflection[curvex_, curvey_, stats_, mp_]*)


(* ::Text:: *)
(*distance between inflection point and various geometric factors*)
(*{curvex, curvey} = point*)
(*stats = assoc*)
(*mp = midplane location (int px)*)
(*OUTPUT: {distance to nozzle, distance to substrate, distance to silicon}*)
(*NEEDS: dpbe*)


dinflection[curvex_, curvey_, stats_, mp_]:=Module[{dn, ds, dSi},
dn = dpbe[curvex, curvey, stats["be"]];
ds = curvey - mp;
dSi = curvex - stats["criticalPoints"][[1,1]];
{dn, ds, dSi}]


(* ::Subsubsection:: *)
(*inflection[curvex_, f_, cf_, stats_, mp_]*)


(* ::Text:: *)
(*calculates important measurements for points of highest curvature*)
(*curvex = x location of point (px)*)
(*f = function y(x)  (px)*)
(*cf = curvature function curvature(x) (px)*)
(*stats = association of video stats*)
(*mp = pixel location of midplane (px)*)
(*OUTPUT = list of measurements in px*)
(*NEEDS: dinflection*)


inflection[curvex_, f_, cf_, stats_, mp_]:=Module[{curvey, r, dn, ds, dSi, cu},
curvey = f[curvex];
cu = cf[curvex];
r = If[cu!=0, 1/cf[curvex], INFINITERADIUS];
{dn, ds, dSi} =  dinflection[curvex, curvey, stats, mp];
<|"x"->curvex, "y"-> curvey, "r"-> r, "dn"-> dn, "ds"-> ds, "dSi"-> dSi|>
]


(* ::Section:: *)
(*upstream fit*)


(* ::Subsection:: *)
(*interpolate upstream points*)


(* ::Subsubsection:: *)
(*upstreamanalysis[upstreampts_, smoothradius_, diag_, stats_, mp_]*)


(* ::Text:: *)
(*analyzes upstream points*)
(*upstreampts = list of points*)
(*smoothradius = radius to use for smoothing (px)*)
(*diag = bool true to print*)
(*stats = assoc*)
(*mp = midplane location fROM THE BOTTOM (px)*)
(*OUTPUT: list of stats*)
(*NEEDS: inflection*)


upstreamanalysis[upstreampts_, smoothradius_, diag_, stats_, mp_]:=Module[{u, du, ddu, cu, plotu, channelx, xmin
																			, ucurvex1, ucurvex2, ucurvex3
																			, ptSL, ptBL, lSL, lBL, \[Theta]SL, \[Theta]BL
																			, curvepointr, curvepointl, curvepointm
																			, minx, maxx, errorRet, xrange, avgslope, xmax
																			, ucurvey3, dn, ds, dSi, upp},
Catch[
{u, du, ddu, cu, plotu} = derivs[upstreampts, {}, smoothradius, 1, diag];
curvepointl = curvepointr = curvepointm = ERRORCURVEPOINT;
errorRet = {ConstantArray[ERRORCURVEPOINT,3], "", "", "", plotu, "", "", ""};
If[NumberQ[u], Throw[errorRet]];
channelx = stats["criticalPoints"][[3,1]];
minx = Max[channelx, Min[upstreampts[[;;,1]]]+10];
maxx = Max[upstreampts[[;;,1]]]-10;
If[maxx<=minx, Throw[errorRet]];
ucurvex1 = MinimalBy[Table[{x, cu[x]}, {x, minx, maxx}], #[[2]]&][[1,1]];
	(*point 1 = between channel and front*)
curvepointr = inflection[ucurvex1, u, cu, stats, mp];
xmin = Min[upstreampts[[;;,1]]]+10;
If[(u[ucurvex1] - Min[upstreampts[[;;,2]]])/stats["ppmm"] < 0.125 && xmin-10 > stats["criticalPoints"][[3,1]] ,
curvepointm = flatcurvepoint[upstreampts, ucurvex1, stats, mp];
,
If[channelx>xmin
	,
	(*(*point 2 = between min point and channel*) *)
	ucurvex2 = MinimalBy[Table[{x, cu[x]}, {x, xmin, channelx}], #[[2]]&][[1,1]];
	If[ucurvex2>xmin+1
		, 
		curvepointl = inflection[ucurvex2, u, cu, stats, mp];
		ucurvex3 = MaximalBy[Table[{x, cu[x]}, {x, ucurvex2, ucurvex1}], #[[2]]&][[1,1]];
		curvepointm = inflection[ucurvex3, u, cu, stats, mp];
	];
];
If[!NumberQ[curvepointm["x"]],
	If[xmin<ucurvex1-10
		,
		ucurvex3 = MaximalBy[Table[{x, cu[x]}, {x, xmin, ucurvex1}], #[[2]]&][[1,1]];
		curvepointm = inflection[ucurvex3, u, cu, stats, mp];
		,
		(*approximate as straight line*)
		curvepointm = flatcurvepoint[upstreampts, ucurvex1, stats, mp];
	];
];
];
ptSL = MinimalBy[upstreampts, #[[2]]&][[1]];
ptBL = MaximalBy[upstreampts, #[[1]]&][[1]];
lSL = ptSL[[1]] - stats["criticalPoints"][[3,1]];
lBL = ptBL[[1]] - stats["criticalPoints"][[3,1]];
xrange = Max[1, RankedMin[upstreampts[[;;,1]], 10] - ptSL[[1]]];
xmin = Max[du["Domain"][[1,1]], ptSL[[1]]];
xmax = Min[du["Domain"][[1,2]], ptSL[[1]]+xrange];
avgslope =Integrate[du[x], {x, xmin, xmax}]/(xmax-xmin);
\[Theta]SL = 180 - ArcTan[avgslope]/Degree;
\[Theta]BL = VectorAngle[{1, Integrate[du[x], {x, Max[du["Domain"][[1,1]], ptBL[[1]]-10], Min[du["Domain"][[1,2]], ptBL[[1]]]}]/10}, {1, stats["be"][[2]]}]/Degree;
	(*point 1 = min curvature front, point2 = min curvature back, point3 = max curvature between*)
plotu = Show[plotu, Graphics[If[NumberQ[#["x"]], Line[{{#["x"], 0}, {#["x"], 1}}], {}]&/@{curvepointr, curvepointl, curvepointm}]];
Return[{{curvepointr, curvepointl, curvepointm}, lSL, \[Theta]SL, \[Theta]BL, plotu, ptSL, ptBL, u}];

]]


flatcurvepoint[upstreampts_, ucurvex1_, stats_, mp_]:=Module[{upp, ucurvex3, ucurvey3, dn, ds, dSi, curvepointm},
		upp = Select[upstreampts[[;;,1]], #<=ucurvex1&];
		If[Length[upp]>0,
			ucurvex3 = Sort[upp][[Round[Length[upp]/2]]];
			ucurvey3 = Median[Select[upstreampts, #[[1]]==ucurvex3&][[;;,2]]];
			{dn, ds, dSi} = dinflection[ucurvex3, ucurvey3, stats, mp];
			curvepointm = <|"x"->ucurvex3, "y"-> ucurvey3, "r"-> INFINITERADIUS, "dn"-> dn, "ds"-> ds, "dSi"-> dSi|>;
			,
			curvepointm = ERRORCURVEPOINT;
		];
		curvepointm]


(* ::Section:: *)
(*downstream fit*)


(* ::Subsection:: *)
(*interpolate downstream points*)


(* ::Subsubsection:: *)
(*dsiContactPoint[dsp_, stats_]*)


(* ::Text:: *)
(*calculates downstream-nozzle contact point metrics for interpolated fits*)
(*dsp = list of points*)
(*stats = assoc*)
(*OUTPUT: length, angle, coordinates*)
(*NEEDS: *)


dsiContactPoint[dsp_, stats_]:=Module[{lineSiL, ne, ptSiLx, ptSiL, \[Theta]SiL, lSiL, s},
lineSiL = LinearModelFit[dsp[[-Min[25, Length[dsp]];;]],x,x];
If[lineSiL["RSquared"]>0.8
	,
	contactpt[lineSiL, stats]
	,
	ConstantArray["", 3]
]
]


contactpt[lineSiL_, stats_]:=Module[{ne, s, ptSiLx, ptSiL, \[Theta]SiL, lSiL},
ptSiL = {"", ""};
\[Theta]SiL = lSiL = "";
ne = stats["noz"][[1]];
	s = Quiet[Solve[{lineSiL[x]==ne[[1,2]]+( x-ne[[1,1]])/(ne[[2]]),ne[[1,1]] - 5<x<ne[[1,1]]+5}, x]];
	If[Length[s]>0,
		ptSiLx = x/.s[[1]];
		ptSiL = {ptSiLx, lineSiL[ptSiLx]};	
		(*Print[
			Show[ListPlot[dsp],Plot[{lineSiL[x], ne[[1,2]]-( x-ne[[1,1]])/(ne[[2]])}
					, {x, Min[dsp[[;;,1]]],ne[[1,1]]+10}], Graphics[{Red, Point[ptSiL]}], PlotRange\[Rule]{0, 280}]];*)
		\[Theta]SiL = VectorAngle[{1, lineSiL'[ptSiLx]}, {1, -1/(stats["noz"][[1,2]])}]/Degree;
		lSiL = EuclideanDistance[ptSiL, stats["criticalPoints"][[1]]];
	];
{lSiL, \[Theta]SiL, ptSiL}]


(* ::Subsubsection:: *)
(*curvept[xrange_, peak_, d_, stats_, mp_]*)


(* ::Text:: *)
(*makes various measurements for a point of high curvature*)
(*xrange = list of x points*)
(*peak = {position, curvature}*)
(*d = interpolation*)
(*stats = assoc*)
(*mp = midplane position in px from the bottom*)
(*OUTPUT: curvepoint associ*)
(*NEEDS: dinflection*)


curvept[xrange_, peak_, d_, stats_, mp_]:=Module[{dcurvex, dcurvey, r, dn, ds, dSi},
dcurvex = xrange[[peak[[1]]]];
dcurvey = d[dcurvex];
r = If[peak[[2]]!=0, -1/peak[[2]], INFINITERADIUS];
{dn, ds, dSi} = dinflection[dcurvex, dcurvey, stats, mp];
<|"x"-> dcurvex, "y"-> dcurvey, "r"-> r, "dn"-> dn, "ds"-> ds, "dSi"-> dSi|>
]


(* ::Subsubsection:: *)
(*dsicurvature[dsp_, downstreampts_, stats_, mp_, diag_]*)


(* ::Text:: *)
(*calculates metrics for the points of highest curvature for interpolated fits*)
(*dsp = list of points*)
(*downstreampoints = longer list of points*)
(*stats = assoc*)
(*mp = midplane in px from bottom*)
(*diag = bool true to get plots*)
(*OUTPUT: two curve point assocs, a plot, and an interpolation*)
(*NEEDS: derivs, curvept*)


dsicurvature[dsp_, downstreampts_, stats_, mp_, diag_]:=Module[{d, dd, ddd, cd, plotd, xrange, dp, fp, lowpeaks, cp, peaks, curvepointr, curvepointl, curvepointm, mincurvature},
Catch[
{d, dd, ddd, cd, plotd} = derivs[dsp, {}, 100, -1, diag];
	(*interpolate, take derivatives*)
(*If[Mean[(#[[2]] - d[#[[1]]])^2&/@dsp[[-10;;]]] > 5
	, Throw[{curvepointr, curvepointl, lSiL, \[Theta]SiL, plotd, ptSiL, d}]
];*)
If[NumberQ[d], Throw[{ConstantArray[ERRORCURVEPOINT,3], plotd, ""}]];
xrange = Range[cd["Domain"][[1,1]], cd["Domain"][[1,2]]];
	(*establish a search range in x*)
dp = Table[d[x], {x, xrange}];	
fp = FindPeaks[dp, 10, Padding->5];
If[Length[fp]>0,
	lowpeaks = xrange[[fp[[;;,1]]]];
	lowpeaks = Select[lowpeaks, #>20&];
	If[Length[lowpeaks]>0, 
		lowpeaks = Min[lowpeaks];
		If[diag, plotd = Show[plotd, Graphics[{Red, Line[{{lowpeaks, -1}, {lowpeaks, 1}}]}]]];
		If[lowpeaks<Max[downstreampts[[;;,1]]]-200,
			xrange = Range[lowpeaks, cd["Domain"][[1,2]]];
		];	
	];
];
cp = Table[cd[x], {x, xrange}];
	(*get a list of curvatures*)
peaks = SortBy[FindPeaks[cp, 25], #[[2]]&];
peaks = Round/@peaks[[-Min[2, Length[peaks]];;]];
	(*find the two largest peaks in curvature, where the peaks are indices*)
curvepointr = curvepointl = curvepointm = ERRORCURVEPOINT;
Switch[Length[peaks]
	,2,
	peaks = Sort[peaks];
	curvepointr = curvept[xrange, peaks[[2]], d, stats, mp];
	curvepointl = curvept[xrange, peaks[[1]], d, stats, mp];
	mincurvature = Min[cp[[peaks[[1,1]];;peaks[[2,1]]]]];	
	,1,
	curvepointr = curvept[xrange, peaks[[1]], d, stats, mp];
	mincurvature = Min[cp[[1;;peaks[[1,1]]]]];		
];
If[Length[peaks]>0,
	curvepointm = curvept[xrange, {Position[cp, mincurvature][[1,1]], mincurvature}, d, stats, mp];
];
{{curvepointr, curvepointl, curvepointm}, plotd, d}
]]


(* ::Subsubsection:: *)
(*downstreaminterp[downstreampts_, diag_, stats_, mp_]*)


(* ::Text:: *)
(*interpolates the downstream points to find criticall stats*)
(*downstreampts = list of {x,y} points*)
(*diag = bool true to print*)
(*stats = assoc*)
(*mp = position of midplane in px from the bottom*)
(*OUTPUT: curvepoint associations and contact point stats*)
(*NEEDS: curvept, dsiContactPoint, dsicurvature*)


downstreaminterp[downstreampts_, diag_, stats_, mp_]:=Module[{dsp, lineSiL, ne, ptSiLx, ptSiL, \[Theta]SiL, lSiL
																, d, dd, ddd, cd, plotd, curvepoints
																, xrange, cp, peaks, diffs
																, lowpeaks, fp, dp},

If[diag, plotd = ListPlot[downstreampts
	, ImageSize->800
	, AxesStyle->Black
	, LabelStyle->Directive[12, Black]
	];
];


dsp = Sort[downstreampts];
diffs = Abs[Differences[dsp[[;;,2]]]];
dsp = dsp[[Position[diffs, z_/;z<5][[;;,1]]]];
If[Length[dsp]<50, Throw[{ConstantArray[ERRORCURVEPOINT, 3], "", "", plotd, "", ""}]];

{lSiL, \[Theta]SiL, ptSiL} = dsiContactPoint[dsp, stats];
{curvepoints, plotd, d} = dsicurvature[dsp, downstreampts, stats, mp, diag];

	
{curvepoints, lSiL, \[Theta]SiL, plotd, ptSiL, d}
]


(* ::Subsection:: *)
(*fit downstream points to equation*)


(* ::Subsubsection:: *)
(*nlmds[dsp_, contactpoint_]*)


(* ::Text:: *)
(*fit a list of points to an equation*)
(*dsp = list of points*)
(*contactpoint = liquid-silicon intersection point*)
(*OUTPUT: FittedModel*)
(*NEEDS:*)


nlmds[dsp_, contactpoint_, stoppinglength_]:=Module[{nlm, maxperiod, yrange},
maxperiod = 3.*Pi/( Max[dsp[[;;,1]]] - Min[dsp[[;;,1]]]);
yrange = MinMax[dsp[[;;,2]]];
nlm = Quiet[Check[NonlinearModelFit[dsp, 
								{a + b*x + c*Exp[di*(x-Max[dsp[[;;,1]]])] + e*Sin[g*x]
									, c>=0
									, 0<= g<=maxperiod
								}
							 , {{a, yrange[[1]]}
							 , {b, (Max[dsp[[;;Round[Length[dsp]/2],2]]] - MinimalBy[dsp, #[[1]]&][[1,2]])/(Max[dsp[[;;,1]]] - Min[dsp[[;;,1]]])}
							 , {c, yrange[[2]] - yrange[[1]]}
							 , {di, 0.05}
							 , {e,  yrange[[2]] - yrange[[1]]}
							 , {g,0}
					}, x],0]];
If[NumberQ[nlm],
	nlm = Quiet[Check[NonlinearModelFit[dsp, 
								{a + b*x + c*Exp[d*(x-Max[dsp[[;;,1]]])]
									, c>=0
								}
							 , {{a, yrange[[1]]}
							 , {b, (Max[dsp[[;;Round[Length[dsp]/2],2]]] - MinimalBy[dsp, #[[1]]&][[1,2]])/(Max[dsp[[;;,1]]] - Min[dsp[[;;,1]]])}
							 , {c, yrange[[2]] - yrange[[1]]}
							 , {d, 0.05}
					}, x],0]];
	If[NumberQ[nlm],
		nlm = LinearModelFit[dsp, x, x];
	];
];
(*If[Abs[contactpoint[[2]] - nlm[contactpoint[[1]]]] > 3 && Length[dsp]>stoppinglength
	, nlmds[dsp[[Round[Length[dsp]/2];;]], contactpoint, stoppinglength]
	, nlm
]*)
nlm
					];


(* ::Subsubsection:: *)
(*fitdownstream[dsp_]*)


(* ::Text:: *)
(*fit downstream points to an equation*)
(*dsp = list of points*)
(*OUTPUT: fittedModel*)
(*NEEDS: nlmds*)


fitdownstream[dsp_]:=Module[{nlm, pts2, contactpoint, nlm2, errorRet},
Catch[
errorRet = 0;
If[Length[dsp]<20
	, Throw[errorRet]
		(*not enough points, throw an error*)
];

contactpoint = MaximalBy[dsp, #[[2]]&][[1]];
nlm = nlmds[dsp, contactpoint, Length[dsp]*0.4];
If[NumberQ[nlm]
	, Throw[errorRet]
		(*if this fit failed, throw an error*)
];
If[nlm["RSquared"]>0.99999
	, Throw[nlm]
		(*this fit is very good. keep it*)
];

pts2 = Select[dsp, Abs[nlm[#[[1]]] - #[[2]]]<5&];
If[Length[pts2]<Length[dsp]/2 || Length[pts2]==Length[dsp]
	,  If[nlm["RSquared"]>0.9999, Throw[nlm], Throw[0]]
	(*here we know that nlm is a bad fit with not enough salvagable points to work with, so throw an error*)
];
nlm2 = nlmds[pts2, contactpoint, Length[pts2]*0.4];
	(*the first fit was just okay, so refine it*)
If[NumberQ[nlm2] || nlm2["RSquared"]<0.9999
	, If[nlm["RSquared"]>0.9999, Return[nlm], Return[0]]
		(*the original fit was good enough, so return it*)
	, Return[nlm2]
		(*nlm2 is a good fit*)
];

]]


(* ::Subsubsection:: *)
(*secondfitdownstream[spreduced_]*)


(* ::Text:: *)
(*a second model for fitting downstream points*)
(*spreduced = list of points*)
(*OUTPUT: nonlinearmodelfit*)
(*NEEDS:*)


secondfitdownstream[spreduced_]:=Module[{xrange, yrange, nlm},
Catch[
xrange = MinMax[spreduced[[;;,1]]];
yrange = MinMax[spreduced[[;;,2]]];
nlm = Check[NonlinearModelFit[spreduced, {a + b*Cos[c*x]+d*x, c>Pi/(xrange[[2]] - xrange[[1]])}, {{a, Mean[yrange]},{b, (yrange[[2]] - yrange[[1]])/2},{c, 0}, d}, x],0];
If[!NumberQ[nlm] && nlm["RSquared"]>0.9999
	, Return[nlm]
	, Return[0]
]]];


(* ::Subsubsection:: *)
(*dcp[d_, mmx_, stats_, mp_, mmy_, downstreampts_, diag_]*)


(* ::Text:: *)
(*finds critical measurements for downstream curve points*)
(*d = nonlinearmodelfit*)
(*mmx = minmax in x (px): curve point must be between the min and max x*)
(*stats = assoc*)
(*mp = midplane position from bottom in px (px)*)
(*mmy = minmax in my (px) (only used for printing)*)
(*downstreampts = list of points (only used for printing)*)
(*diag = bool true to print diagnostics*)
(*OUTPUT: curvepoint assoc in px, plot (graphics)*)
(*NEEDS: dinflection*)


dcp[d_, mmx_, stats_, mp_, mmy_, downstreampts_, minimize_, diag_]:=Module[{curvepoint, dcurvex1, dcurvey1, ddn1, dds1, ddSi1, rd1, plotd, curvepointr},
curvepoint = If[minimize, Minimize, Maximize][{d''[x]/(1 + d'[x]^2)^1.5, mmx[[1]]<x<mmx[[2]]}, x];
If[diag, plotd = dcpplot[mmy, downstreampts, d, curvepoint, mmx]];
dcurvex1 = x/.curvepoint[[2]];
dcurvey1 = d[dcurvex1];
{ddn1, dds1, ddSi1} = dinflection[dcurvex1, dcurvey1, stats, mp];
rd1 = -1/curvepoint[[1]];
curvepointr =  <|"x"->dcurvex1, "y"-> dcurvey1, "r"-> rd1, "dn"-> ddn1, "ds"-> dds1, "dSi"-> ddSi1|>;
{curvepointr, plotd}]


dcpplot[mmy_, downstreampts_, d_, curvepoint_, mmx_]:=Module[{showeq},
showeq = False;
Show[
	ListPlot[{#[[1]], (#[[2]] - mmy[[1]])/(mmy[[2]] - mmy[[1]])}&/@downstreampts
		, PlotStyle->Black
		, PlotRange->{Automatic, {0,1}}
		
	]
	,
	Plot[{(d[x] - mmy[[1]])/(mmy[[2]] - mmy[[1]])
			, (d''[x]/(1 + d'[x]^2)^1.5)/curvepoint[[1]]
		}
		, {x, mmx[[1]], mmx[[2]]}
		, PlotStyle->{Black, Red}
		, PlotRange->{Automatic, {0,1}}
		, PlotLegends->Placed[{If[showeq, Row[{"y = ",Normal[SetPrecision[d,2]]}, ImageSize->200], "y"], Style["\[Kappa]", Red]}, {Left, Center}]
	]
, ImageSize-> 150*100/72
, LabelStyle->Directive[8*100/72, Black]
, FrameLabel->{"x (px)", None}
, ImagePadding->{{22, 22}, {20, 5}}
, PlotRange->{All, {-0.15,1.15}}
, AspectRatio->1/2
]]


(* ::Subsubsection:: *)
(*downstreamFittingMethod*)


downstreamFittingMethod[downstreampts_, stats_, d_, mp_, diag_]:=Module[{minx, ptSiL, \[Theta]SiL, mmx, mmy, ptSiLx
																		, minpoint, curvepointr, curvepointl, curvepointm
																		, plotd, maxpoint, d2, plotd2, lSiL, s},
{lSiL, \[Theta]SiL, ptSiL} = dsiContactPoint[downstreampts, stats];
If[!NumberQ[lSiL], {lSiL, \[Theta]SiL, ptSiL} = contactpt[d, stats]];
minx = Min[downstreampts[[;;,1]]];
If[NumberQ[ptSiL[[1]]], ptSiLx = ptSiL[[1]], ptSiLx = Max[downstreampts[[;;,1]]]];
mmx = {minx, ptSiLx};
	(*minmax in x*)
mmy = MinMax[downstreampts[[;;,2]]];
	(*minmax in y*)
minpoint = x/.(Minimize[{d''[x]/(1 + d'[x]^2)^1.5, mmx[[1]]<x<mmx[[2]]}, x][[2]]);
	(*point of minimum curvature*)
curvepointr = curvepointl = curvepointm = ERRORCURVEPOINT;
If[minpoint<mmx[[2]]
	, (*enough room for the right side*)
	{curvepointr, plotd} = dcp[d, {minpoint, mmx[[2]]}, stats, mp, mmy, downstreampts, False, diag];
	If[minpoint>mmx[[1]]+10
		, 
		(*enough room on the left side*)
		curvepointl = dcp[d, {mmx[[1]], minpoint}, stats, mp, mmy, downstreampts, False, False][[1]];
		curvepointm = dcp[d, {curvepointl["x"], curvepointr["x"]}, stats, mp, mmy, downstreampts, True, False][[1]];
		, 
		(*not enough room on the left side: change the search range*)
		minx = Min[downstreampts[[;;,1]]];
		If[minx<minpoint-10,
			maxpoint = MaximalBy[Select[downstreampts, minx<#[[1]]<minpoint&], #[[2]]&][[1,1]];
				(*peak to the left of the min*)
			If[maxpoint<minpoint-10,
				(*re-fit new span*)
				d2 = secondfitdownstream[Select[downstreampts, maxpoint+10<#[[1]]<minpoint + (minpoint - maxpoint)-10&]];				
				If[!NumberQ[d2],
					{curvepointl, plotd2} = dcp[d2, {maxpoint+10, minpoint + (minpoint - maxpoint)-10}, stats, mp, mmy, downstreampts, False, True];
					curvepointm = dcp[d2, {curvepointl["x"], curvepointr["x"]}, stats, mp, mmy, downstreampts, True, True][[1]];
					If[diag, plotd = Show[plotd, plotd2];];
				];
			];
		];
	];
	If[!NumberQ[curvepointm["x"]], curvepointm = dcp[d, {mmx[[1]], curvepointr["x"]}, stats, mp, mmy, downstreampts, True, True][[1]]];
	, (*not enough room for the right side*)
	If[minpoint>mmx[[1]]
		, curvepointl = dcp[d, {mmx[[1]], minpoint}, stats, mp, mmy, downstreampts, False, diag][[1]];
		curvepointm = dcp[d, {curvepointl["x"], mmx[[2]]}, stats, mp, mmy, downstreampts, True, True][[1]];
	];
];



{{curvepointr, curvepointl, curvepointm}, lSiL, \[Theta]SiL, plotd, ptSiL, d}
	(*curvepointr is the rightmost point of highest curvature, 
		curvepointl is the leftmost point of highest curvature
		curvepointm is the middle point of lowest curvature*)
];


(* ::Subsubsection:: *)
(*downstreamanalysis[downstreampts_, diag_, stats_, mp_]*)


(* ::Text:: *)
(*analyzes downstream points*)
(*downstreampts = list of points*)
(*diag = bool true to print*)
(*stats = assoc*)
(*mp = midplane location FROM THE BOTTOM (px)*)
(*OUTPUT: list of stats in px*)
(*NEEDS: fitdownstream, downstreaminterp, dcp*)


downstreamanalysis[downstreampts_, diag_, stats_, mp_]:=Module[{d, ne, ptSiLx, ptSiL, lSiL, \[Theta]SiL, mmx, mmy, curvepoint, plotd
																, dcurvex, dcurvey, ddn, dds, ddSi, rd, cd, dd, ddd, sb
																, fp, minx, errorRet, dsp, minpoint
																, curvepointr, curvepointl, curvepointm
																, maxpoint, d2, plotd2},
Catch[
d = fitdownstream[downstreampts];
If[NumberQ[d], Throw[downstreaminterp[downstreampts, diag, stats, mp]]];
Return[downstreamFittingMethod[downstreampts, stats, d, mp, diag]];

]]


(* ::Section:: *)
(*intersections*)


(* ::Text:: *)
(*a positive curvature is inside the filament*)
(*a negative curvature is outside the filament*)


(* ::Subsubsection:: *)
(*dIntersections[u_, d_, dcurvex_, dcurvey_, stats_, mp_, diag_]*)


(* ::Text:: *)
(*finds the width-wise radius of curvature of the filament at the upstream and downstream points of highest curvature*)
(*u = interpolated upstream surface*)
(*d = fittedmodel of downstream surface*)
(*dcurvex, dcurvey = points in px*)
(*stats = association*)
(*mp = midplane position FROM THE BOTTOM (px)*)
(*diag = bool true to print*)
(*OUTPUT: downstream inner radius of curvature*)
(*NEEDS:*)


dIntersections[u_, d_, dcurvex_, dcurvey_, stats_, mp_, diag_]:=Module[{be, cp, udomain, dpoint, dpointx, plots},
Catch[
If[!NumberQ[dcurvex], Throw[""]];
be = stats["be"];
cp = stats["criticalPoints"];
udomain = u["Domain"][[1]];

dpoint = Check[FindRoot[dcurvey + (x - dcurvex)*(-1/d'[dcurvex])==u[x]
						, {x, Mean[udomain], udomain[[1]], udomain[[2]]}], {}];
If[diag, 
		plots  = {{"downstream/upstream   "
				, Plot[{dcurvey + (x - dcurvex)*(-1/d'[dcurvex]), u[x]}
					, {x, udomain[[1]], udomain[[2]]}
					, ImageSize->300, PlotRange->{0, 280}, Epilog->Point[{dcurvex, dcurvey}]]
				, "\t"
				, dpoint}
		}
];
If[Length[dpoint]>0, 
	dpointx = x/.dpoint[[1]];
	dpoint = {dpointx, dcurvey + (dpointx - dcurvex)*(-1/d'[dcurvex])};
	dpoint = 0.5*EuclideanDistance[{dcurvex, dcurvey}, dpoint];
	If[diag, Print[Grid[Transpose[plots], Frame->True]]];
	Throw[dpoint];
	(*assume distance from downstream to upstream is full circle*)
	,
	dpoint = Solve[dcurvey + (x - dcurvex)*(-1/d'[dcurvex])==mp, x];
	If[diag, 
		plots = Append[plots, {"downstream/midplane   "
								, Plot[{dcurvey + (x - dcurvex)*(-1/d'[dcurvex]), mp}
										, {x, 0, cp[[2,1]]}
										, ImageSize->300, PlotRange->{0, 280}, Epilog->Point[{dcurvex, dcurvey}]]
								, "\t"
								, dpoint}
				];
		Print[Grid[Transpose[plots], Frame->True]]
	];
	If[Length[dpoint]>0, 
		dpointx = x/.dpoint[[1]];
		dpoint = {dpointx, dcurvey + (dpointx - dcurvex)*(-1/d'[dcurvex])};
		dpoint = EuclideanDistance[{dcurvex, dcurvey}, dpoint];
		Throw[dpoint];
		,
		Throw[""];
	];
	(*assume distance from downstream to substrate is half circle*)
];

]]


(* ::Subsubsection:: *)
(*uintersections[u_, d_, ucurvex_, ucurvey_, stats_, mp_, diag_]*)


(* ::Text:: *)
(*finds the width-wise radius of curvature of the filament at the upstream and downstream points of highest curvature*)
(*u = interpolated upstream surface*)
(*d = fittedmodel of downstream surface*)
(*ucurvex, ucurvey = points in px*)
(*stats = association*)
(*mp = midplane position FROM THE BOTTOM (px)*)
(*diag = bool true to print*)
(*OUTPUT: upstream inner radius of curvature*)
(*NEEDS:*)


uintersections[u_, d_, ucurvex_, ucurvey_, stats_, mp_, diag_, ppmm_]:=Module[{be, cp, ddomain, upoint, upointx, plots},
Catch[
If[!NumberQ[ucurvex], Throw[""]];
be = stats["be"];
cp = stats["criticalPoints"];
ddomain = d["Domain"][[1]];

upoint = Solve[{ucurvey + (x - ucurvex)*(-1/u'[ucurvex])==be[[1,2]] + be[[2]]*(x - be[[1,1]]), cp[[1,1]]<x<cp[[2,1]]}, x];
If[diag, 
	plots = {{"upstream/bottom edge   "
				,Plot[{ucurvey + (x - ucurvex)*(-1/u'[ucurvex]),
						be[[1,2]] + be[[2]]*(x - be[[1,1]])}
				, {x, cp[[1,1]], cp[[2,1]]}, ImageSize->300, PlotRange->{0, 280}, Epilog->Point[{ucurvex, ucurvey}]
				]
				, "\t"
				, upoint}
			}
];
If[Length[upoint]>0
	, (*good fit*)
	upointx = x/.upoint[[1]];
	upoint = {upointx, ucurvey + (upointx - ucurvex)*(-1/u'[ucurvex])};
	upoint = EuclideanDistance[{ucurvex, ucurvey}, upoint];
	upoint = convertChord[upoint, ppmm];
	If[diag, Print[Grid[Transpose[plots], Frame->True]];];
	(*assume distance from upstream to nozzle is half circle*)
	, (*bad fit*)
	upoint = Check[FindRoot[ucurvey + (x - ucurvex)*(-1/u'[ucurvex])==d[x]
						, {x, Mean[ddomain], ddomain[[1]], ddomain[[2]]}], {}];
	If[diag, 
		plots = Append[plots, {"upstream/downstream   ", 
								Plot[{ucurvey + (x - ucurvex)*(-1/u'[ucurvex]),d[x]}
									, {x, ddomain[[1]], ddomain[[2]]}, ImageSize->300, PlotRange->{0, 280}, Epilog->Point[{ucurvex, ucurvey}]]
						, "\t"
						, upoint}
				];
		Print[Grid[Transpose[plots], Frame->True]];
	];
	If[Length[upoint]>0
		, 
		upointx = x/.upoint[[1]];
		upoint = {upointx, ucurvey + (upointx - ucurvex)*(-1/u'[ucurvex])};
		upoint = 0.5*EuclideanDistance[{ucurvex, ucurvey}, upoint];
		upoint = px2mm[upoint, ppmm];
		(*assume distance from upstream to downstream is full circle*)
		,
		upoint = "";
]];
upoint
]]


(* ::Section:: *)
(*together fit*)


(* ::Subsubsection:: *)
(*laplacepressure[gamma_, {c1_, c2_}]*)


(* ::Text:: *)
(*laplace pressure as a function of radii*)
(*gamma = surface tension in mJ/m^3*)
(*c1, c2 = radii in mm*)
(*OUTPUT: number*)
(*NEEDS: *)


laplacepressure[gamma_, {c1_, c2_}]:=Module[{c1r, c2r},
If[NumberQ[c1]&& NumberQ[c2]
	, gamma * (1/c1 + 1/c2)
	, ""
]
]


(* ::Subsubsection:: *)
(*px2mm[n_, ppmm_]*)


(* ::Text:: *)
(*converts pixels to mm*)
(*n = number in px*)
(*ppmm = pixels per mm*)
(*OUTPUT: number*)
(*NEEDS: *)


px2mm[n_, ppmm_]:=If[NumberQ[n], n/ppmm, n]


(* ::Subsubsection:: *)
(*gammaAssoc*)


(* ::Text:: *)
(*surface energies in mJ/m^2 as a function of TEGDMA content*)


gammaAssoc = <|35 -> 34.65, 30 -> 37.89, 25 -> 40.23, 20 -> 41.24|>;


(* ::Subsubsection:: *)
(*streamMeasurementsPlot[diag_, plotd_, plotu_, dcurvex_, ucurvex_, nozzleplot_, ptSL_, ptBL_, ptSiL_, ucurvey_, dcurvey_]*)


streamMeasurementsPlot[superdiag_, diag_, plotd_, plotu_, dcurvepoints_, ucurvepoints_, nozzleplot_, ptSL_, ptBL_, ptSiL_, stats_, \[Theta]SL_, \[Theta]SiL_]:=Module[{ts, o, (*SMP, *)fs},
ts = 24;
o = 20;
fs = 20;
SMP = Show[nozzleplot, 
							Graphics[{White, PointSize[0.01]
										, If[NumberQ[#[[1]]], Point[#], {}]&/@{ptSL, (*ptBL, *)ptSiL}
										(*, If[NumberQ[ptSL[[1]]], Text[Style["SL", ts], ptSL + {-o, -o}], {}]*)
										(*, If[NumberQ[ptBL[[1]]], Text[Style["BL", ts], ptBL + {o, o}], {}]*)
										(*, If[NumberQ[ptSiL[[1]]], Text[Style["SiL", ts], ptSiL + {o, o}], {}]*)
										(*, White, Table[curvePoint[({ucurvepoints, dcurvepoints}[[j]])[[i]], Style[Subscript[{"U", "D"}[[j]], {"R", "L", "M"}[[i]]], ts], o*If[j==1,-1,1]], {i,3}, {j,2}]*)
										, White, curvePoint[ucurvepoints[[3]], "", -10, fs]
										, White, curvePoint[dcurvepoints[[1]], "", 10, fs]
										, Line[{ptSL, ptSL + 80{-Cos[\[Theta]SL*Degree], Sin[\[Theta]SL*Degree]}}]
										, Line[{ptSiL, ptSiL + 80{-Sin[\[Theta]SiL*Degree], -Cos[\[Theta]SiL*Degree]}}]
								}], Frame->None];
SMP = Show[ImageTrim[SMP, {{stats["criticalPoints"][[1,1]]-300, stats["firstmid"]-20}, {stats["criticalPoints"][[2,1]] + 10, stats["firstmid"]+150}}], ImageSize->400];
OUTPUTCOL = Column[{If[superdiag, Column[{
								Show[plotd, Graphics[Table[dcurveline[dcurvepoints[[i]], {"R", "L", "M"}[[i]]],{i,3}]]
										, PlotLabel->Style["Downstream", 8*100/72]]
								, Show[plotu, Graphics[Table[dcurveline[ucurvepoints[[i]], {"R", "L", "M"}[[i]]],{i,3}]]
										, PlotLabel->Style["Upstream", 8*100/72]]
							}], ""]
						, SMP}];	
If[diag, Print[OUTPUTCOL]];
]


dcurveline[dcurvepoint_, t_]:=If[NumberQ[dcurvepoint["x"]]
								, {Line[{{dcurvepoint["x"], 0}, {dcurvepoint["x"], 1}}], Text[Style[t, 10], {dcurvepoint["x"], 1.05}]}
								, {}
							];


curvePoint[curvepoint_, text_, o_, fs_]:=Module[{x, y},
x = curvepoint["x"];
y = curvepoint["y"];
If[NumberQ[x] 
	, {Point[{x,y}], Text[Style[text, fs], {x,y}+{-o,o}]}
	, {}
]]


(* ::Subsubsection:: *)
(*subtractPressures[p1_, p2_]*)


subtractPressures[{p1_, p2_}]:=If[NumberQ[p1]&& NumberQ[p2], p2 - p1, ""]


(* ::Subsubsection:: *)
(*convertChord[c_, ppmm_]*)


convertChord[c_, ppmm_]:=If[NumberQ[c] && c<INFINITERADIUS
												, ((c/ppmm)/2 + 0.35^2/(8*c/ppmm))
												, ""
										]


(* ::Subsubsection:: *)
(*streamMeasurementsReturn*)


streamMeasurementsReturn[superdiag_, diag_, plotd_, plotu_, dcurvepoints_
						, ucurvepoints_, nozzleplot_, ptSL_, ptBL_, ptSiL_
						, frame_, ppmm_, h_, rutrans_, rdtrans_, Plu_, Pld_
						, \[Theta]SL_, \[Theta]BL_, \[Theta]SiL_, Pdiffs_, lSL_, lSiL_, stats_]:=Module[{},
streamMeasurementsPlot[superdiag, diag, plotd, plotu, dcurvepoints, ucurvepoints, nozzleplot, ptSL, ptBL, ptSiL, stats, \[Theta]SL, \[Theta]SiL];
Join[
	{frame, frame/stats["fr"], h} 
	, px2mm[#, ppmm]&/@Flatten[({#["dn"], #["ds"], #["dSi"]}&/@Flatten[{ucurvepoints, dcurvepoints}])]
	, Flatten@Table[{px2mm[ucurvepoints[[i]]["r"], ppmm], rutrans[[i]], Plu[[i]]}, {i, Length[ucurvepoints]}]
	, Flatten@Table[{px2mm[dcurvepoints[[i]]["r"], ppmm], rdtrans[[i]], Pld[[i]]}, {i, Length[dcurvepoints]}]
	, Pdiffs
	, {\[Theta]SL, \[Theta]BL, \[Theta]SiL}
	, px2mm[#, ppmm]&/@{lSL, lSiL}
	, {dplud[Plu, Pld]}
]
]


dplud[Plu_, Pld_]:=Module[{u, d}, Catch[
If[NumberQ[Plu[[3]]], u = Plu[[3]], Throw[""]];
If[NumberQ[Pld[[1]]], d = Pld[[1]], If[NumberQ[Pld[[3]]], d = Pld[[3]], Throw[""]]];
u - d
]]


(* ::Subsubsection:: *)
(*streamMeasurements[video_, frame_, statsinput_, smoothradius_, diag_]*)


(* ::Text:: *)
(*measurements of filament parameters*)
(*video = file name (string)*)
(*frame = frame number (int)*)
(*statsinput = grasGeoAssociation (association) or literally anything else*)
(*diag = bool true to print*)
(*OUTPUT: list of parameters*)
(*NEEDS: grasGeoAssoc, streampoints, upstreamanalysis, downstreamanalysis, streamMeasurementsPlot, px2mm, dIntersections, uIntersections, laplacepressure*)


streamMeasurements[video_, frame_, statsinput_, diag_, superdiag_]:=Module[{smoothradius, stats, upstreampts, downstreampts
												 , plotu, plotd, ppmm, gamma
												 , cu, cd, rud, mp, nozzleplot, spout
												 , rdtrans, rutrans
												 , dun, dus, duSi, ddn, dds, ddSi, ptBL, ptSL, ptSiL
												 , lSL, lBL, lSiL, \[Theta]SL, \[Theta]BL, \[Theta]SiL, errorRet, u, d
												 , ucurvepoints, dcurvepoints
												 , Plu, Pld, Pdiffs, flowpressure, h, rtrans
												},
Catch[Quiet[
smoothradius = 50;
errorRet = ConstantArray["", Length[SMHEADER2]];
errorRet[[1]] = frame;
(*flowpressure = Median[Select[Import[StringReplace[video, {"gras"\[Rule]"data", "avi"\[Rule]"csv"}]][[;;,2]], #>0&]]*100; (*flow pressure in pascal*)*)
If[diag, Print[video, "\t", frame]];
If[AssociationQ[statsinput], stats = statsinput, stats = grasGeoAssoc[video]];
errorRet[[2]] = frame/stats["fr"];
spout = streampoints[video, frame, stats, diag, False];
If[Length[spout]!=4, 
	If[diag, Print[importFrame[video, frame]]];
	Throw[errorRet]
];
{upstreampts, downstreampts, nozzleplot, mp} = spout;
If[Length[upstreampts]<10 || Length[downstreampts]<10, If[diag, Print[nozzleplot]];Return[errorRet]];

ppmm = stats["ppmm"];
(*------UPSTREAM-----*)
{ucurvepoints, lSL, \[Theta]SL, \[Theta]BL, plotu, ptSL, ptBL, u} = upstreamanalysis[upstreampts, smoothradius, superdiag, stats, mp];
(*-----DOWNSTREAM----*)
{dcurvepoints, lSiL, \[Theta]SiL, plotd, ptSiL, d} = downstreamanalysis[downstreampts, superdiag, stats, mp];
(*Print[ucurvepoints, dcurvepoints];*)
(*----CURVATURE INTERSECTIONS---*)
If[And@@(!NumberQ[#["x"]]&/@ucurvepoints) || And@@(!NumberQ[#["x"]]&/@dcurvepoints), 
	Throw[streamMeasurementsReturn[superdiag, diag, plotd, plotu, dcurvepoints, ucurvepoints, nozzleplot, ptSL, ptBL, ptSiL, frame, ppmm, mp
													, ConstantArray["", Length[ucurvepoints]], ConstantArray["", Length[dcurvepoints]], ConstantArray["", Length[ucurvepoints]], ConstantArray["", Length[dcurvepoints]]
													, \[Theta]SL, \[Theta]BL, \[Theta]SiL, ConstantArray["", Binomial[Length[dcurvepoints],2] +  Binomial[Length[ucurvepoints],2]], lSL, lSiL, stats]];
];
h = px2mm[(stats["criticalPoints"][[3,2]] - mp), ppmm];
rtrans = h/2 + 0.35^2/(8h);
rdtrans = ConstantArray[rtrans, Length[dcurvepoints]];
rutrans = ConstantArray[rtrans, Length[ucurvepoints]];
(*rdtrans = px2mm[dIntersections[u, d, #["x"], #["y"], stats, mp, superdiag], ppmm]&/@dcurvepoints;
rutrans = uintersections[u, d, #["x"], #["y"], stats, mp, superdiag, ppmm]&/@ucurvepoints;*)

(*----TOGETHER-----*)
gamma = gammaAssoc[stats["tegdma"]];
Plu = Table[laplacepressure[gamma, {px2mm[ucurvepoints[[i]]["r"], ppmm], rutrans[[i]]}], {i, Length[ucurvepoints]}];
Pld = Table[laplacepressure[gamma, {px2mm[dcurvepoints[[i]]["r"], ppmm], rdtrans[[i]]}], {i, Length[dcurvepoints]}];
	(*laplace pressures*)
	
Pdiffs = subtractPressures/@Join[Subsets[Plu, {2}], Subsets[Pld, {2}]];


Return[streamMeasurementsReturn[superdiag, diag, plotd, plotu, dcurvepoints, ucurvepoints, nozzleplot, ptSL, ptBL, ptSiL, frame, ppmm, h
											, rutrans, rdtrans, Plu, Pld
											, \[Theta]SL, \[Theta]BL, \[Theta]SiL, Pdiffs, lSL, lSiL, stats]];
]]]


(* ::Section:: *)
(*File-level operations*)


(* ::Subsubsection:: *)
(*curvaturesOneLine[beginning_, end_, video_, stats_]*)


(* ::Text:: *)
(*get a table of stats for one line*)
(*beginning, end = frame numbers (int)*)
(*video = file name (string)*)
(*stats = assoc*)
(*forcefull = 1 to force the program to measure every frame, 2 to force it to go short, 0 to let it decide for itself*)
(*OUTPUT: list of curvatures*)
(*NEEDS: streamMeasurements*)


curvaturesOneLine[beginning_, end_, video_, stats_, forcefull_, newfilename_]:=Module[{rowsinit, rows, initialframes, linelength, framesfull, newbeginning, span, newend, stde, lsll, newframes, expanded},
linelength = end - beginning;
newbeginning = beginning + Round[linelength*0.4];
span = Round[linelength*0.5];
newend = newbeginning + span;
expanded = False;
If[FileExistsQ[newfilename]
	,
	rowsinit = Import[newfilename];
	initialframes = Select[rowsinit[[;;,1]], NumberQ];
	,
	initialframes = DeleteDuplicates[Sort[RandomInteger[{newbeginning, newend}, 80]]];
	rowsinit = Table[streamMeasurements[video, f, stats, False, False], {f, initialframes[[1;;40]]}];
	expanded = True;
];
lsll = Select[rowsinit[[;;,49]], NumberQ];
stde = If[Length[lsll]>1, StandardDeviation[lsll], 10];
If[(stde > 0.03 || forcefull==1) 
	,
	If[forcefull!=1,
		(*test a few more points*)
		If[FileExistsQ[newfilename]
			, (*if there is already a file, select a random list of new points*) 
			newframes = Complement[Sort[RandomInteger[{newbeginning, newend}, 80]], initialframes];
			newframes = newframes[[1;;Min[Length[newframes], 40]]];
			, (*if there is no file, select the second half of the points we randomly selected*)
			newframes = initialframes[[41;;]];
		];
		(*join the new and old datasets and get the standard deviation of lsl*)
		rowsinit = Join[rowsinit, Table[streamMeasurements[video, f, stats, False, False], {f, newframes}]];
		expanded = True;
		lsll = Select[rowsinit[[;;,49]], NumberQ];
		stde = If[Length[lsll]>1, StandardDeviation[lsll], 10];
	];
	If[(stde > 0.03 || forcefull==1) && forcefull<2,
		(*if we're forcing the new fit or if the fit is bad and we're not forcing a short fit, fit all frames*)
		Print[newfilename, "\t", SetAccuracy[stde,4], "\t full line (", newbeginning, ",", newend, ")"];
		framesfull = Range[newbeginning, newend];
		rows = Table[streamMeasurements[video, f, stats, False, False]
				, {f, Complement[framesfull, initialframes]}];
		rowsinit = SortBy[Join[rows, rowsinit], First];
		expanded = True;
		,
		(*if we don't have to do every frame, export*)
		If[expanded,
			Print[newfilename, "\t", SetAccuracy[stde,4], "\t partial line\t", {newbeginning, newend}, "\t ", Length[rowsinit]];
			rowsinit = Sort[rowsinit];
		];
	];
	,
	(*if the fit is good and we don't have to do every frame, export*)
	If[expanded,
		Print[newfilename, "\t", SetAccuracy[stde,4], "\t partial line\t", {newbeginning, newend}, "\t ", Length[rowsinit]];
		rowsinit = Sort[rowsinit];
	];
];
rows = rowsinit;
If[expanded,
	Export[newfilename, Prepend[Select[rows, NumberQ[#[[1]]]&], SMHEADER2]];
	Print["Exported ", newfilename];
];
]


(* ::Subsubsection:: *)
(*curvaturesWholeVideo[video_]*)


(* ::Text:: *)
(*get a table of curvatures for the whole video*)
(*video: file name*)
(*forcefull = bool true to force the program to measure every frame*)
(*OUTPUT: nothing, but it exports a file*)
(*NEEDS: grasGeoAssoc, curvaturesOneLine*)


curvaturesWholeVideo[video_, forcefull_]:=Module[{stats, fr, endframes, beginningframes, rows, newfilename, filelength, fen, len},
SetDirectory["D:\\Shear droplet\\shearDroplets"];
If[!FileExistsQ[StringReplace[video, {"gras"->"grasPositions", "avi"->"csv"}]],
stats = grasGeoAssoc[video];
fr = stats["fr"];
endframes = Round/@(stats["ends"]*fr);
beginningframes = Round/@(stats["beginnings"]*fr);
Do[
	newfilename = StringReplace[video, {"gras"->"grasPositions2_line"<>ToString[i], "avi"->"csv"}];
	fen = FileExistsQ[newfilename];
	If[forcefull && fen, len = Length[Import[newfilename]], len = 1000];
	If[!fen || len <50,
		curvaturesOneLine[beginningframes[[i]], endframes[[i]], video, stats, forcefull, newfilename];
	];
,{i, 1, 9}];
]];


(* ::Subsubsection:: *)
(*overrideunderfit[video_]*)


(* ::Text:: *)
(*same as curvaturesWholeVideo, but checks the manual characterization and forces the program to do a full run if there is dropletting*)
(*video = file name (string)*)
(*OUTPUT: exports videos*)
(*NEEDS: grasGeoAssoc, MAPS, stats2mkey, curvaturesOneLine*)


overrideunderfit[video_, linelist_]:=Module[{stats, forcefull, thistable, fr, endframes, beginningframes, ta, newfilename, ll},
If[Length[linelist]>0, ll = linelist, ll = Range[9]];
If[!ValueQ[MAPS], initializeQualitativeFits];
	stats = grasGeoAssoc[video];
	If[Length[Intersection[{"M", "D", "U"},
							Sort[Select[MAPS[stats2mkey[stats]], #[[5]]==stats["vf"] && MemberQ[stats["speeds"], #[[6]]]&]][[;;,-1]]
				]]>0
		, forcefull = 1 (*force full*)
		, forcefull = 2 (*force short*)
	];
	thistable = FileNames[StringReplace[FileNameTake[video], {"gras"->"grasPositions3_line*", "avi"->"csv"}], "*", Infinity];
	If[forcefull==1 || Length[thistable]<9 || Length[linelist]>0,	
		fr = stats["fr"];
		endframes = Round/@(stats["ends"]*fr);
		beginningframes = Round/@(stats["beginnings"]*fr);
		Do[
			newfilename = StringReplace[video, {"gras"->"grasPositions3_line"<>ToString[i], "avi"->"csv"}];
			If[FileExistsQ[newfilename]
				, ta = Import[newfilename]
				, ta = {}
			];
			If[Length[ta]<90,
				curvaturesOneLine[beginningframes[[i]], endframes[[i]], video, stats, forcefull, newfilename];
			];
		,{i, ll}];
	];];
