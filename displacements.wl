(* ::Package:: *)

DISPHEADER = {"TEGDMA", "h", "noz", "sub", "vF", "vS", "critLength", "max x", "mean x", "median x", "ster x", "max y", "mean y", "median y", "ster y"
				, "max x/y", "mean x/y", "median x/y", "ster x/y", "max x/max y", "mean x/meany", "median x/median y", "ster x/ster y"};


(* ::Subsubsection:: *)
(*plotVectorsPic[frames_, crit_]*)


(* ::Text:: *)
(*list of data*)
(*frames = list of frames*)
(*crit = critical displacement (suggest 2)*)
(*OUTPUT: list of max, mean, median, and standard error of x disp, y disp, x/y disp*)
(*NEEDS:*)


plotVectorsPic[frames_, crit_]:=Module[{list, width, height, bd, f1, average, s, ratios},
bd = BorderDimensions[SelectComponents[Dilation[Binarize[frames[[1]]],5], "Count", -1]];
f1 = ImagePad[#, -{bd[[1]], {0,0}}]&/@frames;
list = Reverse/@Transpose[Mean[ImageDisplacements[f1]]];
{width, height} = Dimensions[list][[1;;2]];
s = Select[Flatten[Abs[list],1], #[[1]]>crit|| #[[2]]>crit&];
If[Length[s]>0
,
	ratios = #[[1]]/#[[2]]&/@s;
	average = {Max@s[[;;,1]], Mean@s[[;;,1]], Median@s[[;;,1]], standardError@s[[;;,1]]
				, Max@s[[;;,2]], Mean@s[[;;,2]], Median@s[[;;,2]], standardError@s[[;;,2]]
				, Max@ratios, Mean@ratios, Median@ratios, standardError@ratios};
	average = Join[average, average[[1;;4]]/average[[5;;8]]];
,
	average = ConstantArray[0, 16];
];
average]


(* ::Subsubsection:: *)
(*pvp[frames_]*)


(* ::Text:: *)
(*just get the displacement across the image width*)
(*frames = list of frames*)
(*OUTPUT: {list of data, {full displacements, width of data, height of data, border dimensions of filament region, and first frame}}*)
(*NEEDS:*)


pvp[frames_]:=Module[{list, width, height, bd, f1, average, s, ratios},
bd = BorderDimensions[SelectComponents[Dilation[Binarize[frames[[1]]],5], "Count", -1]];
f1 = ImagePad[#, -{bd[[1]], {0,0}}]&/@frames;
list = Reverse/@Transpose[Mean[ImageDisplacements[f1]]];
{width,height} = ImageDimensions[frames[[1]]];
s = Select[Flatten[Abs[list],1], #[[1]]>2|| #[[2]]>2&];
If[Length[s]>0
	, average = {Max@s[[;;,1]], Mean@s[[;;,1]],Median@s[[;;,1]], standardError@s[[;;,1]]};
	, average = ConstantArray[0, 4]
];
{average,{list, width, height, bd,frames[[1]]}}
];


(* ::Subsubsection:: *)
(*imageMean[im_]*)


(* ::Text:: *)
(*average intensity of an image*)
(*im = image*)
(*OUTPUT: number*)
(*NEEDS: *)


imageMean[im_]:=N@Mean[Flatten[ImageData[im]]]


(* ::Subsubsection:: *)
(*calibrateflea[fv_]*)


(* ::Text:: *)
(*recalibrate frames to fit to the grasshopper frames b/c of frame rate errors. *)
(*fv = flea video file name*)
(*OUTPUT: location of left corner, image size, list of frames to probe, grasshopper video stats*)
(*NEEDS: imageMean*)


calibrateflea[fv_]:=Module[{stats, fr, fc, crop, i, frame, croppx, frameRanges, leftcorner, size, f, lastframe, lastcroppx, ratio, t1, f1, im, ydim},
stats = grasGeoAssoc[StringReplace[fv, {"flea"->"gras"}]];
fr = Import[fv, "FrameRate"];
fc = Import[fv, "FrameCount"];
crop = 0.5;
i = -1;
frame = "";
lastcroppx = croppx = 0;
f =Round[fc + i*fr];
While[crop>0.3 && i>-30 && f>1,
	lastframe = frame;
	frame = importFrame[fv, f];
	If[i==-1, ydim = ImageDimensions[frame][[2]]];
	im = imageMean[ImageTake[frame, -Round[ydim/2]]];
	(*Print[frame, "\t", im];*)
	If[im<0.1 || im>0.4
		,
		crop = 0.5;
		,
		If[croppx>0
			, lastcroppx = croppx
			, If[ImageQ[lastframe]
				, lastcroppx = BorderDimensions[MorphologicalBinarize[lastframe, {0.5, 0.1}]][[2,1]]
				];
		];
		croppx = BorderDimensions[MorphologicalBinarize[frame, {0.5, 0.1}]][[2,1]];
		crop = N@croppx/ydim;
		ratio = f/(stats["ends"][[9]]*fr);
	];
i = i-1;
f =Round[fc + i*fr];
];
While[lastcroppx/ydim<0.3 && i<-2,
	i = i+1;
	(*Print[fc, "\t", Floor[fc + (i+1)*fr]];*)
	lastcroppx = BorderDimensions[MorphologicalBinarize[importFrame[fv,Floor[fc + (i+1)*fr]] , {0.5, 0.1}]][[2,1]];
];
If[lastcroppx/ydim<0.3 || !NumberQ[lastcroppx], lastcroppx = ydim/2];
fr = Import[fv, "FrameRate"]*ratio;
frameRanges = Table[Round[(0.5*stats["beginnings"][[i]] + 0.5*stats["ends"][[i]])*fr]+j, {i, 9}, {j, 10}];
leftcorner = {0, lastcroppx -Round[0.15*ydim]};
size = {ImageDimensions[frame][[1]], Round[ydim*0.35]};
Print[fv];
(*Print[fv, "\n",lastframe,"\t", frame,"\t", ratio,"\t", lastcroppx, "\t", croppx,"\t",  Grid[Partition[ Table[ImageTrim[importFrame[fv, i], {leftcorner, leftcorner +size}], {i, frameRanges[[;;,1]]}], 3]]];*)
{leftcorner, size, frameRanges, stats} 
];


(* ::Subsubsection:: *)
(*pVectors[{list_, width_, height_, bd_, frame_}, scale_]*)


(* ::Text:: *)
(*plot vectors on top of image*)
(*list = list of vectors*)
(*width = width of image*)
(*height = height of image*)
(*bd = border dimensions of filament*)
(*frame = one frame*)
(*scale = scale vectors by this amount*)
(*OUTPUT: graphics*)
(*NEEDS: *)


pVectors[{list_, width_, height_, bd_, frame_}, scale_]:=Module[{},
ListVectorPlot[list, 
						PlotRange->{{0, width}, {1, height}}, 
DataRange-> {{bd[[1,1]], width-bd[[1,2]]}, {1, height}},
						AspectRatio->height/width, 
						VectorStyle->(*Red*)Magenta,
VectorScale->{0.3*scale,Scaled[0.3], Automatic}, 
						ImageSize->ImageDimensions[frame][[1]],
						Frame->None,
						Prolog->{
							Texture[SetAlphaChannel[frame, 1]]
							,
							Polygon[{Scaled[{0, 0}], Scaled[{1, 0}], Scaled[{1, 1}], Scaled[{0, 1}]},
								VertexTextureCoordinates -> {{0, 0}, {1, 0}, {1, 1}, {0, 1}}]
						}
       ]
]


(* ::Subsubsection:: *)
(*pvps[fv_]*)


(* ::Text:: *)
(*get graphics for displacements*)
(*fv = fleavideo name*)
(*OUTPUT: analysis graphics*)
(*NEEDS: calibrateflea, pVectors, pvp*)


pvps[fv_]:=Module[{leftcorner, size, frameRanges, stats, f1, tab1, imdim, w, scale, pictures}, 
{leftcorner, size, frameRanges, stats} = calibrateflea[fv];
tab1 =Table[
f1 = Table[ImageTrim[importFrame[fv, j], {leftcorner, leftcorner +size}], {j, Flatten[frameRanges[[3i-1;;3i]]]}];
pvp[f1]
, {i, 1, 3}];
scale = Table[N@tab1[[i,1,2]]/Max[tab1[[;;, 1,2]]], {i,3}];
Print[scale];
pictures = Table[pVectors[tab1[[i,2]], scale[[i]]], {i, 3}];
imdim = ImageDimensions[pictures[[1]]];
w =140;
Print[Row[tab1[[;;,1]], "\t"]];
Row[Table[Show[Show[pictures[[i]],Graphics[{White, Rectangle[{444 - 87.7143, 10}, {444 - 10, 15}]}], ImageSize->w], Graphics[Text[Style[{"A", "B", "C"}[[i]], Directive[12*100/72, White]], {20, imdim[[2]]-20}]]], {i,3}], "  "]
];


(* ::Subsubsection:: *)
(*exportFlea[fv_, crit_]*)


(* ::Text:: *)
(*export data from flea video*)
(*fv = fleavideo file name (avi)*)
(*crit = critical vector length to be included in analysis (suggested 2)*)
(*OUTPUT: exports stats*)
(*NEEDS: calibrateflea, plotVectorsPic*)


exportFlea[fv_, crit_]:=Module[{t1, f1, leftcorner, size, frameRanges, stats},
{leftcorner, size, frameRanges, stats} = calibrateflea[fv];
t1 = Table[
		f1 = Table[ImageTrim[importFrame[fv, i], {leftcorner, leftcorner +size}], {i, Flatten[frameRanges[[3i-1;;3i]]]}];
		Join[{stats["tegdma"], stats["h"], stats["nozcoat"], stats["subcoat"], stats["vf"], stats["speeds"][[3*i-1]], crit}, plotVectorsPic[f1, crit]]
	, {i, 1, 3}];
Export[StringReplace[fv, {"flea"->"flea_disps"<>ToString[crit], "avi"->"csv"}], Prepend[t1, DISPHEADER]];
];
