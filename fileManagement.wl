(* ::Package:: *)

(* ::Section:: *)
(*File management*)


(* ::Subsubsection:: *)
(*filemaker[t_, h_, noz_, sub_, vf_, vs_]*)


(* ::Text:: *)
(*filemaker produce search string for a video, given parameters*)
(*t = tegdma loading (int)*)
(*h = standoff distance (int)*)
(*noz = nozzle coating (string)*)
(*sub = substrate coating (string)*)
(*vf = flow speed (int)*)
(*vs = stage speed (string)*)
(*OUTPUT: string*)
(*NEEDS:*)


filemaker[t_, h_, noz_, sub_, vf_, vs_]:="*gras_T"<>ToString[t]<>
											"_H"<>ToString[h]<>
											"_"<>noz<>
											"_"<>sub<>
											"_vF"<>ToString[vf]<>
											"_vS"<>vs<>"*.avi"


(* ::Subsubsection:: *)
(*filefinder[t_, h_, noz_, sub_, vf_, vs_]*)


(* ::Text:: *)
(*filefinder find a file name, given a set of parameters*)
(*t = tegdma loading (int)*)
(*h = standoff distance (int)*)
(*noz = nozzle coating (string)*)
(*sub = substrate coating (string)*)
(*vf = flow speed (int)*)
(*vs = stage speed (string)*)
(*OUTPUT: file name*)
(*NEEDS: filemaker*)


filefinder[t_, h_, noz_, sub_, vf_, vs_]:=Module[{f1},
 f1 = FileNames[filemaker[t, h, noz, sub, vf, vs], "*", Infinity];
 If[Length[f1]>0, Sort[f1][[-1]], "failed.avi"]
 ]


(* ::Subsubsection:: *)
(*compareFXN[vs_, vf_, fxn_]*)


(* ::Text:: *)
(*compareFXN compare results for all tests for a given function*)
(*vs = stage speed (string)*)
(*vf = flow speed (int)*)
(*fxn = function describing what to do with each file name*)
(*OUTPUT: grid of images*)
(*NEEDS: filefinder*)


compareFXN[vs_, vf_, fxn_]:=Module[{},Catch[
If[!StringQ[vs] || !NumberQ[vf], Return["inputs must be a string, a number, and a function"]];
Grid[Join[
Table[{"u25/h"<>ToString[h]<>"/FDTS/film", fxn[filefinder[25, h, "FDTS", "film", vf, vs]]}
		, {h, {100, 200, 250}}],
Flatten[Table[{"u25/h150/"<>sub<>"/"<>noz, fxn[filefinder[25, 150, noz, sub, vf, vs]]}
		, {sub, {"film", "glass"}}
		, {noz, {"FDTS", "0"}}],1],
Table[{"u"<>ToString[t]<>"/h150/FDTS/film", fxn[filefinder[t, 150, "FDTS", "film", vf, vs]]}
		, {t, {20, 30, 35}}]
]]]]


(* ::Subsubsection:: *)
(*importFrame[video_, frame_]*)


(* ::Text:: *)
(*importFrame display a frame at a given image size*)
(*video = file name (file name string)*)
(*frame = frame number (int)*)
(*is = image size (int width or Small, Medium, Large, Full)*)
(*OUTPUT: image*)
(*NEEDS: *)


importFrame[video_, frame_]:=Module[{},
	If[FileExistsQ[video]
		,Import[video,{"Frames",{frame}}]
		,"" 
	]
]


(* ::Subsubsection:: *)
(*compareSpeedFrame[frame_, vs_, vf_, is_]*)


(* ::Text:: *)
(*compareSpeedFrame compare equivalent frames and equivalent speed combinations for all tests*)
(*frame = frame number (int)*)
(*vf = flow speed (int)*)
(*vs = stage speed (string)*)
(*is = image size (int width or Small, Medium, Large, Full)*)
(*OUTPUT: grid of images*)
(*NEEDS: compareFXN, importFrame*)


compareSpeedFrame[frame_, vs_, vf_, is_]:=compareFXN[vs, vf, Show[importFrame[#, frame], ImageSize->is]&]


(* ::Subsubsection:: *)
(*getPointsFromBW[frame_]*)


(* ::Text:: *)
(*getPointsFromBW prints out a list of white point coordinates from a binary image*)
(*frame = image*)
(*OUTPUT: list of points {x,y}*)
(*NEEDS:*)


getPointsFromBW[frame_]:=Module[{id, t},
	id = ImageData[frame];
(*	t = DeleteCases[Flatten[Table[If[id[[i,j]]>0,{j,Dimensions[id][[1]]-i},0], {i, Dimensions[id][[1]]},{j,Dimensions[id][[2]]}],1],0];*)
	t = Select[Reverse/@Keys[ArrayRules[SparseArray[Reverse[id]]]], NumberQ[#[[1]]]&&NumberQ[#[[2]]]&]]


(* ::Subsubsection:: *)
(*adjustFrams[video_, frames_]*)


(* ::Text:: *)
(*adjustFrams determines what frames to collect*)
(*video = file name*)
(*frames = list of frames (list of ints or {} for automatic selection)*)
(*OUTPUT: list of frame numbers (list of ints)*)
(*NEEDS: *)


adjustFrams[video_, frames_]:=Module[{firstfram, lastfram, framspacing, numframes, frams},
	numframes = Import[video, "FrameCount"];
	If[Length[frames]<1
		,firstfram = 1000;
		 lastfram = Floor[numframes, 1000];
		 framspacing = Floor[(lastfram-firstfram)/6];
		 frams = Range[firstfram, lastfram, framspacing];
		,frams = Select[frames, #<=numframes&];		 
	];
	frams
	]


(* ::Subsubsection::Closed:: *)
(*averageFrames[video_, frames_, fxn_]*)


(* ::Text:: *)
(*averageFrames adds several frames, given a function*)
(*video = file name (string)*)
(*frames = list of frames or {}*)
(*fxn = a function to use on an image*)
(*OUTPUT: an image*)
(*NEEDS: adjustFrams*)


averageFrames[video_, frames_, fxn_]:=ImageAdd[fxn/@(importFrame[video, #]&/@adjustFrams[video, frames])]


averageEdges[video_, frames_]:=averageFrames[video, frames, EdgeDetect[importFrame[video, #]]&]


(* ::Subsubsection:: *)
(*crop2nozzle[frame_, noz_, padding_]*)


(* ::Text:: *)
(*crop an image to just include the nozzle and its reflection*)
(*frame = image*)
(*noz = nozzle coordinates {{x,y}, dx/dy}*)
(*OUTPUT: image*)
(*NEEDS:*)


crop2nozzle[frame_, noz_, padding_]:=ImageTake[frame, {0, noz[[1,1,2]]}, {noz[[1,1,1]] - padding, noz[[2,1,1]]+padding}]


(* ::Subsubsection:: *)
(*imageSum[image_]*)


imageSum[image_]:=Total[Total[ImageData[image]]];


(* ::Subsection:: *)
(*file name conversions*)


tovideo[f_]:=StringReplace[f, {"grasHeights"->"gras", "grasGeometry"->"gras", "grasSubs"->"gras", "grasSubsRaw"->"gras", "Raw"->"", ".tiff"->".avi", ".csv"->".avi"}]


subsrawfile[video_]:=Module[{f},
f = StringReplace[video, {"gras"->"grasSubsRaw", "avi"->"csv"}];
If[FileExistsQ[f], Import[f], {}]]


grasHImage[video_]:=Module[{f},
f = StringReplace[video, {"gras"->"grasHeights", ".avi"->".tiff"}];
If[FileExistsQ[f], Import[f], {}]]


grasGeoFile[video_]:=Module[{f},
f = StringReplace[video, {"gras"->"grasGeo", ".avi"->".csv"}];
If[FileExistsQ[f], Import[f], {}]]


grasGeoAssoc[video_]:=Module[{f},
f = StringReplace[video, {"gras"->"grasGeoA", ".avi"->".txt"}];
If[FileExistsQ[f], ToExpression[Import[f]], {}]]


grasHeightsFile[video_]:=Module[{f},
f = StringReplace[video, {"gras"->"grasHeights", ".avi"->".csv"}];
If[FileExistsQ[f], ToExpression[Import[f]], {}]]


grasMeans[video_]:=Module[{f},
f = StringReplace[video, {"gras"->"grasMeans", "avi"->"csv"}];
If[FileExistsQ[f], Import[f], {}]]


extractTimeStamp[video_]:=ToExpression[StringSplit[FileNameTake[video], {"_", ".avi"}][[-1]]]


videoSequence[video_]:=Module[{f},
f = StringReplace[video, {"gras"->"grasPositions", ".avi"->".csv"}];
If[FileExistsQ[f], Import[f], {}]];


grasDropletsFile[video_]:=Module[{f},
f = StringReplace[video, {"gras"->"grasDroplets", ".avi"->".csv"}];
If[FileExistsQ[f], Import[f], {}]];


line2video[line_]:= StringReplace[line, {"line1"->"", "line2"->"", "line3"->"", "line4"->"", "line5"->"", "line6"->"", "line7"->"", "line8"->"", "line9"->"", "Positions2_"->"", "csv"->"avi"}]


(* ::Subsubsection:: *)
(*grasGeo2Assoc*)


grasGeo2Assoc[stats_]:=Module[{video, noz, be, firstmid, criticalPoints, ppmm, substrate, beginnings, ends, tegdma, h, nozcoat, subcoat, vf, speeds, fr, numframes},
video = stats[[1,1]];
noz = ToExpression[stats[[2]]];
be = ToExpression[stats[[3]]];
firstmid = ToExpression[stats[[4,1]]];
criticalPoints = ToExpression[stats[[5]]];
ppmm = ToExpression[stats[[6,1]]];
substrate = ToExpression[stats[[7]]][[1]];
beginnings = ToExpression[stats[[8]]];
ends = ToExpression[stats[[9]]];
tegdma = ToExpression[stats[[10]]][[1]];
h = ToExpression[stats[[11]]][[1]];
nozcoat = stats[[12,1]];
subcoat = stats[[13,1]];
vf = ToExpression[stats[[14,1]]];
speeds = ToExpression[stats[[15]]][[;;, 1]];
fr = ToExpression[stats[[16]]][[1]];
numframes = Import[video, "FrameCount"];
<|"video"->video
	, "noz"->noz
	, "be"->be
	, "firstmid"->firstmid
	, "criticalPoints"->criticalPoints
	, "ppmm"->ppmm
	, "substrate"->substrate
	, "beginnings"->beginnings
	, "ends"->ends
	, "tegdma"->tegdma
	, "h"->h
	, "nozcoat"->nozcoat
	, "subcoat"->subcoat
	, "vf"->vf
	, "speeds"->speeds
	, "fr"->fr
	, "numframes"->numframes
	|>
]


(* ::Subsubsection:: *)
(*grasPlot3D*)


grasPlot3D[video_, vp_]:=Module[{gga, ghf, stp},
gga = grasGeoAssoc[video]["speeds"];
If[Length[gga]>0, 
	gga = Transpose[{gga}]; 
	ghf = grasHeightsFile[video];
	If[Length[ghf]>0,
		stp = subtableplot2[gga,ghf];
		Print[stp[[3]]];
		Show[stp[[1,1,1,1,1]], ViewPoint->vp, BoxRatios->{90, 65, 40}]
	]
]
]
