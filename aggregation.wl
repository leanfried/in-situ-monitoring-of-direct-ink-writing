(* ::Package:: *)

dir = NotebookDirectory[];
file = FileNameJoin[{dir, "extrusionBehaviors.wl"}];
If[FileExistsQ[file], Import[file]];
Clear[dir, file];


THISDIRECTORY = "C:\\Users\\Leanne\\Box Sync\\data\\Shear droplets 2\\";


INTERESTINGLIST = {6,12,13,14,20,30,33,39,44,46,48,49,50, 51};


expand[range_, f_]:=Module[{r}, r = range[[2]] - range[[1]]; {range[[1]] - r*f, range[[2]] +r*f}];


IMPORTANTLIST = {6,12,13,14,15,19,20,21,30,33,40, 41, 42, 43, 44, 45, 46,47,48,49,50, 51};


SMHEADER3 = Join[
			{"frame #", "time (s)", "h (mm)"}
			, Flatten[Table[Row[{Subscript["d",k<>","<>i<>j],"(mm)"}], {i, {"u", "d"}}, {j, {"r", "l", "m"}}, {k, {"noz", "sub", "Si"}}]]
			, Flatten[Table[Row[{i,j," ",k}], {i, {"u", "d"}}, {j, {"r", "l", "m"}}, {k, {"r1 (mm)", "r2 (mm)", Row[{Subscript["\[CapitalDelta]P","L"]," (Pa)"}]}}]]									
			, Flatten[Table[Row[{Subscript["\[CapitalDelta]P", "L"<>i<>","<>j[[1]]<>""<>j[[2]]]," (Pa)"}], {i, {"u", "d"}}, {j,  Subsets[{"r", "l", "m"}, {2}]}]]
			, Row[{Subscript["\[Theta]",#],"(\[Degree])"}]&/@{"SL", "BL", "SiL"}
			, Row[{Subscript["l", #],"(mm)"}]&/@{"SL", "SiL"}
			, {Row[{Subscript["\[CapitalDelta]P", "L,ud"], " (Pa)"}]}
		];


standardError[data_]:=If[Length[data]>1,N[StandardDeviation[data]/Sqrt[Length[data]]],0]


columnIndex[v_]:=Transpose[{Range[Length[v]], v}]


(* ::Section:: *)
(*Qualitative fits*)


(* ::Subsubsection:: *)
(*initializeQualitativeFits*)


(* ::Text:: *)
(*import the qualitative fit spreadsheet*)
(*OUTPUT: initializes a global variable MAPS which is an association between the 1st four variables and the *)


initializeQualitativeFits:=Module[{m},
m = Import[THISDIRECTORY<>"morphologies.xlsx"][[1]];
MAPS = GroupBy[m, #[[1;;4]]&];
]


plotmap[combo_, forceblack_, fs_]:=Module[{grouped, morphs, mptemp},
	mptemp = MAPS[combo];
	Graphics[Text[Style[StringReplace[#[[7]], {"O"->"F", "B"->"F", "U"->"D", "M"->"D"}], Directive[fs, Black]], #[[{6,5}]]]&/@Select[mptemp, #[[6]]>0&]]
]


(* ::Text:: *)
(*gkey = {int tegdma, int h, string noz, string sub}*)
(*mkey = {int. tegdma, string sub, string noz, int. h}*)


gkey2mkey[gkey_]:={If[NumberQ[gkey[[1]]]
						, gkey[[1]]*1.
						, If[NumberQ[ToExpression[gkey[[1]]]]
							, ToExpression[gkey[[1]]]*1.
							, ToExpression[StringTake[gkey[[1]],2;;]]*1.
						]
					]
				, gkey[[4]]
				, If[gkey[[3]]=="FDTS", "FDTS", 0.]
				, If[NumberQ[gkey[[2]]], gkey[[2]]*1., If[NumberQ[ToExpression[gkey[[2]]]], ToExpression[gkey[[2]]]*1., ToExpression[StringTake[gkey[[2]],2;;]]*1.]]}


stats2mkey[stats_]:={1.*stats["tegdma"], stats["subcoat"], If[NumberQ[#], 0.,#]&@stats["nozcoat"], 1.*stats["h"]}


deps2qual[{teg_, h_, noz_, sub_, vf_, vs_}]:=StringReplace[Select[MAPS[gkey2mkey[{teg, h, noz, sub}]], #[[6]]==vs && #[[5]]==vf&][[1, -1]], {"B"->"F", "O"->"F", "M"->"D", "U"->"D"}]


(* ::Section:: *)
(*summarizing videos*)


FOTPHEADER = {"tegdma", "h (um)", "noz", "sub", "vf (mm/s)", "column", "dep var", "index", "vs (mm/s)"
				, "Period (s)", "StdEr period (s)", "Period (mm)", "StdEr period (mm)"
				, "Top value", "StdEr top value", "Bot value", "StdEr bot value"
				, "Amplitude", "StdEr amplitude"};


(* ::Subsubsection:: *)
(*removeOutliers[line_, column_]*)


(* ::Text:: *)
(*take a line from a list of 9 lines and a column from that table to get a time/dependent variable list*)
(*OUTPUT: {list, mean of dependent variable, standard error of dependent variable}*)
(*NEEDS: *)


removeOutliers2[xylist_, col_]:=Module[{l, meddiff, yvals, ypos, t1, l1, amplitude, l1a, l2, l3, l4, l2a},Catch[
l = Select[xylist[[;;,{2, col}]], NumberQ[#[[1]]]&&NumberQ[#[[2]]]&];
If[Length[l]>5,
	yvals = SortBy[Gather[l[[;;,2]]], #[[1]]&];
	If[Length[yvals]>5,
		(*bottom edge*)
		t1 = Table[{yvals[[i,1]], yvals[[i+1,1]] - yvals[[i,1]], yvals[[-1,1]] - yvals[[i,1]], Length[yvals[[i]]]}, {i, Min[20, Round[Length[yvals]*0.2]]}];
		ypos = Select[t1, #[[2]]>0.3*#[[3]] && #[[4]]<10&][[;;,1]];
		If[Length[ypos]>0,
			l = Select[l, #[[2]]>=Max[ypos]&];
		];
		(*top edge*)
		t1 = Table[{yvals[[i,1]], yvals[[i+1,1]] - yvals[[i,1]], yvals[[i,1]]-yvals[[1,1]], Length[yvals[[i]]]}, {i, Max[Length[yvals]-20, Round[Length[yvals]*0.8]], Length[yvals]-1}];
		ypos = Select[t1, #[[2]]>0.3*#[[3]] && #[[4]]<10&][[;;,1]];
		If[Length[ypos]>0,
			l = Select[l, #[[2]]<=Min[ypos]&];
		];
	];
];
l = Table[{l[[i]], l[[i,2]] - l[[i-1,2]], l[[i+1,2]]-l[[i,2]]}, {i, 2, Length[l]-1}];
meddiff = 6*Median[Abs[l[[Round[Length[l]/2];;,2]]]];
l4 = Select[l, !(Abs[#[[2]]]>meddiff && Abs[#[[3]]]>meddiff) &][[;;,1]];
If[col==46, l4 = Select[l4, #[[2]]<180&]];
If[col==49, l4 = Select[l4, -1.5<#[[2]]<0.5&]];
If[col==51, l4 = Select[l4, #[[2]]<700&]];
Return[l4]
(*l1 = Select[xylist[[;;,{1, 2, col}]], NumberQ[#[[1]]]&&NumberQ[#[[3]]]&];
If[Length[l1]<=5, Throw[l1[[;;, {2,3}]]]];
amplitude = Max[l1[[;;,3]]] - Min[l1[[;;,3]]];
l1a = Select[Transpose[{l1[[2;;,1]], Differences[l1[[;;,3]]], l1[[2;;, 3]]}], Abs[#[[2]]]>0.2*amplitude&];
If[Length[l1a]<3, Throw[l1[[;;, {2,3}]]]];
l2 =  Split[l1a, #2[[1]] - #1[[1]]<6&];
l2a = Select[l2, Length[#]>1&];
If[Length[l2a]<1, Throw[l1[[;;, {2,3}]]]];
l3 = {Min[#[[;;,1]]]-1, Max[#[[;;,1]]]}&/@l2a;
l4 = Complement[l1, Flatten[Table[Select[l1, l3[[i,1]]<#[[1]]<l3[[i,2]]&], {i, Length[l3]}],1]][[;;, {2,3}]];
Return[l4];*)
]]


(* ::Subsubsection::Closed:: *)
(*fotpPrint*)


(* ::Text:: *)
(*print diagnostics*)


fotpPrint[lslline_, toppeaksl_, botpeaksl_, returnpositionl_, endpositionl_, index_, column_,  error_]:=Module[{xrange},
xrange = MinMax[lslline[[;;,1]]];
	Print[index, "\t", column, "\t", 
				ListLinePlot[lslline
					, Epilog->{Red
								, PointSize[0.005]
								, If[Length[toppeaksl]>0, Point[toppeaksl], {}]
								, Darker[Cyan]
								, If[Length[botpeaksl]>0, Point[botpeaksl], {}]
								}
					, Prolog->{Pink
								,If[Length[returnpositionl]>0,  Line[{{xrange[[1]], returnpositionl[[1]]}, {xrange[[2]], returnpositionl[[1]]}}], {}]
								, Cyan
								,If[Length[endpositionl]>0,  Line[{{xrange[[1]], endpositionl[[1]]}, {xrange[[2]], endpositionl[[1]]}}], {}]
								}
					, Joined->True
					, AspectRatio->1/20
					, ImageSize->1400
					, PlotMarkers->Automatic
					, PlotStyle->Black
					, ImagePadding->{{100, 10}, {40, 10}}
					, FrameLabel->{"Time (s)", SMHEADER3[[column]]}
					, RotateLabel->False
					, PlotRange->All
				]
			,"\t"
			,error
	]
];


(* ::Subsubsection:: *)
(*constructErrorRet[gkey_, column_, index_, speed_, lslline_]*)


(* ::Text:: *)
(*construct an error value to return*)
(*gkey = {tegdma, h, noz, sub, vf}*)
(*column = column number (int 4 to 50)*)
(*index = line index (int 1 to 9)*)
(*speed = stage speed in mm/s*)
(*lsllline = list of {time, dep}*)
(*OUTPUT: list of values*)
(*NEEDS: standardError*)


constructErrorRet[gkey_, column_, index_, speed_, lslline_]:=Module[{errorRet},
errorRet = ConstantArray["", Length[FOTPHEADER]];
errorRet[[1;;5]] = gkey;
errorRet[[6;;9]] = {column, SMHEADER2[[column]], index, speed};
If[Length[lslline]>1,
	errorRet[[14]] = Median[lslline[[;;,2]]];
	errorRet[[15]] = standardError[lslline[[;;,2]]];
];
errorRet
]


(* ::Subsubsection:: *)
(*minmaxpeaks[list_]*)


(* ::Text:: *)
(*finds peak point*)
(*list = list of {time, dep} points*)
(*OUTPUT: one {time, dep} point*)
(*NEEDS:*)


minmaxpeaks[list_]:=Module[{min, max}, 
	If[Length[list]>0,
		max = MaximalBy[list, #[[2]]&][[1]]; 
		If[max==list[[1]]|| max==list[[-1]] , max = ""]; 
		max
		,
		""
	]
	]


(* ::Subsubsection:: *)
(*thepeaks[min_, max_, shift_, period_, lslline_]*)


(* ::Text:: *)
(*finds a list of top peaks*)
(*min = min time value*)
(*max = max time value*)
(*shift = time shift for range*)
(*period = spacing between time values*)
(*lslline = list of points {time, dep}*)
(*OUTPUT: table of points*)
(*NEEDS: minmaxpeaks*)


thepeaks[min_, max_, shift_, period_, lslline_]:=Module[{xrange},
xrange = Range[min-shift, max-shift, period];
Table[minmaxpeaks[Select[lslline, xrange[[i]]<=#[[1]]<xrange[[i+1]]&]], {i, Length[xrange]-1}]]


(* ::Subsubsection:: *)
(*speakstop[min_,  max_, fperiod_, lslline_, shiftlist_]*)


(* ::Text:: *)
(*breaks lslline into fperiod sized segments, finds min and max, repeats with shifted breaking points, and takes the union*)
(*min = time min*)
(*max = time max*)
(*fperiod = time period*)
(*lslline = list of points {time, dep}*)
(*shiftlist = list of periods to shift by*)
(*OUTPUT: union of all shifted lists*)
(*NEEDS: thepeaks*)


speakstop[min_,  max_, fperiod_, lslline_, shiftlist_]:=Select[Union@@(
						Table[thepeaks[min, max+fperiod, i, fperiod, lslline]
							, {i, shiftlist}
						]
					), Length[#]==2&];


(* ::Subsubsection:: *)
(*peakset[peakstop_, i_, lslline_]*)


(* ::Text:: *)
(*find a set of values for a peak*)
(*peakstop = list of top peaks in {time, dep} coords*)
(*i = peak index to probe (int)*)
(*lslline = list of initial {time, dep} point*)
(*OUTPUT: {bottom point after top point point, top point, period, amplitude}*)
(*NEEDS:*)


peakset[peakstop_, i_, lslline_]:=Module[{l1, bot},
l1 = Select[lslline, peakstop[[i,1]]<#[[1]]<peakstop[[i+1,1]]&];
If[Length[l1]>0,
	bot = MinimalBy[l1, #[[2]]&][[1]];
	{bot, peakstop[[i]], peakstop[[i+1,1]]-peakstop[[i,1]], peakstop[[i,2]] - bot[[2]]}
	,
	{}
]]


(* ::Subsubsection:: *)
(*periodsamps[peakstop_, lslline_]*)


(* ::Text:: *)
(*peaks, amplitude, and period*)
(*peakstop: list of peaks*)
(*lslline = list of points {time, dep}*)
(*OUTPUT: list of top peaks, list of bottom peaks, amplitude of peaks, period of peaks*)
(*NEEDS: peakset*)


periodsamps[peakstop_, lslline_]:=Module[{periods, topbotpeaks, maxamp, period, botpeaks, toppeaks, amplitude, errorRet},
Catch[
errorRet = {{},{},{}, {1,10}};
topbotpeaks = Select[Table[peakset[peakstop, i, lslline], {i, Length[peakstop]-1}], Length[#]>0&];
If[Length[topbotpeaks]<3, Throw[errorRet]];
maxamp = RankedMax[topbotpeaks[[;;, 4]], 3];
topbotpeaks = Select[topbotpeaks, #[[4]]>0.5*maxamp&];
If[Length[topbotpeaks]<3, Throw[errorRet]];
periods = Differences[topbotpeaks[[;;,1,1]]];
period = {Median[#], standardError[#]}&@periods;
botpeaks = topbotpeaks[[;;,1]];
toppeaks = topbotpeaks[[;;,2]];
amplitude = {Median[#], standardError[#]}&@((#[[2, 2]] - #[[1,2]])&/@topbotpeaks);
{toppeaks, botpeaks, amplitude, period} 
]]


(* ::Subsubsection:: *)
(*flipthisrow[col_]*)


(* ::Text:: *)
(*does this column decrease with the contact line position?*)
(*col = column number (int)*)
(*OUTPUT: bool*)
(*NEEDS:*)


flipthisrow[col_]:=MemberQ[{5,8,10,12, 16, 17, 20, 22, 25, 30, 36, 37, 41, 41, 44, 45, 46, 51}, col]


(* ::Subsubsection:: *)
(*jumpeaks[lslline_, col_]*)


(* ::Text:: *)
(*detects a list of points where the y value jumps by a lot*)
(*lslline = list of {time, dep} points*)
(*col = column number of the dependent variable*)
(*OUTPUT: list of split points in {time, dep}*)
(*NEEDS: flipthisrow*)


jumpeaks[lslline_, col_]:=Module[{jump, maxjump, splits, add},Catch[
	add = If[flipthisrow[col], 0, 1];
	jump = Table[{lslline[[i+add,1]], lslline[[i+add,2]], Abs[lslline[[i+1,2]] - lslline[[i,2]]]}, {i, Length[lslline]-1}];
	maxjump = Max[Abs[jump[[;;,3]]]];
	If[maxjump<=5*Min[Select[Abs[jump[[;;,3]]], #>0&]], Throw[{}]];
	splits = Select[jump, #[[3]]>0.6*maxjump&];
	If[Length[splits]>0
		, splits[[;;,1;;2]]
		, {}
	]
]]


(* ::Subsubsection:: *)
(*extractPeaks[lslline_, fperiod_, errorRet_]*)


(* ::Text:: *)
(*gets a list of peaks and appropriate values*)
(*lslline = list of points {time, dep}*)
(*fperiod = time period in seconds*)
(*errorRet = value to return if there is an error*)
(*OUTPUT: {list of top peaks, list of bot peaks, {median and std error amplitude}, {median and standard error of period}}*)
(*NEEDS: speakstop, jumpeaks, periodsamps*)


extractPeaks[lslline_, fperiod_, col_]:=Module[{xrange, peaks1, peaks2, peaksbot, peakstop, periods, bopbotpeaks, topbotpeaks, maxamp, toppeaks, botpeaks, amplitude, period, peaks3, min, max, numpeaks, errorRet},
Catch[
errorRet = {{},{},{},{}};
If[Length[lslline]<10, 
	Throw[errorRet]
];
min = Min[lslline[[;;,1]]];
max = Max[lslline[[;;,1]]];
If[min<0, 
	Throw[errorRet]
];
numpeaks =(* 0.75*(max - min)/fperiod*)3;
peakstop = speakstop[min, max, fperiod, lslline, {0, fperiod/2}];
If[Length[peakstop]<numpeaks,
	peakstop = Union[peakstop, jumpeaks[lslline, col]];
	If[Length[peakstop]<numpeaks,
		errorRet[[1]] = peakstop;
		Throw[errorRet]
	];
];
{toppeaks, botpeaks, amplitude, period}  = periodsamps[peakstop, lslline];
If[Length[amplitude]<2 && Length[peakstop]>=numpeaks,
	peakstop = Union[peakstop, jumpeaks[lslline, col]];
	{toppeaks, botpeaks, amplitude, period}  = periodsamps[peakstop, lslline];
];
{toppeaks, botpeaks, amplitude, period} 
]]



(* ::Subsubsection:: *)
(*msepeaks[botpeaks_, amplitude_]*)


(* ::Text:: *)
(*mean and standard error of peaks*)
(*botpeaks = list of peaks*)
(*amplitude = amplitude *)
(*OUTPUT: mean and standard error of peak values*)
(*NEEDS: standardError*)


msepeaks[botpeaks_, amplitude_]:=Module[{botposition, margin, botpeaks2},
margin = 6;
botposition = {Median[#], standardError[#]}&@botpeaks[[;;,2]];
(*If[botposition[[2]] > 0.3 * amplitude[[2]] && Length[botpeaks]>4,
	botpeaks2 = Select[botpeaks, botposition[[1]] - margin*botposition[[2]] <= #[[2]] \[LessEqual] botposition[[1]] + margin*botposition[[2]]&];
	If[Length[botpeaks2]\[GreaterEqual]3
		,
		botposition = {Median[#], standardError[#]}&@botpeaks2[[;;,2]];
		,
		botpeaks2 = botpeaks;
	];
];*)
botposition
];


(* ::Subsubsection:: *)
(*fotprow[lslline_, fperiod_, gkey_, column_, index_, speed_, diag_]*)


(* ::Text:: *)
(*one row in filloutthetimeplot*)
(*lslline = list of points {time, dep}*)
(*fperiod = time period in seconds*)
(*gkey = list of stats for the video*)
(*column = index of column we're probing (1 to 51)*)
(*speed = stage speed in mm/s*)
(*diag = true to print diagnostics*)
(*OUTPUT: list of parameters*)
(*NEEDS: extractPeaks, fotpPrint, constructErrorRet, msepeaks*)


fotprow[lslline_, fperiod_, gkey_, column_, index_, speed_, diag_]:=Module[{toppeaks, botpeaks, amplitude, period, botposition, topposition, margin, botpeaks2},
Catch[
{toppeaks, botpeaks, amplitude, period} = extractPeaks[lslline, fperiod, column];
If[Length[amplitude]<2, 
	If[diag, fotpPrint[lslline, toppeaks, botpeaks, {}, {}, index, column, "insufficient peaks"]]; 
	Throw[constructErrorRet[gkey, column, index, speed, lslline]]
];
botposition = msepeaks[botpeaks, amplitude];
topposition = msepeaks[toppeaks, amplitude];

If[diag, fotpPrint[lslline, toppeaks, botpeaks, topposition, botposition, index, column, ""]];
Flatten[{gkey, {column, SMHEADER2[[column]], index, speed}, period, period*speed, topposition, botposition, amplitude}]]
]


(* ::Subsubsection:: *)
(*adjsumrow[row_]*)


(* ::Text:: *)
(*adjust a row to flip the top and bottom values given a certain dep variable*)
(*row: results from fillOutTheTimePlot*)
(*OUTPUT: row*)
(*NEEDS: flipthisrow*)


adjsumrow[row_]:=Module[{h, tb},
If[flipthisrow[row[[6]]],
h = row;
tb = {row[[14;;15]], row[[16;;17]]};
If[tb[[1,1]]<tb[[2,1]],
	h[[14;;15]] = tb[[1]];
	h[[16;;17]] = tb[[2]];
,
	h[[14;;15]] = tb[[2]];
	h[[16;;17]] = tb[[1]];
];
h
,
row
]]


(* ::Subsubsection:: *)
(*shorten[l_, f_]*)


(* ::Text:: *)
(*shorten a list l by f1 on the left side and f2 on the right side, where f={f1,f2} and f1 and f2 are fractions*)


shorten[l_, f_]:=l[[Max[Round[f[[1]]*Length[l]], 1];;Min[Floor[f[[2]]*Length[l]], Length[l]]]]


(* ::Subsubsection:: *)
(*fourTime[interpolatedline_, spacing_, lslline_, index_, column_, spacing_, diag_, errorRet_]*)


(* ::Text:: *)
(*finds the periodicity in time using a fourier transform*)
(*interpolatedline = list of {time, dep} points*)
(*spacing = time spacing between points*)
(*lslline = original list of {time, dep} points*)
(*index = line index (int 1 to 9)*)
(*column = column index (int 4 to 50)*)
(*diag = bool true to print*)
(*errorRet = value to return in case of error*)
(*OUTPUT: double time period (s)*)
(*NEEDS: shorten*)


fourTime[interpolatedline_, spacing_, diag_]:=Module[{four, fourmax, p, i, fm, baseline, fp},
Catch[
four = Abs[Fourier[interpolatedline[[;;,2]]]];
	(*fourier transform of dep*)

p = (*Round[Length[interpolatedline]/4];*)Round[2/spacing];
baseline = Mean[shorten[four, {0.25, 0.5}]];
i = Min[20, FirstPosition[four, z_/;z<3*baseline][[1]]];
(*i = 5;*)
If[diag, Print[Row[{
	spacing,"\t",
	ListPlot[{(*Transpose[{Range[1, Length[interpolatedline]], four}], *)Transpose[{spacing*Range[i, p], four[[i;;p]]}]}, ImageSize->600, AspectRatio->1/6, Joined->True, PlotRange->All]
	, ListPlot[interpolatedline, ImageSize->600, AspectRatio->1/6, Joined->True, PlotRange->All]
	}]
]];
(*fm = Max[four[[i;;p]]];
fourmax = {Position[four, fm][[1,1]], fm};
Print[fourmax];
If[fourmax[[1]]\[LessEqual]i+5, 
	Throw[-1]
];*)
fp = FindPeaks[four[[i;;p]],Padding->2];
fourmax = MaximalBy[fp, #[[2]]&][[1]];
If[fourmax[[2]] < 3*Mean[four[[i;;p]]] || fourmax[[2]]<1.5*RankedMax[fp[[;;,2]], 2], 
	Throw[0]
];
(fourmax[[1]]+i)*spacing
]]


(* ::Subsubsection:: *)
(*fillInInterpolation[lslline_, index_, column_, diag_, errorRet_]*)


(* ::Text:: *)
(*take a list and construct an interpolated list for use with fourier and findpeaks*)
(*lsllline = {time, dep} list*)
(*index = index in list (int 1 to 9) *)
(*column = column numer (int 4 to 50)*)
(*diag = bool true to print*)
(*errorRet = value to return in case of error*)
(*OUTPUT: {list of points, time spacing between points (double)}*)
(*NEEDS:*)


fillInInterpolation[lslline_]:=Module[{selectedl, spacing, interpl, domains, therange, interpolatedline},Catch[
selectedl = GatherBy[lslline, #[[1]]&][[;;,1]];
	(*remove duplicate x values*)
spacing = Min[Differences[selectedl[[;;,1]]]]; 
	(*time spacing*)
If[Length[Split[lslline, #2[[1]]-#1[[1]]<spacing*2.1&]]>0.2*Length[lslline],
	Throw[{{}, spacing}]
];
interpl = Interpolation[selectedl];
	(*interpolate time v. dep*)
domains = interpl["Domain"][[1]];
	(*min/max value of time*)
therange = Range[domains[[1]], domains[[2]], spacing];
interpolatedline = Table[{x,interpl[x]}, {x, therange}];
	(*fill in any gaps for fourier and findpeaks*)
{interpolatedline, spacing}
]]


(* ::Subsubsection:: *)
(*fillOutTheTimePlot*)


(* ::Text:: *)
(*fillOutTheTimePlot finds periodicity within one line in a dependent variable*)
(*lslline = list of 9 lists of {time, dep}*)
(*speeds = list of 9 speeds*)
(*index = line index to probe*)
(*column = dependent variable column number to probe*)
(*gkey = {tegdma, h, noz, sub, vf}*)
(*diag = true to print*)
(*OUTPUT: list of list of parameters*)
(*NEEDS: constructErrorRet, removeOutliers2, fotpPrint, deps2qual, fillInInterpolation, jumpeaks, fourTime, adjsumrow*)


throwError[lsllines_, toppoints_, botpoints_, index_, message_, speed_, gkey_, diaglist_, diag_]:=Module[{errorRet},
errorRet = Table[constructErrorRet[gkey, column, index, speed, removeOutliers2[lsllines[[index]], column]], {column, If[Length[diaglist]==0, Range[4, Length[SMHEADER2]], diaglist]}];
If[diag, fotpPrint[removeOutliers2[lsllines[[index]], 49], toppoints, botpoints, {}, {}, index, 49, message]]; 
errorRet
]


fillOutTheTimePlot[lsllines_, speeds_, index_, gkey_, diaglist_]:=Module[{lslline, speed, interpolatedline, spacing, fperiod, diag
																			, toppeaks, botpeaks, amplitude,  botposition, topposition
																			, mm, splits, st, morph},
Catch[
diag = If[Length[diaglist]>0, True, False];
speed = speeds[[index]]; 
morph = deps2qual[Join[gkey, {speed}]];
If[morph=="F",
	Throw@throwError[lsllines, {}, {}, index, "filament", speed, gkey, diaglist, diag]; 
];
(*lslline = Select[removeOutliers2[lsllines[[index]], 49], #[[2]]<0.5&];*)
If[Length[lslline]<50, 
	Throw@throwError[lsllines, {}, {}, index, "<50 points", speed, gkey, diaglist, diag]; 
];
{interpolatedline, spacing} = fillInInterpolation[lslline];
If[Length[interpolatedline]<1,
	Throw@throwError[lsllines, {}, {}, index, "interpolation failed", speed, gkey, diaglist, diag]; 
];
splits = jumpeaks[lslline, 49];
If[Length[splits]<3,
	fperiod = fourTime[interpolatedline, spacing, diag];
	If[fperiod<=0,
		Throw@throwError[lsllines, {}, {}, index, "period detection failed", speed, gkey, diaglist, diag];
	];
	,
	{fperiod, st} = {Median[#], standardError[#]}&@Table[splits[[i+1,1]]-splits[[i,1]], {i, Length[splits]-1}];
];
If[diag, Print[fperiod, " s", "\t", st]];

Table[adjsumrow[fotprow[removeOutliers2[shorten[lsllines[[index]], {0, 1}], column], fperiod, gkey, column, index, speed, diag]], {column, If[diag, diaglist, Range[4,Length[SMHEADER2]]]}]
]]


(* ::Subsubsection:: *)
(*exportPeriods[video_]*)


(* ::Text:: *)
(*export periodicity values and means for a video*)
(*video = file name (string)*)
(*OUTPUT: exports a table of values and prints its name*)
(*NEEDS: grasGeoAssoc, fillOutTheTimePlot*)


exportPeriods[video_]:=Module[{thistable, ta, stats, table, gkey, newfilename},
thistable = FileNames[StringReplace[FileNameTake[video], {"gras"->"grasPositions3_line*", "avi"->"csv"}], "*", Infinity];
newfilename = StringReplace[video, {"gras"->"grasSummary4", "avi"->"csv"}];
If[Length[thistable]==9 && !FileExistsQ[newfilename],
	ta = Import/@thistable;
	stats = grasGeoAssoc[video];
	gkey = stats/@{"tegdma", "h", "nozcoat", "subcoat", "vf"};
	table = Flatten[Table[
						fillOutTheTimePlot[ta, stats["speeds"], i, gkey, {}]
						,{i, 9}
					],1];
	Export[newfilename, Prepend[table, FOTPHEADER]];
	Print[newfilename];
];
]


(* ::Subsubsection:: *)
(*periodOneLine[video_, line_, indexprints_]*)


(* ::Text:: *)
(*just detect the period for one line*)
(*video = file name (avi)*)
(*line = line number (1 to 9)*)
(*indexprints = list of columns to print (1 to 51)*)
(*OUTPUT: grid with data*)
(*NEEDS: grasGeoAssoc, fillOutTheTimePlot*)


periodOneLine[video_, line_, indexprints_]:=Module[{thistable, ta, stats, gkey},
thistable = FileNames[StringReplace[FileNameTake[video], {"gras"->"grasPositions3_line"<>ToString[line], "avi"->"csv"}], "*", Infinity];
ta = Import[thistable[[1]]];
stats = grasGeoAssoc[video];
gkey = stats/@{"tegdma", "h", "nozcoat", "subcoat", "vf"};
Grid[Prepend[fillOutTheTimePlot[{ta}, {stats["speeds"][[line]]}, 1, gkey, indexprints], FOTPHEADER]]	
]


(* ::Section:: *)
(*plotting summarized results*)


(* ::Subsection:: *)
(*plotting tools*)


(* ::Subsubsection:: *)
(*per[x_, y_, xer_, yer_, pr_, w_]*)


(* ::Text:: *)
(*plot with error bars*)


per[x_, y_, xer_, yer_, pr_, w_]:=Module[{top, bot, left, right, output},
output = {Point[{x,y}]};
If[yer>0,
	top = y+yer; bot = y-yer; left = x -( pr[[1,2]] - pr[[1,1]])*w; right = x+( pr[[1,2]] - pr[[1,1]])*w;
	output = Join[output, {Line[{{left, top}, {right, top}}], Line[{{left, bot}, {right, bot}}], Line[{{x, top}, {x, bot}}]}];
];
If[xer>0,
	top = x+xer; bot = x-xer; left = y -( pr[[2,2]] - pr[[2,1]])*w; right = y+( pr[[2,2]] - pr[[2,1]])*w;
	output = Join[output, {Line[{{top, left}, {top, right}}], Line[{{bot, left}, {bot, right}}], Line[{{top, y}, {bot, y}}]}];
];
output
]


(* ::Subsubsection:: *)
(*row2graphics[row_, xcol_, pr_, w_]*)


row2graphics[row_, xcol_, pr_, w_, twophase_]:=If[NumberQ[row[[14]]],
If[NumberQ[row[[16]]]
, {If[twophase, GrayLevel[0.5], {}]
	, per[row[[xcol]], row[[14]],0, row[[15]], pr, w]
	, per[row[[xcol]], row[[16]],0, row[[17]],pr, w]
	, Dashed, Line[{{row[[9]], row[[14]]}, {row[[9]], row[[16]]}}]
	}
, {If[twophase, Black, {}]
	, per[row[[xcol]], row[[14]],0, row[[15]], pr, w]
	}
]
,
{}]


(* ::Subsubsection:: *)
(*row2graphics2[row_, pr_, w_, twophase_]*)


row2graphics2[row_, pr_, w_, twophase_]:=Module[{output, tpcolor},
tpcolor = (*getColors[2][[2]]*)GrayLevel[0.5];
If[NumberQ[row[[1,2]]] && NumberQ[row[[2,2]]]
	,
	output = per[row[[1,2]], row[[2,2]], row[[1,3]], row[[2,3]], pr, w];
	If[NumberQ[row[[1,4]]]
		,
		If[NumberQ[row[[2,4]]]
			, output = Join[If[twophase, {tpcolor}, {}], output, 
						{per[row[[1,4]], row[[2,4]], row[[1,5]], row[[2,5]], pr, w]
						, Dashed
						, Line[{{row[[1,2]], row[[2,2]]}, {row[[1,4]], row[[2,4]]}}]
						}
					];
			, output = Join[If[twophase, {tpcolor}, {}], output, 
						{per[row[[1,4]], row[[2,2]], row[[1,5]], row[[2,3]], pr, w]
						, Dashed
						, Line[{{row[[1,2]], row[[2,2]]}, {row[[1,4]], row[[2,2]]}}]
						}
					];
		];
		,
		If[NumberQ[row[[2,4]]]
			, output = Join[If[twophase, {tpcolor}, {}], output, 
						{per[row[[1,2]], row[[2,4]], row[[1,3]], row[[2,5]], pr, w]
						, Dashed
						, Line[{{row[[1,2]], row[[2,2]]}, {row[[1,2]], row[[2,4]]}}]
						}
					];
		];
		,
		output = {Black, output};
	];
	,
	output ={}
];
output
]


(* ::Subsubsection:: *)
(*plotline[tab_, indep_, pr_, ps_, twophase_]*)


plotline[tab_, indep_, pr_, ps_, twophase_]:=Module[{t, t2}, 
	t = SortBy[tab, #[[indep]]&]; 
	t2 = Select[t, NumberQ[#[[14]]]&];
	t2 = {#[[indep]], If[NumberQ[#[[16]]], Mean[#[[{14, 16}]]], #[[14]]]}&/@t2;
	t2 = Median/@GatherBy[t2, #[[1]]&];
	{
		row2graphics[#, indep, pr, ps/2, twophase]&/@t
		, Line[t2]
	}
]


(* ::Subsubsection:: *)
(*plotline2[tab_, pr_, ps_, twophase_]*)


plotline2[tab_, pr_, ps_, twophase_]:={row2graphics2[#, pr, ps, twophase]&/@tab(*, Line[{{#[[1,2]], #[[2,2]]}&/@SortBy[Select[tab, NumberQ[#[[1,2]]] && NumberQ[#[[2,2]]]&], #[[1,1]]&]}]*)}


(* ::Subsection:: *)
(*plots*)


(* ::Subsubsection:: *)
(*vsplotsplitbyvf*)


(* ::Text:: *)
(*dep = dependent variable index*)
(*map1 = table of summary variables that goes with FOTPHEADER*)
(*ps = point size (e.g. 0.1)*)
(*vflist = list of acceptable flow speeds (list of ints) input {} to use all*)
(*NEEDS: getColors, expand, plotline*)


vsplotsplitbyvf[dep_, map1_, ps_, vflist_, indlist_, twophase_, fs_, iw_, plotrange_]:=Module[{tab, yrange, xrange, pr, tabs, speeds, colors, leg, indep, vfl, mode, diffs, difind},
Catch[
indep = 9;
If[Length[Dimensions[map1]]!=2, mode = 1 (*map mode*), mode = 2 (*vf mode*)];
If[Length[vflist]!=1 && mode==1, Throw[vsplotsplitbyvf[dep, #, ps, vflist, indlist, twophase, fs, iw, plotrange]&/@map1]];
If[Length[vflist]>0, vfl = vflist, vfl = DeleteDuplicates[Switch[mode, 1, Flatten[map1,1], 2, map1][[;;, 5]]]];
tab = Select[#, #[[6]]==dep && MemberQ[indlist, Mod[#[[8]],3]] && MemberQ[vfl, #[[5]]] &]&/@Switch[mode, 1, map1, 2, {map1}];
If[Length[plotrange]<2 || Length[plotrange[[1]]]<2,
yrange = MinMax[Select[Flatten[
						If[NumberQ[#[[14]]]
							, If[NumberQ[#[[16]]]
								, {#[[14]]-#[[15]], #[[14]]+#[[15]],#[[16]]-#[[17]], #[[16]]+#[[17]]}
								, {#[[14]]-#[[15]], #[[14]]+#[[15]]}
								]
						, ""]&/@Flatten[tab,1]
					], NumberQ]];
xrange = MinMax[Flatten[tab,1][[;;, indep]]];
pr = expand[#, 0.1]&/@{xrange, yrange};
,
pr = plotrange
];
tabs = Switch[mode, 1, tab, 2, GatherBy[Flatten[tab,1], #[[5]]&]];
speeds = Switch[mode
	, 1, 
	diffs = tabs[[;;, 1, 1;;4]];
	difind = Select[Position[DeleteDuplicates/@Transpose[diffs], z_/;Length[z]>1], Length[#]>0&][[;;,1]];
	diffs = diffs[[;;, difind]];
	Grid[{Style[#, fs]&/@#}]&/@diffs
	, 2, 
	Style[StringJoin[ToString@#, " mm/s"], fs]&/@tabs[[;;, 1, 5]]
];
If[Length[speeds]>1
	, colors = getColors[Length[speeds]];
	, colors = {Black}
];
leg = If[twophase, "", PointLegend[colors, speeds, LegendLabel->Switch[mode, 1, Grid[{Style[#, fs]&/@{"TEGDMA", "h", "Nozzle", "Substrate"}[[difind]]}], 2, "vF (mm/s)"]]];
Row[{Graphics[{
			PointSize[ps]
			,
			Table[{If[twophase, Black, colors[[i]]], plotline[tabs[[i]], indep, pr, ps/2, twophase]}, {i, Length[speeds]}]
		}
		, PlotRange->pr
		, AspectRatio->1
		, Frame->True
		, FrameStyle->Black
		, LabelStyle->Directive[fs, Black]
		, FrameLabel->{"Stage speed (mm/s)", SMHEADER3[[dep]]}
		, PlotLabel->Switch[mode, 1, "", 2, Grid[Style[#, {"Text", Directive[fs, Black]}]&/@#&/@{{"TEG", "h", "Noz", "Sub", "vF"}[[1;;4]], map1[[2,1;;4]]}]]
		, ImageSize->iw]
, leg}]
]];


(* ::Subsubsection:: *)
(*twoindepssplitbyvf[map1_, indeps_, ps_, vflist_, indlist_, plotrange_, twophase_, gl_, fs_, iw_, label_, legend_]*)


(* ::Text:: *)
(*map1 = table of summary variables that goes with FOTPHEADER*)
(*indeps = list of independent variable indices e.g. {46, 49}*)
(*ps = point size (e.g. 0.1)*)
(*vflist = list of acceptable flow speeds (list of ints) input {} to use all*)
(*NEEDS: getColors, expand, plotline*)


twoindepssplitbyvf[map1_, indeps_, ps_, vflist_, indlist_, plotrange_, twophase_, gl_, fs_, iw_, label_, legend_, includedrops_]:=Module[{tab1, yrange, xrange, pr, tabs, speeds, colors, leg, indep, vfl, indl, tab2, vs, usedspeeds, plot},Catch[
If[indeps[[1]]==indeps[[2]], Throw[""]];
If[Length[vflist]>0, vfl = vflist, vfl = DeleteDuplicates[map1[[;;, 5]]]];
If[Length[indlist]>0, indl = indlist, indl = {0,1,2}];
tab1 = Select[map1, MemberQ[indeps, #[[6]]] && MemberQ[indl, Mod[#[[8]],3]] && MemberQ[vfl, #[[5]]] &];
tab1 = GatherBy[tab1, #[[{1,2,3,4,5,8,9}]]&];
If[indeps[[2]]>indeps[[1]]
	, tab1 = SortBy[#, #[[6]]&]&/@tab1;
	, tab1 = Reverse[SortBy[#, #[[6]]&]]&/@tab1;
];
tab2 = tab1[[;;, ;;, {5, 14, 15, 16, 17}]];
tabs = GatherBy[tab2, #[[1,1]]&];
If[includedrops==1, tabs = Select[#, !NumberQ[#[[1,4]]] && !NumberQ[#[[2,4]]]&]&/@tabs];
If[includedrops==2, tabs = Select[#, NumberQ[#[[1,4]]] && NumberQ[#[[2,4]]]&]&/@tabs]; 
tabs = Select[tabs, Length[#]>0&];
(*	tabs[[;;, ;;, 1, 2]] = tabs[[;;, ;;, 1, 2]] - NOZEXITF*NOZZLEWIDTH;
	tabs[[;;, ;;, 1, 4]] = tabs[[;;, ;;, 1, 4]] - NOZEXITF*NOZZLEWIDTH;*)
vs = False;
If[Length[tabs]==1,
	tab2 = Sort[tab1[[;;, ;;, {9, 14, 15, 16, 17}]]];
	tabs = GatherBy[tab2, #[[1,1]]&];
	If[includedrops==1, tabs = Select[#, !NumberQ[#[[1,4]]] && !NumberQ[#[[2,4]]]&]&/@tabs];
	If[includedrops==2, tabs = Select[#, NumberQ[#[[1,4]]] && NumberQ[#[[2,4]]]&]&/@tabs]; 
	tabs = Select[tabs, Length[#]>0&];
	vs = True;
];
If[Length[plotrange]<2,
{xrange, yrange} =Table[ MinMax[Select[Flatten[
						If[NumberQ[#[[2]]]
							, If[NumberQ[#[[4]]]
								, {#[[2]]-#[[3]], #[[2]]+#[[3]],#[[4]]-#[[5]], #[[4]]+#[[5]]}
								, {#[[2]]-#[[3]], #[[2]]+#[[3]]}
								]
						, ""]&/@((*tab2[[;;,i]]*)Flatten[tabs[[;;,;;,i]],1])
					], NumberQ]], {i, 2}];
pr = expand[#, 0.1]&/@{xrange, yrange};
,
pr = plotrange
];
speeds = Style[StringJoin[ToString@#, " mm/s"], {"Text", Directive[fs, Black]}]&/@tabs[[;;, 1, 1,1]];
usedspeeds = Range[Length[speeds]];
If[Length[vflist]>1
	, usedspeeds = Position[vflist, #][[1,1]]&/@tabs[[;;, 1, 1,1]]; 
];
If[!twophase
	, 
	colors = getColors[Length[speeds]][[usedspeeds]];
	leg = PointLegend[colors[[1;;Length[speeds]]], speeds, LegendLabel->Style[If[vs, "vS (mm/s)", "vF (mm/s)"], {"Text", Directive[fs, Black]}]];
	,
	leg = ""
];

plot = Graphics[{
			PointSize[ps]
			,
			Table[{If[twophase, Black, colors[[i]]], plotline2[tabs[[i]], pr, ps/2, twophase]}, {i, Length[speeds]}]
		}
		, PlotRange->pr
		, AspectRatio->1
		, Frame->True
		, FrameStyle->Black
		, LabelStyle->Directive[fs, Black]
		, FrameLabel->SMHEADER3[[indeps]]
		, ImageSize->iw
		, GridLines->gl
		, PlotLabel->If[label, Grid[Style[#, {"Text", Directive[fs, Black]}]&/@#&/@{{"TEG", "h", "Noz", "Sub", "vF"}[[1;;If[vs, 5, 4]]], map1[[2,1;;If[vs, 5, 4]]]}], None]
		];
If[legend, Row[{plot, leg}], plot]
]];


(* ::Subsubsection:: *)
(*splitmapintopoints[map1_, indeps_, indl_]*)


splitmapintopoints[map1_, indeps_, indl_]:=Module[{tab1, tabs},
tab1 = Select[map1, MemberQ[indeps, #[[6]]] && MemberQ[indl, Mod[#[[8]],3]] &];
tab1 = GatherBy[tab1, #[[{1,2,3,4,5,8,9}]]&];
If[indeps[[2]]>indeps[[1]]
	, tab1 = SortBy[#, #[[6]]&]&/@tab1;
	, tab1 = Reverse[SortBy[#, #[[6]]&]]&/@tab1;
];
tab1 = tab1[[;;, ;;, {5, 14, 15, 16, 17}]]
]


(* ::Subsubsection:: *)
(*twoindepssplitbymap[maps_, indeps_, ps_, indlist_, twophase_, fs_, iw_, plotrange_]*)


twoindepssplitbymap[maps_, indeps_, ps_, vflist_, indlist_, twophase_, fs_, iw_, plotrange_, includedrops_]:=Module[{yrange, xrange, pr, tabs, speeds, colors, leg, indep, indl, diffs, difind, ms},Catch[
If[indeps[[1]]==indeps[[2]], Throw[""]];
If[Length[indlist]>0, indl = indlist, indl = {0,1,2}];
If[Length[vflist]>0,
	ms = Select[#, MemberQ[vflist, #[[5]]]&]&/@maps;
	,
	ms = maps;
];
tabs = splitmapintopoints[#, indeps, indl]&/@ms;
If[includedrops==1, tabs = Select[#, !NumberQ[#[[1,4]]] && !NumberQ[#[[2,4]]]&]&/@tabs];
If[includedrops==2, tabs = Select[#, NumberQ[#[[1,4]]] && NumberQ[#[[2,4]]]&]&/@tabs];

If[Length[plotrange]<2 || Length[plotrange[[1]]]<2,
	{xrange, yrange} =Table[ MinMax[Select[Flatten[
						If[NumberQ[#[[2]]]
							, If[NumberQ[#[[4]]]
								, {#[[2]]-#[[3]], #[[2]]+#[[3]],#[[4]]-#[[5]], #[[4]]+#[[5]]}
								, {#[[2]]-#[[3]], #[[2]]+#[[3]]}
								]
						, ""]&/@(Flatten[tabs[[;;,;;,i]],1])
					], NumberQ]], {i, 2}];
	pr = expand[#, 0.1]&/@{xrange, yrange};
,
	pr = plotrange
];

diffs = ms[[;;, 1, 1;;4]];
difind = Select[Position[DeleteDuplicates/@Transpose[diffs], z_/;Length[z]>1], Length[#]>0&][[;;,1]];
If[Length[difind]==0,
	leg = "";
	speeds = {1};
	colors = {Black};
,
	diffs = diffs[[;;, difind]];
	speeds = Grid[{Style[#, fs]&/@#}]&/@diffs;
	colors = getColors[Length[speeds]];
	leg = PointLegend[colors, speeds, LegendLabel->Grid[{Style[#, fs]&/@{"TEGDMA", "h", "Nozzle", "Substrate"}[[difind]]}]];
(*leg = Grid[Style[#, {"Text", Directive[12, Black]}]&/@#&/@Prepend[Table[Join[{Style[\[FilledCircle], colors[[i]]]}, speeds[[i]]], {i, Length[speeds]}], {"", "TEGDMA", "h (um)", "noz", "sub"}[[Prepend[difind+1, 1]]]]];*)
];
Row[{Graphics[{
			PointSize[ps]
			,
			Table[{If[twophase, Black, colors[[i]]], plotline2[tabs[[i]], pr, ps/2, twophase]}, {i, Length[speeds]}]
		}
		, PlotRange->pr
		, AspectRatio->1
		, Frame->True
		, FrameStyle->Black
		, LabelStyle->Directive[fs, Black]
		, FrameLabel->SMHEADER3[[indeps]]
		, ImageSize->iw
		]
,leg}]
]];


(* ::Subsubsection:: *)
(*stripeymap*)


STRIPEYCOORDS = {{0, 0.75}, {0.25, 1}, {0.5, 1}, {0, 0.5}, {0, 0.25}, {0.75, 1}, {1,1}, (*{0,0},  {0.25, 0}, *){1, 0.75},{0.25, 0}, {0.5, 0}, {1, 0.5}, (*{0.5, 0}, {0.75, 0}, *){1, 0.25},{0.75, 0}, {1,0}, {0,0}};
stripeyrect[lowerbounds_, upperbounds_, color1_, color2_]:= {color1, Rectangle[lowerbounds, upperbounds], color2, (*EdgeForm[Black],*) Polygon[((upperbounds- lowerbounds)*# + lowerbounds)&/@STRIPEYCOORDS]}


asquare[row_, xspacing_, yspacing_, mms_, colo_]:=Module[{color1, lowerbounds, upperbounds, color2, c},Catch[
If[!NumberQ[row[[3]]], Throw[{}]];
c = (row[[3]] - mms[[1]])/(mms[[2]]-mms[[1]]);
If[c<0, c=0];
If[c>1, c=1];
color1 = ColorData[colo][c];
{lowerbounds, upperbounds} = {{row[[1]] - xspacing/2,row[[2]]-yspacing/2}, { row[[1]]+xspacing/2, row[[2]]+yspacing/2}};
If[NumberQ[row[[4]]],
color2 = ColorData[colo][(row[[4]] - mms[[1]])/(mms[[2]]-mms[[1]])];
stripeyrect[lowerbounds, upperbounds, color1, color2]
,
{color1, Rectangle[lowerbounds, upperbounds]}
]]]


lastlinethatworks[t_]:=Module[{s}, 
s = Select[t, NumberQ[#[[3]]]&];
If[Length[s]>0, MaximalBy[s, #[[5]]&][[1]], t[[-1]]]];


smtab[tab_, mm_, pl_, fs_, gkey_, leg_, iw_, plotleg_, letters_, overlay_]:=Module[{mms, colo, xvals, yvals, xspacing, yspacing, pr, title, legend, square},
If[Length[mm]==0
	, mms = MinMax[Select[Flatten[tab[[;;, {3,4}]]], NumberQ]];
	, mms = mm
];
colo = "RedBlueTones";
xvals = Sort[DeleteDuplicates[tab[[;;,1]]]];
yvals = Sort[DeleteDuplicates[tab[[;;,2]]]];
xspacing = Min[Differences[xvals]];
yspacing = Min[Differences[yvals]];
pr = {
		{Min[xvals]-xspacing/2
		, Max[xvals]+(Max[xvals]-RankedMax[xvals,2])/2}
	,  
		{Min[yvals]-yspacing/2
		, Max[yvals]+yspacing/2}
	};
title = If[pl
		, Grid[Style[#, {"Text", Directive[fs, Black]}]&/@#&/@{{"TEGDMA", "h", "Nozzle", "Substrate"}, gkey[[1]]}]
		, ""];
legend = If[leg, If[iw>fs*15,Row,Column][{
		plotleg
		, BarLegend[{colo, mms}
			, LegendLayout->"Row"
			, LegendMarkerSize->If[iw>fs*15 && pl,iw/2,iw]
			, LabelStyle->Directive[fs, Black]
			, Method->{FrameStyle->Directive[Thin, Black],TicksStyle->Directive[Black, Opacity[1]]}
			]
	}, Alignment->Center]
	, ""];
(*	Print[Grid[Round[SortBy[Select[tab, NumberQ[#[[3]]]&&!NumberQ[#[[4]]]&][[;;,1;;3]], #[[{2,1}]]&],0.01]]];
*)square = Show[
		Graphics[{
			asquare[#, If[#[[1]]>=4.5, 1, xspacing], yspacing, mms, colo]&/@tab
			, Black
(*			, Line[{{#,pr[[2,1]]}, {#, pr[[2,2]]}}]&/@(Select[xvals, #<4.5&]-xspacing/2)
			, Line[{{#,pr[[2,1]]}, {#, pr[[2,2]]}}]&/@(Select[xvals, #>=4.5&]-1/2)
			, Line[{{pr[[1,1]], #}, {pr[[1,2]], #}}]&/@(yvals-yspacing/2)*)
			}
			, Frame->True
			, PlotRange->pr
			, PlotRangePadding->0
			, ImageSize->iw
			, FrameStyle->Black
			, LabelStyle->Directive[fs, Black]
			, FrameTicks->{{Select[Range[1.5, 6.5, 1], pr[[2,1]]<=#<=pr[[2,2]]&], None}
							,{Select[Range[1.5, 9.5, 1], pr[[1,1]]<=#<=pr[[1,2]]&], None}}
			, FrameLabel->(*{Row[{Subscript["v","s"]," (mm/s)"}], Row[{Subscript["v","f"]," (mm/s)"}]}*){"Stage speed (mm/s)", "Flow speed (mm/s)"}
		]
		, Switch[letters
			,0, plotmap[gkey2mkey[gkey[[1]]], True, fs]
			,1, Graphics[Text[Style[Round[#[[3]],0.01], Directive[fs, If[#[[3]]<0, Red, Black]]], #[[1;;2]]]&/@Select[tab, #[[1]] - Floor[#[[1]]]==0.5 && NumberQ[#[[3]]]&&!NumberQ[#[[4]]]&][[;;,1;;3]]]
			,2, Graphics[]]
(*		, Plot[x, {x, pr[[1,1]], pr[[1,2]]}, PlotStyle->Black, PlotRange\[Rule]pr]*)
		, overlay
	];
Column[If[pl, If[leg, {title, legend, square}, {title, square}], If[leg, {legend, square}, {square}]], Alignment->Center]

]


stripeymap[gkey_, map1_, dep_, iw_, mm_, fs_, pl_, leg_, letters_, overlay_, widen_]:=Module[{tab, t1, plotleg},
If[!ValueQ[MAPS], initializeQualitativeFits];
tab = Select[map1, #[[6]]==dep&][[;;, {9,5,14,16,8}]];
t1 = GatherBy[tab, #[[1;;2]]&];
tab = lastlinethatworks/@t1;
plotleg = If[pl, txts[SMHEADER3[[dep]],fs], ""];
If[widen,
	tab = Select[tab, NumberQ[#[[3]]]&];
	tab = Join[tab, Flatten[Table[{i,j, "", "", ""}, {i, 1.5, 9.5, 1}, {j, 1.5, 6.5, 1}],1]];
	If[gkey[[1,1]]==35, tab = Append[tab, {1.1, 1.5, "", "", ""}]];
	If[gkey[[1,1]]==20, tab = Join[tab, {{1.5, 1.1, "", "", ""}, {1.5, 6.9, "", "", ""}}]];
	tab = If[Length[#]>1, Select[#, NumberQ[#[[3]]]&][[1]], #[[1]]]&/@GatherBy[tab, #[[1;;2]]&];
];
If[gkey[[1,2]]<150,
	tab = Select[tab, NumberQ[#[[3]]]&];
	tab = Append[tab, {1.5, 6.5, "", "", ""}];
	tab = If[Length[#]>1, Select[#, NumberQ[#[[3]]]&][[1]], #[[1]]]&/@GatherBy[tab, #[[1;;2]]&];
];
smtab[tab, mm, pl, fs, gkey, leg, iw, plotleg, letters, overlay]
]


txts[string_, fs_]:=Style[string, {"Text", Directive[fs, Black]}]


(* ::Subsubsection:: *)
(*annotatedStripe[ssplit_, index_, column_, iw_, pr_, fs_, plots_]*)


(* ::Text:: *)
(*ssplit = list of maps*)
(*index = map index (int)*)
(*column = dependent variable (int)*)
(*iw = image width (px)*)
(*pr = plot range for dependent variable ({} for automatic)*)
(*fs = font size *)
(*title = bool true to include plot title*)
(*lenged = bool true to include plot legend*)
(*plots = list of models to plot: 1 for viscous, 2 for viscocapillary, 3 for capillary*)
(*OUTPUT: plot*)
(*NEEDS: stripeymap*)


annotatedStripe[ssplit_, index_, column_, iw_, pr_, fs_, title_, legend_, plots_, letters_, widen_]:=Module[{gkey, st, ann, p, pl},
gkey = ssplit[[index, 1, 1;;4]];
If[Length[plots]>0,
	{p, pl} = anns[ssplit, index, plots];
	st = {Red, {Red, Dashed}, Black, {Black, Dashed}, White, {White, Dashed}}[[p]];
	ann = Plot[pl, 
		{U
			, 1
			, If[widen, 10, 7]
		}
		, FrameLabel->{"U", "Q"}
		, PlotStyle->st
		, PlotRange->{1, 7}
		];
		,
	ann = Graphics[]
];

stripeymap[{gkey}, ssplit[[index]], column, iw, pr, fs, legend, title, letters, ann, widen]
]


anns[ssplit_, index_, plots_]:=Module[{ld, lu, gkey, \[Mu], lcrit, l1, h, \[Sigma], \[Theta], de, v1, v2, vc1, vc2, c1, c2, ann, pl, st, p},
ld = (0.525-0.15);
lu = 0.5;
gkey = ssplit[[index, 1, 1;;4]];

l1 = ld*10^-3; (*m*)
h = gkey[[2]]*10^-6; (*m*)
(*\[Mu] =<|35-> 7.075*10^3, 30-> 1.894*10^4, 25->6.4*10^4, 20->1.301*10^5|>[gkey[[1]]]; (*g/(m*s)*)*)
\[Mu] = viscAssoc[gkey[[1]], U*10^-3, h]; (*g/(m*s)*)
\[Sigma] = \[Gamma]Assoc[gkey[[1]]]; (*g/s^2*)
(*\[Theta] = \[Theta]Assoc[gkey[[{1, 4}]]]; *)
\[Theta] = \[Theta]Assoc[gkey]; (* {overflow, underflow} *)
de= 0.15*10^-3; (*m*)
v1 =  U*h/2/de;
v2 = U*((ld*h^2 + lu*h^2)*h)/(2*ld*h^2)/de;
lcrit = (*-0.1;*)0;
vc1 = 10^3*Simplify@(vf/.Solve[0==6*\[Mu]*(U*10^-3)*l1/h^2(1 +lcrit/ld - 2*(vf*de)/(h*(U*10^-3))) + 1.34*\[Mu]^(2/3)(U*10^-3)^(5/3)*\[Sigma]^(1/3)/(vf*de) - \[Sigma] (Abs[Cos[\[Theta][[2]]]]+1)/h, vf][[2]]); (*droplet*)
vc2 =10^3Simplify@(vf/.Solve[0==6*\[Mu]*(U*10^-3)*l1/h^2(1  +lu/ld-2*(vf*de)/(h*(U*10^-3))) + 1.34*\[Mu]^(2/3)(U*10^-3)^(5/3)*\[Sigma]^(1/3)/(vf*de) - \[Sigma] (Abs[Cos[\[Theta][[1]]]] + 1)/h, vf][[2]]); (*overflow*)
c1 = 10^3*1.34*\[Mu]^(2/3)*(U*10^-3)^(5/3)*h/(de * \[Sigma]^(2/3)*(Cos[\[Theta]]+1));
c2 = 10^3*1.34*\[Mu]^(2/3)*(U*10^-3)^(5/3)*h/(de * \[Sigma]^(2/3)*(-Cos[\[Theta]] + 1));
p = Join[plots*2, plots*2-1];
pl = {v2,v1, vc2, vc1, c2, c1}[[p]];
{p, pl}
]



(* ::Subsubsection:: *)
(*viscAssoc*)


(* ::Text:: *)
(*ink in % TEGDMA*)
(*vS in m/s*)
(*h in m*)
(*output in mPa s or g/(m*s)*)


viscAssoc[ink_, vS_, h_]:=10^3*<|35-> 0.50012 + 15.3568/(1 + 33659.3(vS/h)^5.33223)^0.123512
	, 30-> 0.451824 + 54.7978/(1 + 5.22057*10^31(vS/h)^18.2474)^0.0275023
	, 25->0.748 + 85.7148/(1 + 38.2217(vS/h)^1.6155)^0.38797
	, 20->1.774479 + 125.321/((1 + 18.1235*(vS/h)^1.34)^0.5148)
	|>[ink]


viscAssoc[#, 9.5*10^-3, 150*10^-6]&/@Range[20, 35, 5]


capillaryAssoc[ink_, vs_, h_]:=viscAssoc[ink, vs, h]*vs/(\[Gamma]Assoc[ink]);


(* ::Text:: *)
(*surface tension in mPa m*)


\[Gamma]Assoc[ink_]:=<|35-> 34.6477, 30-> 37.893, 25->40.2325, 20->41.2386|>[ink]


(*\[Theta]Assoc[{ink_, sub_}]:=<|{35, "film"}-> 65, {30, "film"}-> 62.27, {25, "film"}->61.62, {20, "film"}->52.97
						, {35, "glass"}-> 35, {30, "glass"}->43.91, {25, "glass"}->47.13, {20, "glass"}->64.12|>[{ink, sub}]*Degree*)


\[Theta]Assoc[gkey_]:=<|{20, 150, "FDTS", "film"}->{173, 125}
				,{25, 100, "FDTS", "film"}->{140, 120}
				,{25, 150, "FDTS", "film"}->{152, 117}
				,{25, 150, 0, "film"}->{160, 120}
				,{25, 150, 0, "glass"}->{155, 115}
				,{25, 150, "FDTS", "glass"}->{150, 115}
				,{25, 200, "FDTS", "film"}->{157, 115}
				,{25, 250, "FDTS", "film"}->{157, 115}
				,{30, 150, "FDTS", "film"}->{155, 120}
				,{35, 150, "FDTS", "film"}->{155, 110}|>[gkey]


(* ::Subsubsection:: *)
(*manipulatemodule[]*)


manipulatemodule[]:=DynamicModule[{xvar, yvar, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, label, plot, plotlist, mode, modellist, legend, includedrops, letters},
xvar = 49;
yvar = 46;
pointsize = 0.02;
vflist = {};
indlist = {0,1,2};
plotrange = {};
twophase = False;
gridlines = {};
fontsize = 12;
imagewidth = 350;
label = True;
plot = "";
plotlist = {5};
mode = 1;
modellist = {2};
legend = True;
includedrops = 0;
letters = 0;
Dynamic[Panel[Column[{Row[{
	Grid[{{"list", SetterBar[Dynamic[plotlist], Join[{hlist->"h", inklist->"ink", gammalist->"gamma", nozlist->"nozzle", sublist->"substrate"}, ({#}->ssplit[[#,1,1;;4]])&/@Range[10]], Appearance->"Row"]}}, Alignment->{{Right, Left}, None}
	, ItemSize->{{8,20}, None}
	]
	, Grid[{
		{"plot type", SetterBar[Dynamic[mode], {1->"2 indeps (map)", 2->"2 indeps (vf)", 3->"2 indeps (vs)", 4->"vs (vf)", 5->"stripeymap"}, Appearance->"Row"]}		
		, {"x",SetterBar[Dynamic[xvar], (#->SMHEADER3[[#]])&/@IMPORTANTLIST, Appearance->"Row"]}
		, {"y", SetterBar[Dynamic[yvar],  (#->SMHEADER3[[#]])&/@IMPORTANTLIST, Appearance->"Row"]}
		, {"font size", SetterBar[Dynamic[fontsize], Range[6, 24, 2]]}
		}, Alignment->{{Right, Left}, None}
	, ItemSize->{{8,40}, None}
	]
	, Grid[{
		{"point size", Slider[Dynamic[pointsize],{ 0.01, 0.05}]}		
		, {"index", TogglerBar[Dynamic[indlist], {0,1,2}]}
		, {"model", TogglerBar[Dynamic[modellist], {1->"viscous",2->"viscocapillary",3->"capillary"}]}
		, {"", SetterBar[Dynamic[includedrops], {0->"All",1->"Filaments only",2->"Droplets only"}]}
		, {"", SetterBar[Dynamic[letters], {0->"Letters",1->"Values",2->"None"}]}
		, {"plot range", InputField[Dynamic[plotrange], FieldSize->15]}
		, {"grid lines", InputField[Dynamic[gridlines], FieldSize->15]}
		, {"vF", InputField[Dynamic[vflist], FieldSize->15]}
		, {"image width", InputField[Dynamic[imagewidth], FieldSize->15]}
		, {"two phase", Checkbox[Dynamic[twophase]]}
		, {"label", Checkbox[Dynamic[label]]}
		, {"legend", Checkbox[Dynamic[legend]]}
				
		, {"", Button["Update plot", Switch[mode
					,1,
						plot = twoindepssplitbymap[ssplit[[plotlist]], {xvar, yvar}, pointsize, vflist, indlist, twophase, fontsize, imagewidth, plotrange, includedrops];
					,2,
						plot = twoindepsgrid[plotlist, xvar, yvar, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, label, legend, includedrops]
					,3,
						plot = Column[Row[twoindepssplitbyvf[#, {xvar, yvar}, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, label, legend, includedrops]&/@GatherBy[ssplit[[#]], #[[5]]&]]&/@plotlist]
					,4,
						plot = vsplotsplitbyvf[yvar, ssplit[[plotlist]][[If[Length[plotlist]==1, 1, ;;]]], pointsize, vflist, indlist, twophase, fontsize, imagewidth, plotrange]
					,5,
						plot = Row[annotatedStripe[ssplit, #, yvar, imagewidth, If[Length[plotrange]==2 && Length[plotrange[[1]]]==0, plotrange, {}], fontsize, legend, label, modellist, letters, plotlist==inklist]&/@plotlist, "  "]
		]]}
		, {"", Button["Export plot", Export[SystemDialogInput["FileSave"], plot], Method->"Queued"]}
	}
	, Alignment->{{Right, Left}, None}
	, ItemSize->{{8,20}, None}
	]}]
	,
	""
	,
	Dynamic[plot]
}]]]
]


twoindepsgrid[plotlist_, xvar_, yvar_, pointsize_, vflist_, indlist_, plotrange_, twophase_, gridlines_, fontsize_, imagewidth_, label_, legend_, includedrops_]:=Module[{gamma = False, labelrow, output, scrape = False},
If[Length[plotlist]>1
	, If[plotlist[[1]]==2
		, labelrow = Style[ToString[#]<>" \[Mu]m", Directive[fontsize, Black]]&/@{100, 150, 200, 250}
		, If[plotlist[[1]]==1
			, labelrow = Style[ToString[#]<>"% TEGDMA", Directive[fontsize, Black]]&/@{20, 25, 30, 35}
			, gamma = True;
		];
	];
	If[Length[plotrange]>0 (*&& Length[vflist]>0*), scrape = True];
	, labelrow = {}
];
If[!gamma,
output = Grid[{Append[If[Length[labelrow]>0, Row/@Transpose[{txts[#<>"    ",fontsize+2]&/@{"A", "B", "C", "D"}, txts[#, fontsize]&/@labelrow}], {""}],  ""],						
					Append[Table[
						If[scrape
							, Show[twoindepssplitbyvf[ssplit[[plotlist[[i]]]], {xvar, yvar}, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, False, False, includedrops]
								, ImageSize->{Automatic, imagewidth}
								, FrameLabel->If[i>1, {SMHEADER3[[xvar]], None}, SMHEADER3[[{xvar, yvar}]]]
								]
							, twoindepssplitbyvf[ssplit[[plotlist[[i]]]], {xvar, yvar}, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, label, legend, includedrops]
						]
					,{i, Length[plotlist]}]
					, If[scrape && !twophase, PointLegend[getColors[Length[vflist]], vflist, LegendLabel->txts["vF (mm/s)", fontsize]], ""]
					]}, Alignment->Left];
,
output = If[scrape, Grid[{txts[#, fontsize]&/@{"", "Film", "Glass", ""}
				, Join[{txts["No coat", fontsize]}, Table[Show[twoindepssplitbyvf[ssplit[[plotlist[[i]]]], {xvar, yvar}, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, False, False, includedrops]
								, ImageSize->{Automatic, imagewidth-2*fontsize}, FrameLabel->{None, {SMHEADER3[[yvar]], None}[[i]]}
								, ImagePadding->{{{4.5,2}[[i]]*fontsize, fontsize}, {2*fontsize, 0.2*fontsize}}
								], {i, 1, 2}], {""}]
				, Join[{txts["FDTS", fontsize]}, Table[Show[twoindepssplitbyvf[ssplit[[plotlist[[i]]]], {xvar, yvar}, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, False, False, includedrops]
								, ImageSize->{Automatic, imagewidth}, FrameLabel->{SMHEADER3[[xvar]], {SMHEADER3[[yvar]], None}[[i-2]]}
								, PlotLegends-> If[i==Length[plotlist], PointLegend[getColors[Length[vflist]], vflist, LegendLabel->txts["vF (mm/s)", fontsize]], None]
								, ImagePadding->{{{4.5,2}[[i-2]]*fontsize, fontsize}, {4*fontsize, 0.2*fontsize}}
								], {i, 3, 4}], If[!twophase, {PointLegend[getColors[Length[vflist]], vflist, LegendLabel->txts["vF (mm/s)", fontsize]]}, {}]]
		}]
		, Grid[{txts[#, fontsize]&/@{"", "Film", "Glass"}
				, Join[{txts["No coat", fontsize]}, Table[twoindepssplitbyvf[ssplit[[plotlist[[i]]]], {xvar, yvar}, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, label, legend, includedrops], {i, 1, 2}]]
				, Join[{txts["FDTS", fontsize]}, Table[twoindepssplitbyvf[ssplit[[plotlist[[i]]]], {xvar, yvar}, pointsize, vflist, indlist, plotrange, twophase, gridlines, fontsize, imagewidth, label, legend, includedrops], {i, 3, 4}]]
		}]
		];
];
output
]


speed2string[ink_, vs_, vf_]:=Module[{speed, select},
select = 1;
If[ink<35 || vs>4.3,
If[vs<=3.5,speed = "slow",If[vs<=6.5,speed = "fast",speed = "turbo"]]
,
If[vs<=1.9
	, speed = "slow"
	, If[vs<2.5
		, speed = "fast"; If[vf<3.5, select = 2];
		, If[vs<=3.1
			, speed = "turbo"; If[vf<3.5, select = 2];
			, If[vs<=3.7
				, speed = "veryfast"
				, speed = "extremefast"
			]
		]
	]
]
];
{speed, select}]
