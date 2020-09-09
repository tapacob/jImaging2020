#pragma rtGlobals=1		// Use modern global access method.
#pragma version=1.22

// 1.22 fixed offset calculation

// 12/10/09 1.21

// 6/24/08 1.20

// line between cursors

// 6/9/08 1.12

// 9/12/07
// 7/23/07 1.10b
// added smoothed spline baseline
// 7/3/07 1.00


// Four menus: graph marquee, trace and all-trace popups and a tab inside the Macro menu

Menu "GraphMarquee"
	submenu "Interval Analysis"
		"Initialise Interval Analysis", /Q, MenuInitBaselineFitting()
		"Add Region to Analysis", /Q, GetMarquee /K bottom; ResetFitRegion(V_left, V_right, 1) 
		"Remove Region from Analysis", /Q, GetMarquee /K bottom; ResetFitRegion(V_left, V_right, 0) 
		"Clear All Regions", /Q,  ClearFitRegion()
		submenu "Baseline fit"
			"line", /Q, MenuFitBaseline()
			"poly 3", /Q, MenuFitBaseline()
			"poly 4", /Q, MenuFitBaseline()
			"gauss", /Q, MenuFitBaseline()		
			"lor", /Q, MenuFitBaseline()
			"exp", /Q, MenuFitBaseline()
			"Tony's gauss", /Q, MenuFitBaseline()
			"sin", /Q, MenuFitBaseline()
			"sigmoid", /Q, MenuFitBaseline()
			"spline", /Q,  MenuFitBaseline()
			"line between cursors", /Q, MenuFitBaseline()
			submenu "more polynomials..."
				"poly 5", /Q, MenuFitBaseline() //ait
				"poly 6", /Q, MenuFitBaseline() //ait
				"poly 7", /Q, MenuFitBaseline() //ait
				"poly 8", /Q, MenuFitBaseline() //ait
				"poly 9", /Q, MenuFitBaseline() //ait
				"poly 10", /Q, MenuFitBaseline() //ait
				"poly 11", /Q, MenuFitBaseline() //ait
				"poly 12", /Q, MenuFitBaseline() //ait
				"poly 13", /Q, MenuFitBaseline() //ait
				"poly 14", /Q, MenuFitBaseline() //ait
				"poly 15", /Q, MenuFitBaseline() //ait
				"poly 16", /Q, MenuFitBaseline() //ait
				"poly 17", /Q, MenuFitBaseline() //ait
				"poly 18", /Q, MenuFitBaseline() //ait
				"poly 19", /Q, MenuFitBaseline() //ait
				"poly 20", /Q, MenuFitBaseline() //ait
			end
			submenu "piecewise"
				"piecewise linear", /Q, MenuFitBaseline() //ait
				"piecewise square", /Q, MenuFitBaseline() //ait
				"piecewise cubic", /Q, MenuFitBaseline() //ait
				"piecewise exp", /Q, MenuFitBaseline() //ait
			end
//			"exp_XOffset", /Q, MenuFitBaseline()
//			"dblexp_XOffset", /Q, MenuFitBaseline()
		end
		"Subtract Baseline", /Q, MenuSubtractBaseline()
		Submenu "Correct the Recording Artifacts"
			"end - start", /Q, MenuCorrectArtifact()
		end
		Submenu "Quantify Effects within Regions"
			"linear", /Q, MenuQuantifyRegions()
			"sigmoid", /Q, MenuQuantifyRegions()
			"Hill", /Q, MenuQuantifyRegions()
			"end-start", /Q, MenuQuantifyRegions()
			"average", /Q, MenuQuantifyRegions()
		end
		Submenu "Manipulate Regions"
			"Translate Baseline Regions", /Q, TranslateBaseline()
			"Invoke Saved Regions", /Q, InvokeRegions()
			"Dilate Regions", /Q, DilateRegions()
		End
	end
End

Menu "TracePopup"
	submenu "Interval Analysis"
		"Initialise Interval Analysis", /Q, MenuInitBaselineFitting()		
		submenu "Baseline fit"
			"line", /Q, MenuFitBaseline()
			"poly 3", /Q, MenuFitBaseline()
			"poly 4", /Q, MenuFitBaseline()
			"gauss", /Q, MenuFitBaseline()		
			"lor", /Q, MenuFitBaseline()
			"exp", /Q, MenuFitBaseline()
			"Tony's gauss", /Q, MenuFitBaseline()
			"sin", /Q, MenuFitBaseline()
			"sigmoid", /Q, MenuFitBaseline()
			"spline", /Q, MenuFitBaseline()
			"line between cursors", /Q, MenuFitBaseline()
			submenu "more polynomials..."
				"poly 5", /Q, MenuFitBaseline() //ait
				"poly 6", /Q, MenuFitBaseline() //ait
				"poly 7", /Q, MenuFitBaseline() //ait
				"poly 8", /Q, MenuFitBaseline() //ait
				"poly 9", /Q, MenuFitBaseline() //ait
				"poly 10", /Q, MenuFitBaseline() //ait
				"poly 11", /Q, MenuFitBaseline() //ait
				"poly 12", /Q, MenuFitBaseline() //ait
				"poly 13", /Q, MenuFitBaseline() //ait
				"poly 14", /Q, MenuFitBaseline() //ait
				"poly 15", /Q, MenuFitBaseline() //ait
				"poly 16", /Q, MenuFitBaseline() //ait
				"poly 17", /Q, MenuFitBaseline() //ait
				"poly 18", /Q, MenuFitBaseline() //ait
				"poly 19", /Q, MenuFitBaseline() //ait
				"poly 20", /Q, MenuFitBaseline() //ait
			end
			submenu "piecewise"
				"piecewise linear", /Q, MenuFitBaseline() //ait
				"piecewise square", /Q, MenuFitBaseline() //ait
				"piecewise cubic", /Q, MenuFitBaseline() //ait
				"piecewise exp", /Q, MenuFitBaseline() //ait
			end
//			"exp_XOffset", /Q, MenuFitBaseline()
//			"dblexp_XOffset", /Q, MenuFitBaseline()
		end
		"Subtract Baseline", /Q, MenuSubtractBaseline()
		Submenu "Correct the Recording Artifacts"
			"end - start", /Q, MenuCorrectArtifact()
		end
		Submenu "Quantify Effects within Regions"
			"linear", /Q, MenuQuantifyRegions()
			"sigmoid", /Q, MenuQuantifyRegions()
			"Hill", /Q, MenuQuantifyRegions()
			"end-start", /Q, MenuQuantifyRegions()
			"average", /Q, MenuQuantifyRegions()
		end
		Submenu "Manipulate Regions"
			"Translate Baseline Regions", /Q, TranslateBaseline()
			"Invoke Saved Regions", /Q, InvokeRegions()
			"Dilate Regions", /Q, DilateRegions()
		End
	end
End

Menu "AllTracesPopup" 
	submenu "Fit baseline to all traces on plot"
		"line", /Q, FitAllTraces()
		"poly 3", /Q, FitAllTraces()
		"poly 4", /Q, FitAllTraces()
		"gauss", /Q, FitAllTraces()		
		"lor", /Q, FitAllTraces()
		"exp", /Q, FitAllTraces()
		"Tony's gauss", /Q, FitAllTraces()
		"sin", /Q, MenuFitBaseline()
		"sigmoid", /Q, MenuFitBaseline()
		"spline", /Q,  FitAllTraces()
		"line between cursors", /Q,  FitAllTraces()
		submenu "more polynomials..."
			"poly 5", /Q, FitAllTraces() //ait
			"poly 6", /Q, FitAllTraces() //ait
			"poly 7", /Q, FitAllTraces() //ait
			"poly 8", /Q, FitAllTraces() //ait
			"poly 9", /Q, FitAllTraces() //ait
			"poly 10", /Q, FitAllTraces() //ait
			"poly 11", /Q, FitAllTraces() //ait
			"poly 12", /Q, FitAllTraces() //ait
			"poly 13", /Q, FitAllTraces() //ait
			"poly 14", /Q, FitAllTraces() //ait
			"poly 15", /Q, FitAllTraces() //ait
			"poly 16", /Q, FitAllTraces() //ait
			"poly 17", /Q, FitAllTraces() //ait
			"poly 18", /Q, FitAllTraces() //ait
			"poly 19", /Q, FitAllTraces() //ait
			"poly 20", /Q, FitAllTraces() //ait
		end
		submenu "piecewise"
				"piecewise linear", /Q, FitAllTraces() //ait
				"piecewise square", /Q, FitAllTraces() //ait
				"piecewise cubic", /Q, FitAllTraces() //ait
				"piecewise exp", /Q, FitAllTraces() //ait
			end
//		"exp_XOffset", /Q, MenuFitBaseline()
//		"dblexp_XOffset", /Q, MenuFitBaseline()
	end
	Submenu "Correct the Recording Artifacts"
			"end - start", /Q, CorrectAllArtifacts()
		end
		Submenu "Quantify Effects within Regions"
			"linear", /Q, QuantifyAllRegions()
			"sigmoid", /Q, QuantifyAllRegions()
			"Hill", /Q, QuantifyAllRegions()
			"end-start", /Q, QuantifyAllRegions()
			"average", /Q, QuantifyAllRegions()
		end
		Submenu "Manipulate Regions"
			"Translate Baseline Regions", /Q, TranslateBaseline()
			"Invoke Saved Regions", /Q, InvokeRegions()
			"Dilate Regions", /Q, DilateRegions()
		End
End
	

Menu "Macros"
	submenu "Interval Analysis"
		"Initialise Interval Analysis", /Q, MenuInitBaselineFitting()
		"Add Region to Analysis", /Q, AddRegionToFit() 
		"Remove Region from Analysis", /Q, GetMarquee /K bottom; ResetFitRegion(V_left, V_right, 0) 
		"Clear All Regions", /Q,  ClearFitRegion()
		submenu "Baseline fit"
			"line", /Q, MenuFitBaseline()
			"poly 3", /Q, MenuFitBaseline()
			"poly 4", /Q, MenuFitBaseline()
			"gauss", /Q, MenuFitBaseline()		
			"lor", /Q, MenuFitBaseline()
			"exp", /Q, MenuFitBaseline()
			"Tony's gauss", /Q, MenuFitBaseline()
			"sin", /Q, MenuFitBaseline()
			"sigmoid", /Q, MenuFitBaseline()
			"spline", /Q, MenuFitBaseline()
			"line between cursors", /Q, MenuFitBaseline()
			submenu "more polynomials..."
				"poly 5", /Q, MenuFitBaseline() //ait
				"poly 6", /Q, MenuFitBaseline() //ait
				"poly 7", /Q, MenuFitBaseline() //ait
				"poly 8", /Q, MenuFitBaseline() //ait
				"poly 9", /Q, MenuFitBaseline() //ait
				"poly 10", /Q, MenuFitBaseline() //ait
				"poly 11", /Q, MenuFitBaseline() //ait
				"poly 12", /Q, MenuFitBaseline() //ait
				"poly 13", /Q, MenuFitBaseline() //ait
				"poly 14", /Q, MenuFitBaseline() //ait
				"poly 15", /Q, MenuFitBaseline() //ait
				"poly 16", /Q, MenuFitBaseline() //ait
				"poly 17", /Q, MenuFitBaseline() //ait
				"poly 18", /Q, MenuFitBaseline() //ait
				"poly 19", /Q, MenuFitBaseline() //ait
				"poly 20", /Q, MenuFitBaseline() //ait
			end
			submenu "piecewise"
				"piecewise linear/CF6", /Q, MenuFitBaseline() //ait
				"piecewise square", /Q, MenuFitBaseline() //ait
				"piecewise cubic", /Q, MenuFitBaseline() //ait
				"piecewise exp", /Q, MenuFitBaseline() //ait
			end
//			"exp_XOffset", /Q, MenuFitBaseline()
//			"dblexp_XOffset", /Q, MenuFitBaseline()
		end
//		submenu "Clear Junk"
//		"Make 2D and Clear Junk", /Q, ClearJunk ()
//		"Put waves into 2D",/Q, PutWavesInto2D (WaveFragment)
//		end
		Submenu "Correct the Recording Artifacts"
			"end - start", /Q, MenuCorrectArtifact()
		end
		Submenu "Quantify Effects within Regions"
			"linear", /Q, MenuQuantifyRegions()
			"sigmoid", /Q, MenuQuantifyRegions()
			"Hill", /Q, MenuQuantifyRegions()
			"end-start", /Q, MenuQuantifyRegions()
			"average", /Q, MenuQuantifyRegions()
		end
			"Make 2D and Clear Junk", /Q, ClearJunk ()
		"Subtract Baseline", /Q, MenuSubtractBaseline()
		Submenu "Manipulate Regions"
			"Translate Baseline Regions", /Q, TranslateBaseline()
			"Invoke Saved Regions", /Q, InvokeRegions()
			"Dilate Regions", /Q, DilateRegions()
		End
		Submenu "AllTracesPopup" 
	submenu "Fit baseline to all traces on plot"
		"line", /Q, FitAllTraces()
		"poly 3", /Q, FitAllTraces()
		"poly 4", /Q, FitAllTraces()
		"gauss", /Q, FitAllTraces()		
		"lor", /Q, FitAllTraces()
		"exp", /Q, FitAllTraces()
		"Tony's gauss", /Q, FitAllTraces()
		"sin", /Q, MenuFitBaseline()
		"sigmoid", /Q, MenuFitBaseline()
		"spline", /Q,  FitAllTraces()
		"line between cursors", /Q,  FitAllTraces()
		submenu "more polynomials..."
			"poly 5", /Q, FitAllTraces() //ait
			"poly 6", /Q, FitAllTraces() //ait
			"poly 7", /Q, FitAllTraces() //ait
			"poly 8", /Q, FitAllTraces() //ait
			"poly 9", /Q, FitAllTraces() //ait
			"poly 10", /Q, FitAllTraces() //ait
			"poly 11", /Q, FitAllTraces() //ait
			"poly 12", /Q, FitAllTraces() //ait
			"poly 13", /Q, FitAllTraces() //ait
			"poly 14", /Q, FitAllTraces() //ait
			"poly 15", /Q, FitAllTraces() //ait
			"poly 16", /Q, FitAllTraces() //ait
			"poly 17", /Q, FitAllTraces() //ait
			"poly 18", /Q, FitAllTraces() //ait
			"poly 19", /Q, FitAllTraces() //ait
			"poly 20", /Q, FitAllTraces() //ait
		end
		submenu "piecewise"
				"piecewise linear/CF6", /Q, FitAllTraces() //ait
				"piecewise square", /Q, FitAllTraces() //ait
				"piecewise cubic", /Q, FitAllTraces() //ait
				"piecewise exp", /Q, FitAllTraces() //ait
			end
//		"exp_XOffset", /Q, MenuFitBaseline()
//		"dblexp_XOffset", /Q, MenuFitBaseline()
	end
	Submenu "Correct the Recording Artifacts"
			"end - start", /Q, CorrectAllArtifacts()
	End
	Submenu "Manipulate Regions"
		"Translate Baseline Regions", /Q, TranslateBaseline()
		"Invoke Saved Regions", /Q, InvokeRegions()
		"Dilate Regions", /Q, DilateRegions()
	End
	Submenu "Quantify Effects within Regions"
		"linear", /Q, QuantifyAllRegions()
		"sigmoid", /Q, QuantifyAllRegions()
		"Hill", /Q, QuantifyAllRegions()
		"end-start", /Q, QuantifyAllRegions()
		"average", /Q, QuantifyAllRegions()
	End
	
End
	end
	
end


// a wrapper function for InitBaselineFit
// Initialises baseline fit on the datawave specified (read from the image)
// Starts the routine InitBaseFitting on the wave $strDataWave = W_data

function MenuInitBaselineFitting()
	// need to select data wave
	string strDataWave
	//string normMethod //ratio or subtraction
	//prompt normMethod, "Subtract or Normalise?",popup "subtract;normalise;"//prompt for norm method
	prompt strDataWave, "Data Wave", popup, WaveList("*", ";", "WIN:" )
	doPrompt "Initialise Baseline Fit", strDataWave//, normMethod
	
	if (V_flag)
		return 0
	endif
	
	wave W_data=$strDataWave
	InitBaselineFitting(W_data)
	CheckDisplayed W_data
	if (V_flag==0)
		appendtograph W_data
	endif
	
	//wave N_method=$normMethod
	//Variable N_Index
	//N_Index=StringMatch(normMethod, "subtract")

	
end

function AddRegionToFit()
	GetMarquee /K bottom
	if (V_flag)		
		ResetFitRegion(V_left, V_right, 1) 
	else
		// dialog
		
	endif
	
end


// value = 1 to include, 0 to exclude.
// function to mark the baseline regions

 function ResetFitRegion(V_left, V_right, value)
	variable V_left, V_right, value
	
	if (exists("root:Packages:TonyIR:W_mask")==0)
		return 0
	endif
	
	SVAR wname=root:Packages:TonyIR:BaselineFitDataWaveName
	wave W_data=$wname
	
	wave W_mask=root:Packages:TonyIR:W_mask
	wave W_display=root:Packages:TonyIR:W_display 
	
	variable p_low=min(x2pnt(W_data,V_left), x2pnt(W_data,V_right))
	variable p_high=max(x2pnt(W_data,V_left), x2pnt(W_data,V_right))
	
	W_mask[p_low, p_high]=value
	W_display = W_mask[p] ? W_data[p] : NaN
	
	printf "ResetFitRegion(%d, %d, %d)\r", V_left, V_right, value
end

function ClearFitRegion()
	wave W_mask=root:Packages:TonyIR:W_mask
	wave W_display=root:Packages:TonyIR:W_display 
	W_mask=0
	W_display=nan
end


// set the data wave (raw spectrum from which baseline is to be subtracted)
// creates the Tony's folder, copies the W_data wave (inherited from the parent menu) into the folder
// as W_mask  
function InitBaselineFitting(w)
	wave w
	
	NewDataFolder /O root:Packages
	NewDataFolder /O root:Packages:TonyIR
	
	String /G root:Packages:TonyIR:BaselineFitDataWaveName=GetWavesDataFolder(w, 2)
	
	duplicate /O w root:Packages:TonyIR:W_Display 
	wave W_display=root:Packages:TonyIR:W_display 
	
	// copied the selected wave as W_display
	
	// don't reset the mask wave if new data wave has same length as previous one
	// in case we want to apply the same fit to many spectra
	if (exists("root:Packages:TonyIR:W_mask")==1)
		wave W_mask=root:Packages:TonyIR:W_mask
		if (numpnts(W_mask)!=numpnts(W_display))
			duplicate /O w root:Packages:TonyIR:W_mask
			W_mask=0
			W_display=nan
		endif
		// if W_display and the pre-existing W_mask have different numbers of points, W_mask is zeroed, W_display is nan-ed
		// if the number of points is the same, the old W_mask is kept
	else
		duplicate /O w root:Packages:TonyIR:W_Mask
		wave W_mask=root:Packages:TonyIR:W_mask
		W_mask=0
	endif
	// if no W_mask exists, make a new one

	W_display = W_mask[p] ? w[p] : NaN
// processes W_display: removes the data (NaN) from every point at which W_mask[p]=0 and
// assings the data to W_mask[i]*w[i] for each i if W_mask[i]=1 	
	checkdisplayed W_display
	if (V_flag==0)
		appendtograph W_display
		ModifyGraph mode(W_display)=7,hbFill(W_display)=4
		ModifyGraph rgb(W_display)=(24576,24576,65280)
	endif
	// displays the W_display wave, fills the region down to zero
		
	duplicate /O w  root:Packages:TonyIR:W_Base
	wave W_base=root:Packages:TonyIR:W_base
	W_base=nan
	
	if (WhichListItem("W_Base",TraceNameList("", ";", 1 ) )==-1)
		appendtograph W_Base
		ModifyGraph rgb(W_Base)=(0,15872,65280)
	endif
	
	duplicate /O w  root:Packages:TonyIR:W_NoBase
	wave W_NoBase=root:Packages:TonyIR:W_NoBase
	W_NoBase=nan
	if (WhichListItem("W_NoBase",TraceNameList("", ";", 1 ) )==-1)
		appendtograph W_NoBase
		ModifyGraph rgb(W_NoBase)=(0,0,0)
	endif
	// Generates new waves, W_base, W_NoBase with the same parameters as the original wave w
	// NaNs them and appends them to the graph.
	variable /G root:Packages:TonyIR:V_smooth
	NVAR V_smooth=root:Packages:TonyIR:V_smooth
	V_smooth+=0.5*(V_smooth==0)
	
	printf "InitBaselineFitting(%s)\r", nameofwave(w)
end


// wrapper function to subtract the current fit from the data wave
function MenuSubtractBaseline() //N_Index
	//Variable N_Index
	
	SVAR wname=root:Packages:TonyIR:BaselineFitDataWaveName
	wave W_data=$wname
	
	
	SubtractBaseline(W_data) //, N_Index
	
end

// subtract current baseline from W_data
function SubtractBaseline(W_data) 
	
	wave W_data
		
	wave W_base=root:Packages:TonyIR:W_base
	if(numpnts(W_base)!=numpnts(W_data))
		doalert 0, nameofwave(W_data) +" and baseline have different length"
		return 0
	endif
	
	// save a copy of the baseline
	string strNewName=CleanupName( nameofwave(W_data)+"_BL",0)
	if (exists(strNewName))
		doalert 1, strNewName+" exists. Overwrite?"
		if(V_flag==2)
			return 0
		endif
	endif
	duplicate /o W_data $strNewName
	wave newbase= $strNewName
	newbase=W_base
	

	// subtract baseline
	strNewName=CleanupName( nameofwave(W_data)+"_Sub",0)
		if (exists(strNewName))
		doalert 1, strNewName+" exists. Overwrite?"
		if(V_flag==2)
			return 0
		endif
	endif
	
		
	duplicate /o W_data $strNewName
	wave subtracted= $strNewName
	subtracted=W_data-W_base+1
	print "SubtractBaseline("+nameofwave(W_data)+")"
	checkdisplayed subtracted
	if (V_flag)
		return 1
	else
		appendtograph subtracted		
	endif
	
	// subtract baseline - ait
	String strNewNameR
	strNewNameR=CleanupName( nameofwave(W_data)+"_Rat",0)
		if (exists(strNewNameR))
		doalert 1, strNewNameR+" exists. Overwrite?"
		if(V_flag==2)
			return 0
		endif
	endif
	
		
	duplicate /o W_data $strNewNameR
	wave ratio= $strNewNameR
//	ratio=W_data/W_base
	ratio=(subtracted-1)/W_base+1
	print "SubtractBaseline("+nameofwave(W_data)+")"
	checkdisplayed ratio
	if (V_flag)
		return 1
	else
		appendtograph ratio		
	endif


	
	KillControl SplineSmoothSetVar
 
	
end

// wrapper function to start baseline fit of type defined by menu item
// which is read as text into the S_value string variable
function MenuFitBaseline() //N_Index
//Variable N_Index	
	SVAR wname=root:Packages:TonyIR:BaselineFitDataWaveName
	wave w=$wname
	
	GetLastUserMenuInfo
	
	printf "FitBaseline(%s, \"%s\")\r", nameofwave(w), S_value

	FitBaseline(w, S_value) //,N_Index

end


// fit a baseline defined by type to the data wave W_data using predefined mask wave
// this function allows batch fitting from the command line 
function FitBaseline(W_data, type) //,N_Index
	wave w_data
	string type
//	Variable N_Index
	
	wave W_Base=root:Packages:TonyIR:W_Base
	wave W_display=root:Packages:TonyIR:W_display 
	wave W_mask=root:Packages:TonyIR:W_mask
	wave W_noBase=root:Packages:TonyIR:W_noBase
	
	if (stringmatch(type, "spline"))
		// remove hook in case we were fitting to cursors
		SetWindow $WinList("", "", "WIN:" )  hook=$"" 	
		
		W_display = W_mask[p] ? W_data[p] : NaN // reset W_display in case we're batch fitting
		
		NVAR V_smooth=root:Packages:TonyIR:V_smooth
		interpolate2/T=3/I=3/F=(V_smooth)/Y=root:Packages:TonyIR:W_Base root:Packages:TonyIR:W_display 
		
		SetVariable SplineSmoothSetVar title="Smoothing", pos={200,100}, size={100,16}
		SetVariable SplineSmoothSetVar labelBack=(65535,65535,65535)
		SetVariable SplineSmoothSetVar limits={0,1e6,0.1 }, value=root:Packages:TonyIR:V_smooth
		SetVariable SplineSmoothSetVar proc=BL_SplineSetVarProc
	elseif(stringmatch(type, "line between cursors"))
		
		// remove the spline smoothing control in case previous fit was a spline
		KillControl SplineSmoothSetVar
		
		variable Yval
		if ( strlen(CsrWave(J))==0 )
			getaxis /Q bottom
			Yval=w_data(V_max)
			if (numtype(Yval))
				getaxis /Q left
				Yval=(V_max-V_min)/2
			endif
			cursor  J $nameofwave(w_data) V_max //, w_data(V_max)
		else
			cursor  J $nameofwave(w_data) hcsr(J)
		endif
		if ( strlen(CsrWave(J))==0  )
			getaxis /Q bottom
			Yval=w_data(V_min)
			if (numtype(Yval))
				getaxis /Q left
				Yval=(V_max-V_min)/2
			endif
			cursor  I $nameofwave(w_data) V_min   //, Yval
		else
			cursor  I $nameofwave(w_data) hcsr(I)
		endif
		SetWindow $WinList("", "", "WIN:" ) hook=UpdateBaselineBetweenCursors, hookEvents=2^2
		UpdateBaselineBetweenCursors("")
	
	// ait
	elseif(stringmatch(type, "piecewise*"))
		variable k
		variable segmentNumber=0
		variable transNumber
		variable blSegmentSize1, blSegmentSize2
		string StartTermSegmentAvail
		
//		make/O/N=(numpnts (root:Packages:TonyIR:W_Mask)) W_Mask_copy
//		duplicate/O root:Packages:TonyIR:W_Mask, W_Mask_copy
		
		
		differentiate root:Packages:TonyIR:W_Mask/D=W_MaskDF
		duplicate W_MaskDF,W_MaskDFA
		WaveTransform/O abs W_MaskDFA
		duplicate W_MaskDFA,W_MaskDFAsr
		MakeIndex/R W_MaskDFAsr,W_MaskDFAsr
		duplicate/O/R=(0,2*sum(W_MaskDFA)-1) W_MaskDFAsr,W_MaskDFAE
		sort W_MaskDFAE, W_MaskDFAE
		transNumber=numpnts(W_MaskDFAE)/2 
//		make/N=(0,0) startingSegment=NaN
//		make/N=(0,0) terminalSegment=NaN
						
		if (W_mask[k]==1) // if the first point is a part of the baseline region
			if (round(sum(W_MaskDFA)/2)==sum(W_MaskDFA)/2) //if W_MaskDFA is an even number
				segmentNumber=sum(W_MaskDFA)/2
				StartTermSegmentAvail="no starting or terminal segments"
				InsertPoints 0,1, W_MaskDFAE
				InsertPoints (numpnts(W_MaskDFAE)+1),1, W_MaskDFAE
				W_MaskDFAE[numpnts(W_MaskDFAE)]=numpnts(w_data)
			else
				segmentNumber=(sum(W_MaskDFA)-1)/2
//				duplicate/R=(W_MaskDFAE[numpnts(W_MaskDFAE)],numpnts(w_data)) w_data, terminalSegment 
				InsertPoints 0,1, W_MaskDFAE
				DeletePoints (numpnts(W_MaskDFAE)-1),1, W_MaskDFAE
				StartTermSegmentAvail="Terminal segment"
			endif
		else
			if (round(sum(W_MaskDFA)/2)==sum(W_MaskDFA)/2) //if W_MaskDFA is an even number
				segmentNumber=sum(W_MaskDFA)/2-1
				StartTermSegmentAvail="Starting and Terminal segments"
//				duplicate/O/R=(0,W_MaskDFAE[0]) w_data, startingSegment 
//				duplicate/O/R=(W_MaskDFAE[numpnts(W_MaskDFAE)],numpnts(w_data)) w_data, terminalSegment
				DeletePoints 0,1, W_MaskDFAE
				DeletePoints (numpnts(W_MaskDFAE)-1),1, W_MaskDFAE
			else
				segmentNumber=(sum(W_MaskDFA)-1)/2
				StartTermSegmentAvail="Starting segment"
//				duplicate/O/R=(0,W_MaskDFAE[0]) w_data, startingSegment 
				InsertPoints (numpnts(W_MaskDFAE)+1),1, W_MaskDFAE
				DeletePoints 0,1, W_MaskDFAE
				W_MaskDFAE[numpnts(W_MaskDFAE)]=numpnts(w_data)
			endif
		endif
//		print	num2str(segmentNumber)+ " segments,"+StartTermSegmentAvail
		
			
		for(k=1; k<numpnts(W_MaskDFAE)-1;k+=2) // if the baseline interval has odd number of points, delete one point from the interval
			if (round((W_MaskDFAE[k]+W_MaskDFAE[k-1])/2)==(W_MaskDFAE[k]+W_MaskDFAE[k-1])/2)
				W_MaskDFAE[k]=W_MaskDFAE[k]-1
//				print "correcting baseline interval No"+num2str((k-1)/2+1)
			endif
			DeletePoints k+1,2, W_MaskDFAE
		endfor
				
		for(k=1; k<=segmentNumber;k+=1)
			if(k==1)
//				duplicate/O/R=(W_MaskDFAE[0],floor((W_MaskDFAE[2]+W_MaskDFAE[3])/2)) w_data, w_segment
//				duplicate/O/R=(W_MaskDFAE[0],floor((W_MaskDFAE[2]+W_MaskDFAE[3])/2)) root:Packages:TonyIR:W_mask, w_mask_segment
				duplicate/O/R=(0,floor((W_MaskDFAE[2]+W_MaskDFAE[3])/2)) w_data, w_segment
				duplicate/O/R=(0,floor((W_MaskDFAE[2]+W_MaskDFAE[3])/2)) root:Packages:TonyIR:W_mask, w_mask_segment
				if (stringmatch(type, "*square")|stringmatch(type, "*cubic"))
					blSegmentSize1=W_MaskDFAE[1]-W_MaskDFAE[0]
					blSegmentSize2=floor((W_MaskDFAE[2]+W_MaskDFAE[3])/2)
					w_mask_segment[round(BLsegmentSize1/2)]=0
					w_mask_segment[numpnts(w_mask_segment)-1-round(BLsegmentSize2/2)]=0
				endif
//				print W_MaskDFAE[0]
//				print floor((W_MaskDFAE[2]+W_MaskDFAE[3])/2)
			elseif (k==segmentNumber)
//				duplicate/O/R=(ceil((W_MaskDFAE[2*k-2]+W_MaskDFAE[2*k-1])/2),W_MaskDFAE[2*k+1]) w_data, w_segment
//				duplicate/O/R=(ceil((W_MaskDFAE[2*k-2]+W_MaskDFAE[2*k-1])/2),W_MaskDFAE[2*k+1]) root:Packages:TonyIR:W_mask, w_mask_segment
				duplicate/O/R=(ceil((W_MaskDFAE[2*k-2]+W_MaskDFAE[2*k-1])/2),numpnts(w_data)-1) w_data, w_segment
				duplicate/O/R=(ceil((W_MaskDFAE[2*k-2]+W_MaskDFAE[2*k-1])/2),numpnts(w_data)-1) root:Packages:TonyIR:W_mask, w_mask_segment
//				print ceil((W_MaskDFAE[2*segmentNumber]+W_MaskDFAE[2*segmentNumber-1])/2)
				if (stringmatch(type, "*square")|stringmatch(type, "*cubic"))
					blSegmentSize2=W_MaskDFAE[2*k+1]-W_MaskDFAE[2*k]
					blSegmentSize1=floor((W_MaskDFAE[2*k-2]+W_MaskDFAE[2*k-1])/2)
					w_mask_segment[round(BLsegmentSize1/2)]=0
					w_mask_segment[numpnts(w_mask_segment)-1-round(BLsegmentSize2/2)]=0
				endif
			else
				duplicate/O/R=(ceil((W_MaskDFAE[2*k-2]+W_MaskDFAE[2*k-1])/2),floor((W_MaskDFAE[2*(k+1)-2]+W_MaskDFAE[2*(k+1)-1])/2)) w_data, w_segment
				duplicate/O/R=(ceil((W_MaskDFAE[2*k-2]+W_MaskDFAE[2*k-1])/2),floor((W_MaskDFAE[2*(k+1)-2]+W_MaskDFAE[2*(k+1)-1])/2)) root:Packages:TonyIR:W_mask, w_mask_segment
//				print ceil((W_MaskDFAE[2*k-2]+W_MaskDFAE[2*k-1])/2)
//				print floor((W_MaskDFAE[2*(k+1)-2]+W_MaskDFAE[2*(k+1)-1])/2)
				if (stringmatch(type, "*square")|stringmatch(type, "*cubic"))
					blSegmentSize2=floor((W_MaskDFAE[2*(k+1)-1]-W_MaskDFAE[2*(k+1)-2])/2)
					blSegmentSize1=ceil((W_MaskDFAE[2*k-1]-W_MaskDFAE[2*k-2])/2)
					w_mask_segment[round(BLsegmentSize1/2)]=0
					w_mask_segment[numpnts(w_mask_segment)-1-round(BLsegmentSize2/2)]=0
				endif
			endif
			
			duplicate/O w_segment, w_segment_base
			duplicate/O w_segment, w_segment_bs
			

			if (stringmatch(type, "*linear"))
				CurveFit/NTHR=0/Q line, w_segment /M=w_mask_segment //, s_hold, type, GetWavesDataFolder(w_data, 2)
				wave CoefWave=$GetWavesDataFolder(W_coef,2)
				w_segment_base= CoefWave[0]+CoefWave[1]*(p+DimOffset(w_segment,0))
			elseif (stringmatch(type, "*exp"))
				CurveFit/NTHR=0/Q exp, w_segment /M=w_mask_segment //, s_hold, type, GetWavesDataFolder(w_data, 2)
				wave CoefWave=$GetWavesDataFolder(W_coef,2)
				w_segment_base= CoefWave[0]+CoefWave[1]*exp(CoefWave[2]*(p+DimOffset(w_segment,0)))
			elseif (stringmatch(type, "*square"))
				CurveFit/NTHR=0/Q poly 3, w_segment /M=w_mask_segment //, s_hold, type, GetWavesDataFolder(w_data, 2)
				wave CoefWave=$GetWavesDataFolder(W_coef,2)
				w_segment_base= CoefWave[0]+CoefWave[1]*(p+DimOffset(w_segment,0))+CoefWave[2]*(p+DimOffset(w_segment,0))^2
			else 
				CurveFit/NTHR=0/Q poly 4, w_segment /M=w_mask_segment //, s_hold, type, GetWavesDataFolder(w_data, 2)
				wave CoefWave=$GetWavesDataFolder(W_coef,2)
				w_segment_base= CoefWave[0]+CoefWave[1]*(p+DimOffset(w_segment,0))+CoefWave[2]*(p+DimOffset(w_segment,0))^2+CoefWave[3]*(p+DimOffset(w_segment,0))^3
			endif

			w_segment_bs=w_segment-w_segment_base
			
			if (WaveExists(w_lbase)==1)
				duplicate/O w_lbase,w_lbase_d
				concatenate/O {w_lbase_d,w_segment_base}, w_lbase
			else
				duplicate w_segment_base,w_lbase
			endif
			if (WaveExists(w_bs)==1)
				duplicate/O w_bs, w_bs_d
				concatenate/O {w_bs_d,w_segment_bs}, w_bs
			else
				duplicate w_segment_bs,w_bs
			endif
		endfor
//		duplicate/O w_bs,w_bs_d
//		duplicate/O w_lbase, w_lbase_d
//		duplicate/O startingSegment, startBL
//		duplicate/O terminalSegment, termBL
//		startBL=1
//		termBL=1
//		concatenate/O {startBL, w_lbase_d, termBL},w_lbase
		duplicate/O w_lbase, root:Packages:TonyIR:W_base 
//		concatenate/O {startingSegment, w_bs_d, terminalSegment},w_lbs
//		duplicate/O w_bs,w_lbs
		
		KillWaves/Z W_MaskDFAsr, W_sigma,startBL, termBL, W_coef, w_lbase_d, w_bs_d, w_mask_segment, startingSegment, terminalSegment, w_segment, w_lbase, w_segment_base, w_segment_bs, W_MaskDFAE, W_MaskDFA, W_MaskDF  
		
				
//		CurveFit/NTHR=0 /Q %s %s , %s /M=root:Packages:TonyIR:W_mask", s_hold, type, GetWavesDataFolder(w_data, 2)
		
		
	else
		// remove the spline smoothing control in case previous fit was a spline
		KillControl SplineSmoothSetVar
		// remove hook in case we were fitting to cursors
		SetWindow $WinList("", "", "WIN:" )  hook=$"" 
	
		string s_hold=""
		if (stringmatch(type, "Tony's gauss"))
			s_hold="/H=\"1000\""
			type="gauss"
			execute /Q "K0 = 0"
		endif
		
		string cmd=""
		variable i
		wave w_mask=root:Packages:TonyIR:W_mask
		variable mask1=0
		for(i=0;i<numpnts(W_mask); i+=1)
			if(w_mask[i]!=mask1)
				mask1=1-mask1
				if(mask1) // started to include
					cmd += num2str(round(pnt2x(w_mask, i)))
				else // stopped including
					cmd+="-"+num2str(round(pnt2x(w_mask, i-1)))+" "
				endif
			elseif( (i==numpnts(w_mask)-1) && mask1)
				cmd+="-"+num2str(round(pnt2x(w_mask, i)))
			endif
		endfor
		print "fitting "+type+" baseline to "+nameofwave(w_data)+" at "+cmd
	
	
		sprintf cmd, "CurveFit/NTHR=0 /Q %s %s , %s /M=root:Packages:TonyIR:W_mask", s_hold, type, GetWavesDataFolder(w_data, 2)
		execute cmd
		
		wave W_coef=W_coef
		
		type=ReplaceString("gauss", type, "Gauss1D")
		type=ReplaceString("line", type, "BL_line")
		type=ReplaceString("lor", type, "BL_lor")
		type=ReplaceString("exp", type, "BL_exp")
		type=ReplaceString("3", type, "")
		type=ReplaceString("4", type, "")
		type=ReplaceString("sin", type, "BL_sin")
		type=ReplaceString("sigmoid", type, "BL_sigmoid")
		type=ReplaceString("5", type, "") //ait
		type=ReplaceString("6", type, "") //ait
		type=ReplaceString("7", type, "") //ait
		type=ReplaceString("8", type, "") //ait
		type=ReplaceString("9", type, "") //ait
		type=ReplaceString("10", type, "") //ait
		type=ReplaceString("11", type, "") //ait
		type=ReplaceString("12", type, "") //ait
		type=ReplaceString("13", type, "") //ait
		type=ReplaceString("14", type, "") //ait
		type=ReplaceString("15", type, "") //ait
		type=ReplaceString("16", type, "") //ait
		type=ReplaceString("17", type, "") //ait
		type=ReplaceString("18", type, "") //ait
		type=ReplaceString("19", type, "") //ait
		type=ReplaceString("20", type, "") //ait
		type=ReplaceString("piecewise linear", type,"pwBL_line") //ait		
		sprintf cmd, "root:Packages:TonyIR:W_base = %s(%s, x)", type, GetWavesDataFolder(W_coef, 2)
		execute cmd	
	endif
	
	if (stringmatch(type, "piecewise linear"))
		W_noBase=w_bs
		KillWaves/Z w_bs
	else
		W_noBase=W_data-W_base
	endif
	
end



Function BL_SplineSetVarProc(ctrlName,varNum,varStr,varName) : SetVariableControl
	String ctrlName
	Variable varNum
	String varStr
	String varName
	
	interpolate2/T=3/I=3/F=(varNum)/Y=root:Packages:TonyIR:W_Base root:Packages:TonyIR:W_display 
	
	SVAR S_name=root:Packages:TonyIR:BaselineFitDataWaveName
	
	wave W_Data=$S_name
	wave W_noBase=root:Packages:TonyIR:W_noBase
	wave W_base=root:Packages:TonyIR:W_base
	
	W_noBase=W_data-W_base
End




// use these functions to fill the baseline wave
function BL_line(w, x)
	wave w; variable x	
	return w[0]+w[1]*x
end

function BL_exp(W_coef, x)
	wave W_coef; variable x
	return W_coef[0]+W_coef[1]*exp(-W_coef[2]*x)
end

function BL_lor(W_coef, x)
	wave W_coef; variable x
	return W_coef[0]+W_coef[1]/((x-W_coef[2])^2+W_coef[3])
end


//function BL_exp_XOffset(W_coef, x)
//	wave W_coef; variable x
//	return W_coef[0]+W_coef[1]*exp(-(x-W_fitConstants[0])/W_coef[2])
//end
//	
//	
//function BL_dblexp_XOffset(W_coef, x)
//	wave W_coef; variable x
//	return 
//end

function BL_sin(W_coef, x)
	wave W_coef; variable x
	return W_coef[0]+W_coef[1]*sin(W_coef[2]*x+W_coef[3])
end

function BL_sigmoid(W_coef, x)
	wave W_coef; variable x
	return W_coef[0] + W_coef[1]/(1+exp(-(x-W_coef[2])/W_coef[3]))
end


function FitAllTraces() //N_Index
//Variable N_Index
	GetLastUserMenuInfo
	
	string ListOfTraces=TraceNameList("", ";", 1 )
	string NonDataTraces="W_Display;W_Base;W_NoBase"
	string strDataWave
	string strDataWaveNew
	ListOfTraces=removefromlist(NonDataTraces,ListOfTraces, ";", 0)
	
	Variable i=1
	String MaskCopy="Mask_b00"
	do
	if(waveexists($MaskCopy)==1)
		if(i<10)
			MaskCopy=RemoveEnding(RemoveEnding(MaskCopy))+num2str(0)+num2str(i)
		else
			MaskCopy=RemoveEnding(RemoveEnding(MaskCopy))+num2str(i)
		endif
	else
		break
	endif
	i=i+1
	while(1)
	
	make/O/N=(numpnts (root:Packages:TonyIR:W_Mask)) $MaskCopy
	duplicate/O root:Packages:TonyIR:W_Mask, $MaskCopy
	
	
	do
		wave W_data=tracenametowaveref("", StringFromList(0, ListOfTraces))
		
		//rename mother wave -- ait
		strDataWave=StringFromList(0, ListOfTraces)
		strDataWaveNew=(strDataWave+"_Ini")
				
		ListOfTraces=RemoveListItem(0, ListOfTraces)
		FitBaseline(W_data, S_value) //,N_Index
		SubtractBaseline(W_data) //,N_Index
		Rename $strDataWave, $strDataWaveNew
		
	while (itemsinlist(ListOfTraces))

end


// can use this function from data browser (Execute Cmd...). N defines baseline type:
 // 1 line
 // 2 poly 3
 // 3 poly 4
 // 4 gauss
 // 5 lor
 // 6 exp
 // 7 Tony's gauss
 // 8 poly 5 -- ait
 // 9 poly 6 -- ait
 // 10 poly 7 -- ait
 // 11 poly 8 -- ait
 // 12 poly 9 -- ait
 // 13 poly 10 -- ait
 // 14 poly 11 -- ait
 // 15 poly 12 -- ait
 // 16 poly 13 -- ait
 // 17 poly 14 -- ait
 // 18 poly 15 -- ait
 // 19 poly 16 -- ait
 // 20 poly 17 -- ait
 // 21 poly 18 -- ait
 // 22 poly 19 -- ait
 // 23 poly 20 -- ait
 
 
function FitAndGo(W_data, N) //,N_Index
	wave W_data; variable N; 
	//Variable N_Index
	
	string FitTypeList="line;poly 3;poly 4;gauss;lor;exp;Tony's gauss"
	string type=StringFromList(N-1, FitTypeList)
	
	 FitBaseline(W_data, type) //,N_Index
	 SubtractBaseline(W_data) //,N_Index
end

function OffsetFromZero(W_data, Offset)
	wave W_data
	variable Offset
	
	variable MinVal=wavemin(W_data)
	
	W_data=W_data-MinVal+Offset
end

function alignSpectra(ref, wavenumber, w_data)
	wave ref, w_data
	variable wavenumber
	
	variable offset=w_data(wavenumber)-ref(wavenumber)
	W_data-=offset
end


function UpdateBaselineBetweenCursors(infoStr)
	string infoStr
	if (strlen(infoStr))
		String event= StringByKey("EVENT",infoStr)
		if (!stringmatch(event, "cursormoved"))
			return 0
		endif
	endif	
	
	variable x1=hcsr(I), x2=hcsr(J), y1=vcsr(I), y2=vcsr(J)
	wave W_Base=root:Packages:TonyIR:W_Base	
	wave W_noBase=root:Packages:TonyIR:W_noBase
	SVAR S_name=root:Packages:TonyIR:BaselineFitDataWaveName
	wave W_Data=$S_name

	W_Base=y1+(x-x1)*(y2-y1)/(x2-x1)
		
	W_noBase=W_data-W_base

	return 1
end


//***********************************************

function MenuQuantifyRegions() //N_Index
//Variable N_Index	
	SVAR wname=root:Packages:TonyIR:BaselineFitDataWaveName
	wave w=$wname
	
	
	
	GetLastUserMenuInfo
	
	printf "Quantify Regions(%s, \"%s\")\r", nameofwave(w), S_value

	QuantifyRegions(w, S_value) //,N_Index
	
	
	
	
end


function QuantifyRegions(W_data, type) //,N_Index
// Needs to have at least two points between the neighbouring regions, otherwise may confuse the 'in' and 'out'
// regions

	wave w_data
	string type
//	Variable N_Index
	
	wave W_Base=root:Packages:TonyIR:W_Base
	wave W_display=root:Packages:TonyIR:W_display 
	wave W_mask=root:Packages:TonyIR:W_mask
	wave W_noBase=root:Packages:TonyIR:W_noBase
	
	string  IntervalSubName=CleanupName( nameofwave(W_data)+"_iSub",0)
	string  IntervalRatName=CleanupName( nameofwave(W_data)+"_iRat",0)
//	duplicate /o W_data $strNewName
//	wave newbase= $strNewName
	
	String FirstRun = "yes"

Variable Offset=0
NVAR/Z DefOffset

if(!NVAR_exists(DefOffset)==0)
Offset=DefOffset
endif

	
	
		variable k
		variable segmentNumber=0
		variable transNumber
//		variable blSegmentSize1, blSegmentSize2
		string StartTermSegmentAvail
		
//		make/O/N=(numpnts (root:Packages:TonyIR:W_Mask)) W_Mask_copy
//		duplicate/O root:Packages:TonyIR:W_Mask, W_Mask_copy
		
		
		differentiate root:Packages:TonyIR:W_Mask/D=W_MaskDF
		duplicate W_MaskDF,W_MaskDFA
		WaveTransform/O abs W_MaskDFA
		duplicate W_MaskDFA,W_MaskDFAsr
		MakeIndex/R W_MaskDFAsr,W_MaskDFAsr
		duplicate/O/R=(0,2*sum(W_MaskDFA)-1) W_MaskDFAsr,W_MaskDFAE
		sort W_MaskDFAE, W_MaskDFAE
		transNumber=numpnts(W_MaskDFAE)/2 

						
		if (W_mask[k]==1) // if the first point is a part of the baseline region
			if (round(sum(W_MaskDFA)/2)==sum(W_MaskDFA)/2) //if W_MaskDFA is an even number
				segmentNumber=sum(W_MaskDFA)/2
				StartTermSegmentAvail="no starting or terminal segments"
				InsertPoints 0,1, W_MaskDFAE
				InsertPoints (numpnts(W_MaskDFAE)+1),1, W_MaskDFAE
				W_MaskDFAE[numpnts(W_MaskDFAE)]=numpnts(w_data)
			else
				segmentNumber=(sum(W_MaskDFA)-1)/2
//				duplicate/R=(W_MaskDFAE[numpnts(W_MaskDFAE)],numpnts(w_data)) w_data, terminalSegment 
				InsertPoints 0,1, W_MaskDFAE
				DeletePoints (numpnts(W_MaskDFAE)-1),1, W_MaskDFAE
				StartTermSegmentAvail="Terminal segment"
			endif
		else
			if (round(sum(W_MaskDFA)/2)==sum(W_MaskDFA)/2) //if W_MaskDFA is an even number
				segmentNumber=sum(W_MaskDFA)/2-1
				StartTermSegmentAvail="Starting and Terminal segments"
//				duplicate/O/R=(0,W_MaskDFAE[0]) w_data, startingSegment 
//				duplicate/O/R=(W_MaskDFAE[numpnts(W_MaskDFAE)],numpnts(w_data)) w_data, terminalSegment
				DeletePoints 0,1, W_MaskDFAE
				DeletePoints (numpnts(W_MaskDFAE)-1),1, W_MaskDFAE
			else
				segmentNumber=(sum(W_MaskDFA)-1)/2
				StartTermSegmentAvail="Starting segment"
//				duplicate/O/R=(0,W_MaskDFAE[0]) w_data, startingSegment 
				InsertPoints (numpnts(W_MaskDFAE)+1),1, W_MaskDFAE
				DeletePoints 0,1, W_MaskDFAE
				W_MaskDFAE[numpnts(W_MaskDFAE)]=numpnts(w_data)
			endif
		endif
//		print	num2str(segmentNumber+1)+ " intervals,"+StartTermSegmentAvail
		make/N=(segmentNumber+1) $IntervalSubName
		make/N=(segmentNumber+1) $IntervalRatName 
		wave isub=$IntervalSubName
		wave irat=$IntervalRatName
		
		if ((WaveExists(LabelText)==1)&(WaveExists(LabelTimes)==1)&(WaveExists(GroupLabels)==0))
			FirstRun = "yes"
			make/T/N=(segmentNumber) GroupLabels
			print "generating group labels"
			wave/T lText=LabelText
			wave/T gLabels=Grouplabels 
			duplicate LabelTimes, lTimes
			wave lTimes=LabelTimes
			lTimes=lTimes+Offset
		else
//			print "group labels already exist"
			FirstRun = "no"
		endif			
		
		Variable l=0
		String CurrentLabel="early label"
				
		for(k=0; k<=segmentNumber;k+=1)
//				print k
//				print W_MaskDFAE[4*k]
//				print W_MaskDFAE[4*k+1]
				duplicate/O/R=(W_MaskDFAE[4*k],W_MaskDFAE[4*k+1]) w_data, w_segment
//				print "interval points"+W_MaskDFAE[4*k]+" : "+W_MaskDFAE[4*k+1]
				print w_segment
			if (stringmatch(type, "*linear"))
				CurveFit/NTHR=0/Q line, w_segment //, s_hold, type, GetWavesDataFolder(w_data, 2)
				wave CoefWave=$GetWavesDataFolder(W_coef,2)
				duplicate/O w_segment, w_segment_base
				w_segment_base= CoefWave[0]+CoefWave[1]*(p+DimOffset(w_segment,0))
				isub[k]=w_segment_base[numpnts(w_Segment_base)-1]-w_segment_base[0]
				irat[k]=w_segment_base[numpnts(w_Segment_base)-1]/w_segment_base[0]

			elseif (stringmatch(type, "*sigmoid"))
				CurveFit/NTHR=0/Q sigmoid, w_segment  //, s_hold, type, GetWavesDataFolder(w_data, 2)
				wave CoefWave=$GetWavesDataFolder(W_coef,2)
				isub[k]=CoefWave[1]*exp(CoefWave[2])/(1+exp(CoefWave[2]))
				irat[k]=(CoefWave[0]+CoefWave[1])/(CoefWave[0]-CoefWave[1]/(1+exp(CoefWave[2])))
			elseif (stringmatch(type, "*Hill"))
				CurveFit/NTHR=0/Q hillequation, w_segment  //, s_hold, type, GetWavesDataFolder(w_data, 2)
				wave CoefWave=$GetWavesDataFolder(W_coef,2)
				isub[k]= CoefWave[1]-CoefWave[0]
				irat[k]=CoefWave[1]/CoefWave[0]
				 
			elseif (stringmatch(type, "*end*"))
//				Smooth 5, Rm5R, w_segment
				Smooth 5, w_segment
				isub[k]=  w_segment[numpnts(w_segment)-1]-w_segment[0]
				irat[k]=w_segment[numpnts(w_segment)-1]/w_segment[0]
				
				
			
			elseif (stringmatch(type, "*average*"))
//				Smooth 5, Rm5R, w_segment
				Smooth 5, w_segment
				isub[k]=mean(w_segment)// [numpnts(w_segment)-1]-w_segment[0]
				irat[k]=mean(w_segment)//[numpnts(w_segment)-1]/w_segment[0]
//				print isub[k]			
//				print k
//				print w_segment
			else 
			endif
			
//			if ((WaveExists(LabelText)==1)&(WaveExists(LabelTimes)==1))
			if (stringmatch(FirstRun,"yes")==1)
				for(l=0; l<=numpnts(LabelTimes)+1;l+=1)
					if((lTimes[l]>=W_MaskDFAE[4*k])&(lTimes[l]<=W_MaskDFAE[4*k+1]))
						CurrentLabel=lText[l]
						print CurrentLabel
						gLabels[k]=lText[l]
//						gLabels[k]=CurrentLabel
					endif
				endfor
			endif
				
				
		endfor
//		print w_data
	

		
		KillWaves/Z W_MaskDFAsr, W_sigma,startBL, termBL, W_coef, w_lbase_d, w_bs_d, w_mask_segment, startingSegment, terminalSegment, w_segment, w_lbase, w_segment_base, w_segment_bs, W_MaskDFA, W_MaskDF, W_MaskDFAE  
		
end



function QuantifyAllRegions()

GetLastUserMenuInfo
	
	string ListOfTraces=TraceNameList("", ";", 1 )
	string NonDataTraces="W_Display;W_Base;W_NoBase"
	string strDataWave
	string strDataWaveNew
	
	Variable i=1
				
	String MaskCopy="Mask_q00"
	do
	if(waveexists($MaskCopy)==1)
		if(i<10)
			MaskCopy=RemoveEnding(RemoveEnding(MaskCopy))+num2str(0)+num2str(i)
		else
			MaskCopy=RemoveEnding(RemoveEnding(MaskCopy))+num2str(i)
		endif
	else
		break
	endif
	i=i+1
	while(1)
	
	make/O/N=(numpnts (root:Packages:TonyIR:W_Mask)) $MaskCopy
	duplicate/O root:Packages:TonyIR:W_Mask, $MaskCopy
	
	ListOfTraces=removefromlist(NonDataTraces,ListOfTraces, ";", 0)
	
	i=1
	
	do
		wave W_data=tracenametowaveref("", StringFromList(0, ListOfTraces))
		
		//rename mother wave -- ait
		strDataWave=StringFromList(0, ListOfTraces)
		strDataWaveNew=(strDataWave+"_iIni")
		print strDataWave	
		ListOfTraces=RemoveListItem(0, ListOfTraces)
		QuantifyRegions(W_data, S_value) //,N_Index
//		SubtractBaseline(W_data) //,N_Index
		Rename $strDataWave, $strDataWaveNew
	i=i+1
	print i	
	while (itemsinlist(ListOfTraces))

end



//*******************************************

function MenuCorrectArtifact() //N_Index
//Variable N_Index	
	SVAR wname=root:Packages:TonyIR:BaselineFitDataWaveName
	wave w=$wname
	
	GetLastUserMenuInfo
	
	printf "FitBaseline(%s, \"%s\")\r", nameofwave(w), S_value

	CorrectArtifact(w, S_value) //,N_Index

end


function CorrectArtifact(W_data, type) //,N_Index
	wave w_data
	string type
//	Variable N_Index
	
	wave W_Base=root:Packages:TonyIR:W_Base
	wave W_display=root:Packages:TonyIR:W_display 
	wave W_mask=root:Packages:TonyIR:W_mask
	wave W_noBase=root:Packages:TonyIR:W_noBase
	
	string  IntervalSubName=CleanupName(nameofwave(W_data)+"_Sub",0)
	string  IntervalRatName=CleanupName(nameofwave(W_data)+"_Rat",0)
	
		
		variable k
		variable segmentNumber=0
		variable transNumber
		string StartTermSegmentAvail
		
//		make/O/N=(numpnts (root:Packages:TonyIR:W_Mask)) W_Mask_copy
//		duplicate/O root:Packages:TonyIR:W_Mask, W_Mask_copy
		
		
		differentiate root:Packages:TonyIR:W_Mask/D=W_MaskDF
		duplicate W_MaskDF,W_MaskDFA
		WaveTransform/O abs W_MaskDFA
		duplicate/O W_MaskDFA,W_MaskDFAsr
		MakeIndex/R W_MaskDFAsr,W_MaskDFAsr
		duplicate/O/R=(0,2*sum(W_MaskDFA)-1) W_MaskDFAsr,W_MaskDFAE
		sort W_MaskDFAE, W_MaskDFAE
		transNumber=numpnts(W_MaskDFAE)/2 

				segmentNumber=sum(W_MaskDFA)/2-1
				StartTermSegmentAvail="Starting and Terminal segments"
				DeletePoints 0,1, W_MaskDFAE
				DeletePoints (numpnts(W_MaskDFAE)-1),1, W_MaskDFAE

//		print	num2str(segmentNumber+1)+ " intervals,"+StartTermSegmentAvail
//		make/N=(segmentNumber+1) $IntervalSubName
		duplicate w_data,$IntervalSubName	
		duplicate w_data,$IntervalRatName	
		
k=0				
		for(k=0; k<=segmentNumber;k+=1)
				duplicate/O/R=(0,W_MaskDFAE[4*k]-1) $IntervalSubName, startingSegment
				duplicate/O/R=(W_MaskDFAE[4*k],W_MaskDFAE[4*k+1]-2) $IntervalSubName, insertSegment
				duplicate/O/R=(W_MaskDFAE[4*k+1]-1,numpnts(w_data)-1) $IntervalSubName, terminalSegment
//				Smooth 5, Rm5R, w_segment
				duplicate/O terminalSegment, terminalSegmentSub
				duplicate/O terminalSegment, terminalSegmentRat
				terminalSegmentSub=terminalSegment-(insertSegment[numpnts(insertSegment)-1]-insertSegment[0])
				terminalSegmentRat=terminalSegment/(insertSegment[numpnts(insertSegment)-1]/insertSegment[0])
				concatenate/O {startingSegment,insertSegment,terminalSegmentSub}, $IntervalSubName
				concatenate/O {startingSegment,insertSegment,terminalSegmentRat}, $IntervalSubName
		endfor

		
		KillWaves/Z W_MaskDFAsr, W_sigma,startBL, termBL, W_coef, w_lbase_d, w_bs_d, w_mask_segment, startingSegment, terminalSegment, w_segment, w_lbase
		KillWaves/Z w_segment_base, w_segment_bs, W_MaskDFA, W_MaskDF, W_MaskDFAE, insertSegment, terminalSegmentSub, terminalSegmentRat 
		
end

//***************************

function CorrectAllArtifacts()
GetLastUserMenuInfo
	
	string ListOfTraces=TraceNameList("", ";", 1 )
	string NonDataTraces="W_Display;W_Base;W_NoBase"
	string strDataWave
	string strDataWaveNew
	Variable i=1
	
	String MaskCopy="Mask_c0"
	do
	if(waveexists($MaskCopy)==1)
		MaskCopy=RemoveEnding(MaskCopy)+num2str(i)
	else
		break
	endif
	i=i+1
	while(1)
	
	make/O/N=(numpnts (root:Packages:TonyIR:W_Mask)) $MaskCopy
	duplicate/O root:Packages:TonyIR:W_Mask, $MaskCopy
	
	ListOfTraces=removefromlist(NonDataTraces,ListOfTraces, ";", 0)
	
	do
		wave W_data=tracenametowaveref("", StringFromList(0, ListOfTraces))
		
		//rename mother wave -- ait
		strDataWave=StringFromList(0, ListOfTraces)
		strDataWaveNew=(strDataWave+"_Ini")
				
		ListOfTraces=RemoveListItem(0, ListOfTraces)
		CorrectArtifact(W_data, S_value) //,N_Index
//		SubtractBaseline(W_data) //,N_Index
		Rename $strDataWave, $strDataWaveNew
		
	while (itemsinlist(ListOfTraces))


end



//*******************************

function TranslateBaseline()

Variable TransPoints=0
Variable i=0

String MaskCopy="Mask_t0"
	do
	if(waveexists($MaskCopy)==1)
		MaskCopy=RemoveEnding(MaskCopy)+num2str(i)
	else
		break
	endif
	i=i+1
	while(1)
	
	make/O/N=(numpnts (root:Packages:TonyIR:W_Mask)) $MaskCopy
	duplicate/O root:Packages:TonyIR:W_Mask, $MaskCopy

Prompt TransPoints, "Number of points to translate to the right?"
DoPrompt "", TransPoints //
	if (V_Flag)
		return 0
	endif

duplicate/O root:Packages:TonyIR:W_Mask, W_Mask_dupl
if (TransPoints>0)
	InsertPoints 0,TransPoints, W_Mask_dupl
	DeletePoints (numpnts(W_Mask_dupl)-TransPoints-1),TransPoints, W_Mask_dupl
elseif (TransPoints<0)
	InsertPoints (numpnts(W_Mask_dupl)-1),-TransPoints, W_Mask_dupl
	DeletePoints 0,-TransPoints, W_Mask_dupl
else
endif
duplicate/O W_Mask_dupl, root:Packages:TonyIR:W_Mask
KillWaves/Z W_Mask_dupl

MenuInitBaselineFitting()
end

//*******************************

function InvokeRegions()

SVAR wname=root:Packages:TonyIR:BaselineFitDataWaveName
wave w=$wname
	
String NameOfMaskWave
prompt NameOfMaskWave, "Mask wave?"  popup, WaveList("*ask*",";","MAXCOLS:1,MINROWS:"+ num2str(dimsize($wname,0)))// 

DoPrompt "", NameOfMaskWave //
	if (V_Flag)
		return 0
	endif

duplicate/O $NameOfMaskWave, root:Packages:TonyIR:W_Mask

MenuInitBaselineFitting()

end

//**********************************

function DilateRegions()

Variable DilPointsR=0
Variable DilPointsL=0
Variable i=1

String MaskCopy="Mask_d0"
	do
	if(waveexists($MaskCopy)==1)
		MaskCopy=RemoveEnding(MaskCopy)+num2str(i)
	else
		break
	endif
	i=i+1
	while(1)
	
	make/O/N=(numpnts (root:Packages:TonyIR:W_Mask)) $MaskCopy
	duplicate/O root:Packages:TonyIR:W_Mask, $MaskCopy

Prompt DilPointsR, "Number of points to dilate/erode by to the right?"
Prompt DilPointsL, "Number of points to dilate/erode by to the left?"

DoPrompt "", DilPointsR, DilPointsL //
	if (V_Flag)
		return 0
	endif

duplicate/O root:Packages:TonyIR:W_Mask, W_Mask_dupl
duplicate/O root:Packages:TonyIR:W_Mask, W_Mask_duplR
duplicate/O root:Packages:TonyIR:W_Mask, W_Mask_duplL

if (DilPointsR>0)
	InsertPoints 0,DilPointsR, W_Mask_duplR
	DeletePoints (numpnts(W_Mask_dupl)-DilPointsR-1),DilPointsR, W_Mask_duplR
	W_Mask_dupl=W_Mask_dupl+W_Mask_duplR
elseif (DilPointsR<0)
	InsertPoints (numpnts(W_Mask_dupl)-1),-DilPointsR, W_Mask_duplR
	DeletePoints 0,-DilPointsR, W_Mask_duplR
	W_Mask_dupl=W_Mask_dupl*W_Mask_duplR
else
endif

if (DilPointsL>0)
	InsertPoints (numpnts(W_Mask_dupl)-1),DilPointsL, W_Mask_duplL
	DeletePoints 0,DilPointsL, W_Mask_duplL
	W_Mask_dupl=W_Mask_dupl+W_Mask_duplL
elseif (DilPointsL<0)
	InsertPoints 0,-DilPointsL, W_Mask_duplL
	DeletePoints (numpnts(W_Mask_dupl)+DilPointsL-1),DilPointsL, W_Mask_duplL
	W_Mask_dupl=W_Mask_dupl*W_Mask_duplL
else
endif

W_Mask_dupl=(sign(W_Mask_dupl-1)+1)/2


//i=0
//for (i=0;i<=numpnts(W_Mask_dupl);i+=1)
//	if (
//endfor


duplicate/O W_Mask_dupl, root:Packages:TonyIR:W_Mask
KillWaves/Z W_Mask_dupl, W_Mask_duplR, W_Mask_duplL

MenuInitBaselineFitting()



end
