run("Find Maxima...", "noise=20 output=[Maxima Within Tolerance] exclude");
run("Analyze Particles...", "add");
close();
roiManager("Select", 0);

for(i=0;i<roiManager("count");i++)
{
	roiManager("select",i);
	run("Enlarge...", "enlarge=3 pixel");
	run("Interpolate", "interval=2 smooth adjust");
	roiManager("Update");	
}