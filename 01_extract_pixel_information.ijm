/*This script extract pixel information from
stipe images in folders*/

// Core function
function action(input, output, filename) {
//Open image
	open(input + filename);
// Convert to HSB
	run("HSB Stack");
// Get Saturation channel
	setSlice(2);
// Set threshold to select stripes
	setThreshold(80, 255);
// Select stripes and ignore small particles
	run("Analyze Particles...", "size=1000-Infinity add slice");
	n = roiManager("count");
// Loop to select stripes and save their pixel values
	for (i=0; i<n; i++) {
			      roiManager("select", i);
				  run("Save XY Coordinates...", "save=["+output+filename+"_"+i+".csv]");
	 }
	 roiManager("Deselect");
	 roiManager("Delete"); 
	 } 

// Set input and output directories
input = "C:/path_to_images/";
output = "C:/path_to_output_folder/"; 

// loop to apply everyting as a batch
setBatchMode(true); 
list = getFileList(input);
for (i = 0; i < list.length; i++)
        action(input, output, list[i]);
setBatchMode(false);