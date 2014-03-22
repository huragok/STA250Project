library(XML)
library(SVGAnnotation)
setwd("../data")

svg_list = xmlToList(xmlTreeParse("testVis.svg", useInternalNodes=TRUE))

num_circle = length(svg_list[[3]]) - 1


# Function to evaluate the Bhattacharyya distance between the two nodes (assuming independent Gaussian distribution on each direction for each BS)
distRSSI <- function(i, j, svg_list) { 
	circle_i = svg_list[[3]][[i]]
	circle_j = svg_list[[3]][[j]]
	
	dist = 0
	if (!(i == j)) {
		for (ind_direction in 1 : 4) {
			for (ind_BS in 1 : 5) {
				mean_i = as.numeric(circle_i[[ind_direction]][[ind_BS]]$mean)
				mean_j = as.numeric(circle_j[[ind_direction]][[ind_BS]]$mean)
				std_i = max(c(as.numeric(circle_i[[ind_direction]][[ind_BS]]$sd), 0.1))
				std_j = max(c(as.numeric(circle_j[[ind_direction]][[ind_BS]]$sd), 0.1))
				dist = dist + log(((std_i ^ 2) / (std_j ^ 2) + (std_j ^ 2) / (std_i ^ 2) + 2) / 4) / 4 + ((mean_i - mean_j) ^ 2 / (std_i ^ 2 + std_j ^ 2)) / 4
			}
		}
	}
	return(dist)
}

dist = matrix(unlist(lapply(1 : num_circle, function(j) {sapply(1 : num_circle, distRSSI, j = j, svg_list = svg_list)})), nrow = num_circle, ncol = num_circle)
mds = cmdscale(dist, k = 2)

# Now again lets manually plot a SVG which I am pretty good at!
svg_root = newXMLNode("svg") # Create the svg root node
addAttributes(svg_root, "xmlns"="http://www.w3.org/2000/svg", "xmlns:xlink"="http://www.w3.org/1999/xlink", "width"="620pt", "height"="485pt", "viewBox"="0 0 620 485", "version"="1.1")

group_box = newXMLNode("g", parent = svg_root)
addAttributes(group_MDS, "id"="layer_box")
box = newXMLNode("rect", parent = group_box)
addAttributes(box, "width"="619", "height"="484", "style"="stroke-width:1;stroke:rgb(0,0,0);fill-opacity:0.0")

group_MDS = newXMLNode("g", parent = svg_root)
addAttributes(group_MDS, "id"="layer_MDS")

createMDS <- function(index_MDS, mds, group_MDS) {
	circle_MDS = newXMLNode("circle", parent = group_MDS) # Plot a solid svg circle
	addAttributes(circle_MDS, "id" = paste("MDS", index_MDS, sep = "", collapse = NULL), "cx" = 0.71 * (mds[index_MDS, 1] + 410), "cy" = 0.71 * (170 - mds[index_MDS, 2]), "r"="3", "stroke"="black", "stroke-width"="1", "fill"="black")

}
lapply(1 : num_circle, createMDS, mds = mds, group_MDS = group_MDS)




saveXML(svg_root, file = "testMDS.svg")
