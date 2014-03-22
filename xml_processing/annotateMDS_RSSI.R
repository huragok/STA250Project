library(XML)
library(SVGAnnotation)
setwd("../data")

# Annotate testVis.svg
testVis_root = xmlRoot(xmlTreeParse("testVis.svg", useInternalNodes=TRUE))
circlesVis = xmlChildren(testVis_root[[3]])
sapply(seq(along = circlesVis), 
       function(i) { 
        addAttributes(circlesVis[[i]], 
                      onmouseover = 
                        "moverVis(this)", onmouseout="moutVis(this)") 
       }) 

saveXML(testVis_root, file = "testVisAnnotated.svg")

# Annotate testMDS.svg
testMDS_root = xmlRoot(xmlTreeParse("testMDS.svg", useInternalNodes=TRUE))
circlesMDS = xmlChildren(testMDS_root[[2]])
sapply(seq(along = circlesMDS), 
       function(i) { 
        addAttributes(circlesMDS[[i]], 
                      onmouseover = 
                        "moverMDS(this)", onmouseout="moutMDS(this)") 
       }) 

saveXML(testMDS_root, file = "testMDSAnnotated.svg")
