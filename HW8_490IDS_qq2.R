# HW 8 - Due Tuesday Nov 8, 2016 in moodle and hardcopy in class. 
# Upload R file to Moodle with filename: HW8_490IDS_YOURID.R
# Do not remove any of the comments. These are marked by #

### This assignment will use Google Earth for data display. 
### The .rda file is uploaded to Moodle.

### Load HW8.rda and attach the XML library
install.packages('XML')
library('XML')

### Part 1.  Create the data frame
### Look at the instructions in HW8.pdf.
### Functions you'll want to use: xmlParse(), xmlRoot(), xpathSApply(), xmlGetAttr().
### It also might make it easier to use: xmlToList(), merge().

### Load the data frame called LatLon from HW8.rda.  
load('hw8.rda')

### Download the gzipped XML factbook document from
### http://jmatchparser.sourceforge.net/factbook/
### and create an XML "tree" in R 
factbook=xmlParse("factbook.xml")
factbook_list=xmlToList(factbook)

### Use XPath to extract the infant mortality and the CIA country codes from the XML tree
###   
### Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.
factbook_root=xmlRoot(factbook)
getNodeSet(factbook_root, '//field[@name="Infant mortality rate"]')
getNodeSet(factbook_root, '//field[@name="Population"]')
infant=xpathSApply(factbook, "//field[@name='Infant mortality rate']/rank", xmlGetAttr, "number")
country=xpathSApply(factbook, "//field[@name='Infant mortality rate']/rank", xmlGetAttr, "country")
IM=data.frame("Infant Mortality"=infant, "CIA.Codes"=country)
head(IM)

### Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.
population=xpathSApply(factbook, "//field[@name='Population']/rank", xmlGetAttr, "number")
country=xpathSApply(factbook, "//field[@name='Population']/rank", xmlGetAttr, "country")
Pop=data.frame("Population" = population, "CIA.Codes" = country)
head(Pop)


### Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes
IMPop=merge(IM,Pop,by='CIA.Codes')
head(IMPop)

### Now merge IMPop with LatLon (from newLatLon.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality
### (please check lat,long are not reversed in the file)
names(LatLon)
IMPop$CIA.Codes=toupper(IMPop$CIA.Codes)
AllData=merge(IMPop,LatLon,by='CIA.Codes')
head(AllData)


### Part 2.  Create a KML document
### Make the KML document described in HW8.pdf.  It should have the basic
### structure shown in that document.  You can use the addPlacemark function below to make
### the Placemark nodes, you just need to complete the line for the Point node and
### figure out how to use the function.

makeBaseDocument = function(){
### This code creates the template KML document 
XML_doc=newXMLDoc()
XML_root=newXMLNode('kml',namespaceDefinitions = c(xmlns = "http://www.opengis.net/kml2.2"),doc=XML_doc)
Document_Node=newXMLNode('Document',parent = XML_root,doc=XML_doc)
LookAt_Node=newXMLNode('LookAt',parent = XML_root,doc=XML_doc)
newXMLNode('Name','Country Facts',parent = Document_Node,doc=XML_doc)
newXMLNode('Description','Infant Mortality',parent = Document_Node,doc=XML_doc)
return(XML_doc)

}

KML_Doc=makeBaseDocument()
Children=xmlChildren(KML_Doc)
Document_Node=Children[[1]][[1]]

addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, 
                        inf1, pop1, style = FALSE)
{
  pm = newXMLNode("Placemark", 
                  newXMLNode("name", ctryName), attrs = c(id = ctryCode), 
                  parent = parent)
  newXMLNode("description", paste(ctryName, "\n Population: ", pop, 
                                  "\n Infant Mortality: ", infM, sep =""),
             parent = pm)

  newXMLNode("Point",newXMLNode('coordinates',paste(lat,',',lon,',',0,sep='')),parent = pm)
             
### You need to fill in the code for making the Point node above, including coordinates.
### The line below won't work until you've run the code for the next section to set up
### the styles.

  if(style) newXMLNode("styleUrl", paste("#YOR", inf1, "-", pop1, sep = ''), parent = pm)
}
for(i in 1:(dim(AllData)[1])){
  addPlacemark(lat = AllData$Latitude[[i]],lon = AllData$Longitude[[i]],
               ctryCode = AllData$CIA.Codes[[i]],ctryName = AllData$Country.Name[[i]],
               pop = AllData$Population[[i]],infM = AllData$Infant.Mortality[[i]],
               parent = Document_Node)
}
saveXML(doc=KML_Doc,file='Part2.kml')
### Save your KML document here, call it Part2.kml, and open it in Google Earth.
### (You will need to install Google Earth.)  
### It should have pushpins for all the countries.

### Part 3.  Add Style to your KML
### Now you are going to make the visualizatiion a bit fancier.  Pretty much all the code is given to you
### below to create style elements that are to be placed near the top of the document.
### These , you just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()

doc2 = makeBaseDocument()
children2=xmlChildren(doc2)
Document_Node2=children2[[1]][[1]]

### The following code is an example of how to create cut points for 
### different categories of infant mortality and population size.
### Figure out what cut points you want to use and modify the code to create these 
### categories.
infCut = cut(as.numeric(AllData[,2]), breaks = quantile(as.numeric(AllData[,2]), seq(0,1,length=6)), include.lowest = TRUE)
infCut = as.numeric(infCut)
popCut = cut(log(as.numeric(AllData[,3])), breaks=quantile(log(as.numeric(AllData[,3])), seq(0,1,length=6)), include.lowest = TRUE)
popCut = as.numeric(popCut)

### Now figure out how to add styles and placemarks to doc2
### You'll want to use the addPlacemark function with style = TRUE

### Below is code to make style nodes. 
### You should not need to do much to it.

### You do want to figure out what scales to you for the sizes of your circles
scales = c(0.5, 1, 3, 5, 10)

addStyle = function(col1, pop1, parent, urlBase, scales = scales)
{
  st = newXMLNode("Style", attrs = c("id" = paste("YOR", col1, "-", pop1, sep="")), parent = parent)
  newXMLNode("IconStyle", 
			 newXMLNode("scale", scales[pop1]), 
		     newXMLNode("Icon", paste(urlBase, "yor", col1, "ball.png", sep ="")), parent = st)
}

for (k in 1:5)
{
  for (j in 1:5)
  {
    addStyle(j, k, parent=Document_Node2,  'http://web.stanford.edu/~vcs/StatData/circles/color_label_circle_blue.png',scales)
  }
}

### You will need to figure out what order to call addStyle() and addPlacemark()
### so that the tree is built properly. You may need to adjust the code to call the png files
for(i in 1:(dim(AllData)[1])) {
  addPlacemark(lat=AllData$Latitude[[i]], lon=AllData$Longitude[[i]], 
               ctryCode=AllData$CIA.Codes[[i]], ctryName=AllData$Country.Name[[i]], 
               pop=AllData$Population[[i]], infM=AllData$Infant.Mortality[[i]], 
               parent=Document_Node2, inf1=infCut[[i]], pop1=popCut[[i]], 
               style=TRUE)
}



### Finally, save your KML document, call it Part3.kml and open it in Google Earth to 
### verify that it works.  For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded HW8.rda.
saveXML(doc2,file='Part3.kml')
