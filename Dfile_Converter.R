
#install tools package if you dont have it 

library(tools)

#when opening R set the working directory to the parent directory 
##where all .D files are stored
##Make sure there are no other files except for the .D files 
#create empty data frames to store stuff
Fulldirectoryofsamples<-data.frame()
Fulldirectoryofsamples_totals<-data.frame()
FulldirectoryofsamplesM<-data.frame()
Fulldirectoryofsamples_totalsM<-data.frame()
# Get the current working directory
dir_path <- getwd()

# List all directories with .D in the current directory
directories <- dir(dir_path, pattern = ".D", full.names = TRUE, recursive = FALSE)
#check if there is a mass normalization file in working directory (Needs to be .csv)
#example if file 002-P1-A1-1.D
##units are arbitrary but just so you make sure everything is measured the same 
#SAMPLENAME,MASS,Units
#P1-A1-1,1.25,mg

if(!!length(list.files(pattern = "MASSNORMALIZE.csv"))){
  MASSNORMALIZE<-read.csv("MASSNORMALIZE.csv",colClasses = c("character","numeric","character"))
}else{MASSNORMALIZE<-NULL}


i<-1
# Loop through all directories
for (i in 1:length(directories)) {
  
  #go to .D file [i]
  print(paste0("Starting Directory: ", directories[i], "\n"))
  # Find REPORT.TXT in the current directory 
  files <- dir(directories[i],pattern = "Report.TXT", full.names = TRUE, recursive = F)
  j<-1
  # Loop through all files #there really should only be one but just incase weird 
  for (j in 1:length(files)) {

    # Read the original data it will make each line a character string 
    data <- readLines(files[j],skipNul = T, encoding = "UTF-16LE")
    
    #Make a sample name subset everything before last slash
    SampleName<-gsub("(.*/\\s*(.*$))", "\\2", directories[i])
    #Remove .D
    SampleName<-gsub('.{2}$', '', SampleName)
    #we need to find the line that the Peak information start and create a useful data frame
    #assuming that all tables are build the same & actual data starts 6 rows down
    selection <- which(grepl("Peak RetTime Sig Type    Area      Height     Area  ",data))
    #also find the last line where totals are at 
    Totals_string<-which(grepl("Totals :",data))
    
    #subset data frame between selection and total pulling just peak information  
    #remember assuming that all reports are structured the same 
    RAW_report<- data[(selection+6):(Totals_string-2)]
    #Remove all blank lines()
    RAW_report<-RAW_report[RAW_report != ""]
    #find first pea
   #create a data frame 
   #collapse multi spaces 
    string <- trimws(gsub("\\s+", " ", RAW_report))
    #smush letters i dont thing they are useful
    string <-  gsub("([a-zA-Z]) ([a-zA-Z])", '\\1\\2', string)
   out <- strsplit(as.character(string),' ') 
    out2<-as.data.frame(do.call(rbind, out))
    #make numeric columnts
    out2[,c(5:7)] <- sapply(out2[,c(5:7)], as.numeric)
    #add proper columnames
    colnames(out2)<-c("Peak","RetTime","Sig","Type","Area","Height","AreaPerc")
    #add samplename to file just incase 
    out2$Samplename<-SampleName
    #export peak report 
    #
    Fulldirectoryofsamples  <-rbind(Fulldirectoryofsamples,out2)
    write.csv( file=paste0(gsub('.{4}$', '', files[j]),"_",SampleName,"_Peak.csv"),out2,row.names = F)
  #do mass normalization if need
    if(!is.null(MASSNORMALIZE)){
      NormalizationFactor<-max(MASSNORMALIZE$MASS)/MASSNORMALIZE$MASS[i]
      #Multiply each column by NormalizationFactor
      #make numeric columnts 
      out2[,c(5:7)]<-out2[,c(5:7)] * NormalizationFactor
    write.csv(file=paste0(gsub('.{4}$', '', files[j]),"_",SampleName,"_Peak_MassNormalize.csv"),out2,row.names = F) 
    }else{}
    
    
  #make larger dataframe to add 
  
    FulldirectoryofsamplesM  <-rbind(FulldirectoryofsamplesM,out2)
    
    
    ##################################wrangle totals similarly #######################################################
  
    
    #also find the last line where totals are at 
    Totals_string<-which(grepl("Totals :",data))
    
    #subset data frame between selection and total pulling just peak information  
    #remember assuming that all reports are structured the same 
    RAW_report<- data[Totals_string]
    #Remove all blank lines()
    RAW_report<-RAW_report[RAW_report != ""]
    #find first pea
    #create a data frame 
    #collapse multi spaces 
    string <- trimws(gsub("\\s+", " ", RAW_report))
    out <- strsplit(as.character(string),' ') 
    out2<-as.data.frame(do.call(rbind, out))
    #remove1st two columns 
    out2<-out2[,-c(1:2)]
    #make numeric columnts
    out2[,c(1:2)] <- sapply(out2[,c(1:2)], as.numeric)
    #add proper columnames
    colnames(out2)<-c("Area","Height")
    out2$Samplename<-SampleName
    
    
    Fulldirectoryofsamples_totals  <-rbind(Fulldirectoryofsamples_totals,out2)
    #export peak report 
    write.csv( file=paste0(gsub('.{4}$', '', files[j]),"_",SampleName,"_Total_Peak.csv"),out2,row.names = F)
    #do mass normalization if need
    if(!is.null(MASSNORMALIZE)){
      NormalizationFactor<-max(MASSNORMALIZE$MASS)/MASSNORMALIZE$MASS[i]
      #Multiply each column by NormalizationFactor
      #make numeric columnts 
      out2[,c(1:2)]<-out2[,c(1:2)] * NormalizationFactor
      write.csv(file=paste0(gsub('.{4}$', '', files[j]),"_",SampleName,"_Total_Peak_MassNormalize.csv"),out2,row.names = F)
      }else{}
    
    Fulldirectoryofsamples_totalsM  <-rbind(Fulldirectoryofsamples_totalsM,out2)
    
    
  } # end for files
  
} # end for directories
#Note that the R script uses readLines and iconv functions to read and convert the text files, respectively. It also uses file function to open and write the converted data to new files. Additionally, the R script skips non-directories and non-Report.TXT files using file.info and grepl functions.

write.csv(file=paste0(dir_path,"_Total_Peak_MassNormalize.csv"),Fulldirectoryofsamples_totalsM,row.names = F)

write.csv( file=paste0(dir_path,"_Peak_MassNormalize.csv.csv"),FulldirectoryofsamplesM,row.names = F)

write.csv(file=paste0(dir_path,"_Total.csv"),Fulldirectoryofsamples_totals,row.names = F)

write.csv( file=paste0(dir_path,"_Peak.csv"),Fulldirectoryofsamples,row.names = F)
