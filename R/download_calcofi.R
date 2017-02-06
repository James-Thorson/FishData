# download calcofi data from erddap

# standardized catch per 10m2
# pull data for the 14 species by Sam McClatchie as being important
# custom speficied spp name, quarter
# pulls information for all years
# pulls information for all stations
# returns a table of tows for all specified species


# needs library(rerddap)

download_calcofi = function(survey="CalCOFI", species=c("Bathylagus pacificus","Diogenichthys laternatus"), years=1951:2014, quarters=1) {
  
  data_list <-list()
  
  # loop over species 
  for (i in 1:length(species)) {
    variables <- c("scientific_name","larvae_10m2","latitude","longitude","station","time")
    
    if (species[i]=="Bathylagus pacificus") {data_host<-"erdCalCOFIlrvcntAStoBA"}
    if (species[i]=="Bathylagoides wesethi") {data_host<-"erdCalCOFIlrvcntAStoBA"}	
    if (species[i]=="Ceratoscopelus townsendi") {data_host<-"erdCalCOFIlrvcntCtoCE"}
    if (species[i]=="Diogenichthys laternatus") {data_host<-"erdCalCOFIlrvcntDHtoEC"}
    if (species[i]=="Leuroglossus stilbius") {data_host<-"erdCalCOFIlrvcntLBtoLI"}
    if (species[i]=="Lipolagus ochotensis") {data_host<-"erdCalCOFIlrvcntLBtoLI"}
    if (species[i]=="Protomyctophum crockeri") {data_host<-"erdCalCOFIlrvcntPPtoPZ"}
    if (species[i]=="Stenobrachius leucopsarus") {data_host<-"erdCalCOFIlrvcntSJtoST"}
    if (species[i]=="Symbolophorus californiensis") {data_host<-"erdCalCOFIlrvcntSUtoTE"}
    if (species[i]=="Tarletonbeania crenularis") {data_host<-"erdCalCOFIlrvcntSUtoTE"}
    if (species[i]=="Triphoturus mexicanus") {data_host<-"erdCalCOFIlrvcntTFtoU"}
    if (species[i]=="Vinciguerria") {data_host<-"erdCalCOFIlrvcntVtoZ"}
    if (species[i]=="Vinciguerria lucetia") {data_host<-"erdCalCOFIlrvcntVtoZ"}
    if (species[i]=="Vinciguerria poweriae") {data_host<-"erdCalCOFIlrvcntVtoZ"}
    
    out = info(as.character(data_host))
    data <- tabledap(out,fields=variables)
    T <- matrix(as.numeric(unlist(strsplit(data[,6],'-'))),nrow(data),byrow=TRUE)
    data_all <- cbind(data[,1:5],year=T[,1],quarter=ceiling(T[,2]/3))
    
    # match specified year and quarter
      data_list[[i]] <- data_all[which(data_all$quarter==quarters & data_all$year >= min(years) & data_all$year <= max(years)),]
   }
  
  out <- dplyr::bind_rows(data_list)
  
  return(out)
}