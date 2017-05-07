# edit task using the results from former run
leafDraw()

# typical flight plan
depa09_60_1 <- uavRst::makeFP(projectDir="~/uav/gentree/",
                              locationName = "DEPA01",
                              surveyArea="~/uav/gentree/Traddelkopf/data/Traddelkopf_2017_core2.json",
                              followSurface = TRUE,
                              followSurfaceRes = 1,
                              flightAltitude = 60,
                              overlap = 0.8,
                              demFn = "~/uav/gentree/Traddelkopf/data/DGM1_kellerwald.tif",
                              altFilter = .5,
                              horizonFilter = 25,
                              picRate = 2,
                              maxSpeed = 15,
                              uavType = "solo",
                              windCondition = 2)

  # edit task using the results from former run
leafDraw(overlay = traddelkopf60$demA)
fA <- edit_map(mapview(traddelkopf60$demA)@map)



# track flight 
t3p<-t3p(projectDir ="/home/creu/uav/bayerwald",
         missionName = "filzmoosTree",
         missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
         demFn = "~/uav/grossfilz/grosserfilz.tif",
         windCondition = 2,
         uavType = "djip3",
         followSurfaceRes=5,
         launchPos = c(13.409114897133804,48.92039612988935))

t3<-fp_t3p(projectDir ="/home/creu/uav/gentree/Traddelkopf",
           locationName = "traddeltree_2017_0506",
           missionTrackList="~/uav/gentree/Traddelkopf/data/Traddelkopf_NEU.csv",
           demFn = "~/proj/uav/gentree/Traddelkopf/data/basic/Traddelkopf75_dem.tif",
           windCondition = 2,
           uavType = "solo",
           followSurfaceRes=5,
           launchPos = c(8.979992866516115,51.13024583390035))

t3<-fp_t3p(projectDir ="/home/creu/uav/gentree/Hagenstein",
           locationName = "hagentree_2017_0506",
           missionTrackList="~/uav/gentree/Hagenstein/data/tree_positions_hagenstein_handheld-gps.csv",
           demFn = "~/uav/gentree/Traddelkopf/data/DGM1_kellerwald.tif",
           windCondition = 2,
           uavType = "solo",
           followSurfaceRes=1,
           launchPos = c(8.905967,51.161932))
#8.905491828918459,51.1581923912853
mapview(t3$wp,zcol = "altitude",lwd=1,cex=5)+mapview(t3$lp,color="red",cex=5)
