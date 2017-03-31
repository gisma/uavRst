# edit task using the results from former run
leafDraw()

# typical flight plan
traddelkopf60 <- uavRst::makeFP(projectDir="~/uav/gentree/Traddelkopf/",
                                missionName = "50m_c2q",
                                surveyArea="~/uav/gentree/Traddelkopf/data/Traddelkopf_2017_core2q.json",
                                followSurface = TRUE,
                                followSurfaceRes = 1,
                                flightAltitude = 50,
                                overlap = 0.8,
                                demFn = "~/proj/uav/gentree/Traddelkopf/data/DGM1_kellerwald.tif",
                                altFilter = 1.,
                                horizonFilter = 25,
                                maxSpeed = 14,
                                uavType = "djip3",
                                windCondition = 1) 


# edit task using the results from former run
leafDraw(overlay = traddelkopf60$demA)
fA <- edit_map(mapview(traddelkopf60$demA)@map)



# track flight 
t3p<-t3p(projectDir ="/home/creu/uav/bayerwald",
         missionName = "filzmoosTree",
         missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
         demFn = "~/uav/grossfilz/grosserfilz.tif",
         windCondition = 2,
         uavType = "solo",
         followSurfaceRes=5,
         launchPos = c(13.409114897133804,48.92039612988935))

