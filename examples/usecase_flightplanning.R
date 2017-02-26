# edit task using the results from former run
leafDraw()

# typical flight plan
traddelkopf60 <- uavRst::makeFP(projectDir="~/proj/uav/gentree/Traddelkopf/",
                                missionName = "65msolo",
                                surveyArea="~/proj/uav/gentree/Traddelkopf/data/task_areas/Traddelkopf_2017_02_paralell.json",
                                followSurface = TRUE,
                                followSurfaceRes = 1,
                                flightAltitude = 65,
                                overlap = 0.75,
                                demFn = "~/proj/uav/gentree/Traddelkopf/data/DGM1_kellerwald.tif",rcRange = TRUE,
                                  
                                altFilter = 1.,
                                horizonFilter = 25,
                                maxSpeed = 10,
                                uavType = "solo",
                                cameraType = "MAPIR2",
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
         uavType = "solo",
         followSurfaceRes=5,
         launchPos = c(13.409114897133804,48.92039612988935))

