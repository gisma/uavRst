fp<-makeFP(projectDir="~/uav/hasenkopf",
           missionName = "hase50",
           surveyArea = "hase50.json", 
           followSurface = TRUE, 
           flightAltitude = 50,
           overlap = 0.6,
           demFn = "mrbiko.tif",
           altFilter = 5,
           maxSpeed = 30,
           windCondition = 2)

fp<-makeFP(projectDir="~/uav/npbw",
           missionName = "npbw100m_nord",
           surveyArea="bw100_nord.json", 
           followSurface = TRUE, 
           flightAltitude = 100,
           overlap = 0.6,
           altFilter = 5,
           maxSpeed = 40,
           windCondition = 3)

fp<-makeFP(projectDir ="/home/creu/uav/",
           missionName = "grossfilz100m_nord",
           surveyArea="grossfilz100_nord.json", 
           followSurface = TRUE, 
           flightAltitude = 100,
           demFn = "grosser-filz_latlon.tif",
           overlap = 0.6,
           altFilter = 5,
           maxSpeed = 40,
           windCondition = 3)

fpdata<-makeFP(projectDir ="/home/creu/uav/",
               missionName = "grossfilz100m_sued",
               surveyArea="/home/creu/uav/grossfilz/grossfilz100m_sued3.json", 
               followSurface = TRUE, 
               flightAltitude = 100,
               demFn = "/home/creu/uav/grossfilz/grosserfilz.tif",
               overlap = 0.6,
               altFilter = 5,
               maxSpeed = 40,
               windCondition = 3)          



fp<-makeFP(projectDir ="/home/creu/uav/kellerwald",
           missionName = "hagenstein100",
           surveyArea="~/uav/hagenstein.json",
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "~/uav/Hagenstein_dsm.tif",
           windCondition = 2,
           uavType = "djip3"
)

fp<-makeFP(projectDir ="/home/creu/uav/kellerwald",
           missionName = "hagenstein100",
           surveyArea="~/uav/hagenstein.json",
           followSurface = TRUE,
           flightAltitude = 100,
           windCondition = 2,
           uavType = "djip3"
)

fp<-t3p(projectDir ="/home/creu/uav/bayerwald",
        missionName = "filzmoosTree",
        missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
        demFn = "~/uav/grossfilz/grosserfilz_latlon.tif",
        windCondition = 2,
        uavType = "solo",
        followSurfaceRes=5)

fp<-makeFP(projectDir ="/home/creu/uav/kellerwald",
           missionName = "hagenstein100",
           surveyArea="~/uav/hagenstein.json",
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "~/uav/uniwald/data/mrbiko.tif",
           windCondition = 2,
           followSurfaceRes=10,
           altFilter = 3
)

fp<-makeFP(projectDir ="~/uav/cookbook",
           missionName = "firstSurvey",
           surveyArea="~/proj/drone/uniwald/myFirstSurvey.json",
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "~/uav/uniwald/data/mrbiko.tif",
           windCondition = 2,
           followSurfaceRes=10,
           altFilter = 3
)

data(mrbiko)
writeRaster(mrbiko,"~/dem.tif")
fp<-makeFP(projectDir ="~/uav/cookbook",
           missionName = "firstSurvey",
           surveyArea="~/proj/drone/uniwald/myFirstSurvey.json",
           flightAltitude = 100,
           demFn ="~/dem.tif")


t3p<-t3p(projectDir ="/home/creu/uav/bayerwald",
         missionName = "filzmoosTree",
         missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
         demFn = "~/uav/grossfilz/grosserfilz.tif",
         windCondition = 2,
         uavType = "solo",
         followSurfaceRes=5,
         launchPos = c(13.409114897133804,48.92039612988935))

fp<-makeFP(projectDir ="/home/creu/uav/kellerwald",
           missionName = "traddelkopf30",
           surveyArea="~/uav/traddelkopf/Traddelkopf2016_09_19_30.json",
           followSurface = TRUE,
           flightAltitude = 30,
           demFn = "~/uav/kellerwald/data/Traddelkopf75_dem.tif",
           windCondition = 3,
           uavType = "djip3",
           followSurfaceRes = 5)



fp<-makeFP(projectDir ="/home/creu/uav/uniwald",
           missionName = "uniwald_q1_100",
           surveyArea="~/uav/uniwald/uniwald100_q1.kml",
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "~/uav/uniwald/data/mrbiko.tif",
           windCondition = 3,
           followSurfaceRes = 20,
           uavType = "solo"
           
)















gui(makeFP,
    argSlider = list(windCondition = c(1, 3,1), followSurfaceRes = c(1, 20, 1),
                     flightAltitude = c(5, 100,5)),
    surveyArea = guiGetSafe("PERSONAL_dataset"),
    exec = NULL,
    cancelButton = TRUE,
    title = "TEST",
    callback = guiExec)

res<-guiFilename( sframe=tktoplevel(), text="Filename ...", default="foo.txt", title="e",
                  filter="{{All files} {.*}}")