h1<-fp_t3p(projectDir ="/home/creu/uav/gentree/Hagenstein",
           locationName = "hagentree_2017_0509_c1",
           missionTrackList="~/uav/gentree/Hagenstein/data/Hagenstein_NEU_sorted_North.csv",
           demFn = "~/proj/uav/gentree/Traddelkopf/data/basic/DGM1_kellerwald.tif",
           uavType = "solo",
           followSurfaceRes=1,
           flightAltitude = 60,
           aboveTreeAlt = 45,
           launchPos = c(8.905967,51.161932))

h2<-fp_t3p(projectDir ="/home/creu/uav/gentree/Hagenstein",
           locationName = "hagentree_2017_0509_c2",
           missionTrackList="~/uav/gentree/Hagenstein/data/Hagenstein_NEU_sorted_South.csv",
           demFn = "~/proj/uav/gentree/Traddelkopf/data/basic/DGM1_kellerwald.tif",
           uavType = "solo",
           followSurfaceRes=1,
           flightAltitude = 60,
           aboveTreeAlt = 45,
           launchPos = c(8.905967,51.161932))

t1<-fp_t3p(projectDir ="/home/creu/uav/gentree/Traddelkopf",
           locationName = "traddeltree_2017_0509_c1",
           missionTrackList="~/uav/gentree/Traddelkopf/data/Traddelkopf_NEU_sorted_core_1.csv",
           demFn = "~/proj/uav/gentree/Traddelkopf/data/basic/DGM1_kellerwald.tif",
           uavType = "solo",
           followSurfaceRes=1,
           flightAltitude = 60,
           aboveTreeAlt = 45,
           launchPos = c(8.976860,51.1322252))

t2<-fp_t3p(projectDir ="/home/creu/uav/gentree/Traddelkopf",
           locationName = "traddeltree_2017_0509_c2",
           missionTrackList="~/uav/gentree/Traddelkopf/data/Traddelkopf_NEU_sorted_core_2.csv",
           demFn = "~/proj/uav/gentree/Traddelkopf/data/basic/DGM1_kellerwald.tif",
           uavType = "solo",
           followSurfaceRes=1,
           flightAltitude = 60,
           aboveTreeAlt = 45,
           launchPos = c(8.980250,51.130313))
+-
  soloLog(logDir = "/home/creu/temp3")
