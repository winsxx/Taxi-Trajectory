// Taxi Trajectory, By: LapanDua

#include <Rcpp.h>
#include <string>
#include <sstream>
#include <vector>
#include <cmath>
using namespace Rcpp;

const float GRID_LAT_MIN = 41.11;
const float GRID_LAT_MAX = 41.26;
const float GRID_LONG_MIN  = -8.72;
const float GRID_LONG_MAX  = -8.54;
const float OUTLIER_LAT_MIN = 40.00;
const float OUTLIER_LAT_MAX = 42.00;
const float OUTLIER_LONG_MIN  = -9.00;
const float OUTLIER_LONG_MAX  = -8.00;
const int N_GRID_ROW = 15;
const int N_GRID_COL = 18;
const float CELL_WIDTH  = (GRID_LONG_MAX-GRID_LONG_MIN)/N_GRID_COL;
const float CELL_HEIGHT = (GRID_LAT_MAX-GRID_LAT_MIN)/N_GRID_ROW;
const int N_EDGES = ((N_GRID_COL+1)*N_GRID_ROW + (N_GRID_ROW+1)*N_GRID_COL);

typedef struct{
  float lat;
  float lon;
} Coordinate;

typedef struct{
  int pos;
  int neg;
} Edge;

// Class Mapping 
std::map<std::string,Coordinate> classMapping;
bool isClassMappingInitialized = false;

// [[Rcpp::export]]
void initializeClassMapping(NumericMatrix lastCoords){
  isClassMappingInitialized = true;
  classMapping.clear();
  
  Coordinate cMatrix[N_GRID_ROW][N_GRID_COL];
  int countMatrix[N_GRID_ROW][N_GRID_COL];
  memset(cMatrix, 0, sizeof(cMatrix));
  memset(countMatrix, 0, sizeof(countMatrix)); 
  
  Coordinate ocMat[3][3];
  int countOcMat[3][3];
  memset(ocMat, 0, sizeof(ocMat));
  memset(countOcMat, 0, sizeof(countOcMat));
  
  for(int i=0; i<lastCoords.nrow(); i++){
     float lat = lastCoords(i,1);
     float lon = lastCoords(i,0);
     int row = std::floor((lat-GRID_LAT_MIN)/CELL_HEIGHT);
     int col = std::floor((lon-GRID_LONG_MIN)/CELL_WIDTH);
     // Ignore coordinates out of bound
     if(row >= N_GRID_ROW || col >= N_GRID_COL || row < 0 || col < 0){
       bool isOutlier = false;
       if(lat<=OUTLIER_LAT_MIN || lat>=OUTLIER_LAT_MAX) isOutlier = true;
       if(lon<=OUTLIER_LONG_MIN || lon>=OUTLIER_LONG_MAX) isOutlier = true;
       if(isOutlier){
         continue;
       } else{
         int latP = 1;
         int lonP = 1;
         if (lat<=GRID_LAT_MIN) latP = 0;
         if (lat>=GRID_LAT_MAX) latP = 2;
         if (lon<=GRID_LONG_MIN) lonP = 0;
         if (lon>=GRID_LONG_MAX) lonP = 2;
         if(latP!=1 || lonP!=1){
           countOcMat[latP][lonP]++;
           ocMat[latP][lonP].lat = ocMat[latP][lonP].lat + (lat - ocMat[latP][lonP].lat)/countOcMat[latP][lonP];
           ocMat[latP][lonP].lon = ocMat[latP][lonP].lon + (lon - ocMat[latP][lonP].lon)/countOcMat[latP][lonP];
         }
       }
     } else{
       countMatrix[row][col] ++;
       cMatrix[row][col].lat = cMatrix[row][col].lat + (lat- cMatrix[row][col].lat)/countMatrix[row][col];
       cMatrix[row][col].lon = cMatrix[row][col].lon + (lon- cMatrix[row][col].lon)/countMatrix[row][col];
     }
  }
 
  for(int i=0; i<N_GRID_ROW; i++){
    for(int j=0; j<N_GRID_COL; j++){
      std::ostringstream name;
      name << "R"<< (i+1) << "C" << (j+1);

      Coordinate coord = cMatrix[i][j];
      classMapping[name.str()] = coord;
    }  
  }
  for(int i=0; i<3; i++){
    for(int j=0; j<3; j++){
      if(i!=1 || j!=1){
        std::ostringstream name;
        name << "O" << i << j;
        
        Coordinate coord = ocMat[i][j];
        classMapping[name.str()] = coord;   
      }
    }
  }
}

// [[Rcpp::export]]
NumericVector classToCoordinate(String c){
  if (!isClassMappingInitialized) {
    stop("Initialize Class Mapping to Use");
  }
  if(classMapping.find(c) == classMapping.end()){
    // Class name not found
    float lat = (GRID_LAT_MIN+GRID_LAT_MIN)/2;
    float lon = (GRID_LONG_MIN+GRID_LONG_MIN)/2;
    return NumericVector::create(lat,lon);
  } else {
    // Class name found
    float lat = classMapping[c].lat;
    float lon = classMapping[c].lon;
    return NumericVector::create(lon,lat);
  }
}

// [[Rcpp::export]]
String CoordinateToClass(NumericVector coor){
   bool isOuter = false;
   float lat = coor[1];
   float lon = coor[0];
   if(lat<=GRID_LAT_MIN || lat>=GRID_LAT_MAX) isOuter = true;
   if(lon<=GRID_LONG_MIN || lon>=GRID_LONG_MAX) isOuter = true;
   if(isOuter){
     int latP = 1;
     int lonP = 1;
     if (lat<=GRID_LAT_MIN) latP = 0;
     if (lat>=GRID_LAT_MAX) latP = 2;
     if (lon<=GRID_LONG_MIN) lonP = 0;
     if (lon>=GRID_LONG_MAX) lonP = 2;

     std::ostringstream name;
     name << "O"<< latP << lonP;
     return name.str();

   } else {
     int row = std::floor((lat-GRID_LAT_MIN)/CELL_HEIGHT);
     int col = std::floor((lon-GRID_LONG_MIN)/CELL_WIDTH);
     std::ostringstream name;
     name << "R"<< (row+1) << "C" << (col+1);
     return name.str();
   }
}

// [[Rcpp::export]]
NumericVector coordsToFeature(NumericMatrix coords) {
   Edge verticalEdges[N_GRID_COL+1][N_GRID_ROW];
   Edge horizontalEdges[N_GRID_ROW+1][N_GRID_COL];
   
   memset(verticalEdges, 0, sizeof(verticalEdges));
   memset(horizontalEdges, 0, sizeof(horizontalEdges));
   
   NumericVector feature(N_EDGES);

   for(int r=1; r<coords.nrow(); r++){
     float SLat = coords(r-1,1);
     float SLon = coords(r-1,0);
     float FLat = coords(r,1);
     float FLon = coords(r,0);
     
     int row = std::floor((SLat-GRID_LAT_MIN)/CELL_HEIGHT);
     int col = std::floor((SLon-GRID_LONG_MIN)/CELL_WIDTH);
     
     // Ignore coordinates out of bound
     if(row >= N_GRID_ROW || col >= N_GRID_COL || row < 0 || col <0) continue;
     
     float delta_lat = FLat - SLat;
     float delta_lon = FLon - SLon;
     
     if(delta_lat >= 0.0){
       verticalEdges[col][row].pos++;
       verticalEdges[col+1][row].pos++;
     } else {
       verticalEdges[col][row].neg++;
       verticalEdges[col+1][row].neg++;
     }
     
     if(delta_lon >= 0.0){
       horizontalEdges[row][col].pos++;
       horizontalEdges[row+1][col].pos++;
     } else {
       horizontalEdges[row][col].neg++;
       horizontalEdges[row+1][col].neg++;
     }       
   }

   int iterator = 0;
   for(int i=0; i<N_GRID_COL+1; i++){
     for(int j=0; j<N_GRID_ROW; j++){
       if(verticalEdges[i][j].pos == 0 && verticalEdges[i][j].neg==0){
         feature[iterator] = 0;
       } else {
         if(verticalEdges[i][j].pos > verticalEdges[i][j].neg){
           feature[iterator] = 1;
         } else {
           feature[iterator] = 2;
         }
       }
       iterator++;
     }
   }
   for(int i=0; i<N_GRID_ROW+1; i++){
     for(int j=0; j<N_GRID_COL; j++){
       if(horizontalEdges[i][j].pos == 0 && horizontalEdges[i][j].neg == 0){
         feature[iterator] = 0;
       } else {
         if(horizontalEdges[i][j].pos > horizontalEdges[i][j].neg){
           feature[iterator] = 1;
         } else {
           feature[iterator] = 2;
         }
       }
       iterator++;
     }
   }
   return feature;
}

// [[Rcpp::export]]
CharacterVector getEdgesFeatureLabel(){
  CharacterVector labels(N_EDGES);
   int iterator = 0;
   std::ostringstream stream;
   for(int i=0; i<N_GRID_COL+1; i++){
     for(int j=0; j<N_GRID_ROW; j++){
       stream.str(std::string());
       stream << "V" << i+1 << "N" << j+1;
       labels[iterator] = stream.str();
       iterator++;
     }
   }
   for(int i=0; i<N_GRID_ROW+1; i++){
     for(int j=0; j<N_GRID_COL; j++){
       stream.str(std::string());
       stream << "H" << i+1 << "N" << j+1;
       labels[iterator] = stream.str();
       iterator++;
     }
   }
   return labels;
}


// [[Rcpp::export]]
NumericMatrix outlierRemovedCoords(NumericMatrix coords){
  std::vector<Coordinate> notOutlier;
  for(int i=0; i<coords.nrow(); i++){
    bool isOutlier = false;
    if(coords(i,1)<=OUTLIER_LAT_MIN || coords(i,1)>=OUTLIER_LAT_MAX) isOutlier = true;
    if(coords(i,0)<=OUTLIER_LONG_MIN || coords(i,0)>=OUTLIER_LONG_MAX) isOutlier = true;
    if(!isOutlier){
      Coordinate temp;
      temp.lat = coords(i,1);
      temp.lon = coords(i,0);
      notOutlier.push_back(temp);
    }
  }
  NumericMatrix newCoords(notOutlier.size(), 2);
  for(unsigned i=0; i<notOutlier.size(); i++){
    newCoords(i,1) = notOutlier[i].lat;
    newCoords(i,0) = notOutlier[i].lon;
  }
  return newCoords;
}

// [[Rcpp::export]]
CharacterVector getClassLevel(){
  CharacterVector level(N_GRID_ROW*N_GRID_COL+8);
  int iterator = 0;
  for(int i=0; i<N_GRID_ROW; i++){
    for(int j=0; j<N_GRID_COL; j++){
      std::ostringstream name;
      name << "R"<< (i+1) << "C" << (j+1);
      level[iterator] = name.str();
      iterator++;
    }
  }
  for(int i=0; i<3; i++){
    for(int j=0; j<3; j++){
      if(i!=1 || j!=1){
        std::ostringstream name;
        name << "O" << i << j;
        level[iterator] = name.str();
        iterator++;
      } 
    }
  }
  return level;
}