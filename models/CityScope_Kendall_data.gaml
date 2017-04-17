/**
* Name: CityScope Kendall
* Author: Arnaud Grignard
* Description: An agent-based model running on the CityScope Kendall
* Scenario 1: Existing amenities
* Scenario 2: Volpe Amenities
* Scenario 3: CityMatrix Amenities
*/

model CityScope_Kendall

global {
	// GIS FILE //	
	file bound_shapefile <- file("../includes/Bounds.shp");
	file buildings_shapefile <- file("../includes/Buildings.shp");
	file roads_shapefile <- file("../includes/Roads.shp");
	file amenities_shapefile <- file("../includes/Amenities.shp");
	file table_bound_shapefile <- file("../includes/table_bounds.shp");
	file imageRaster <- file('../images/gama_black.png') ;
	geometry shape <- envelope(bound_shapefile);
	
	float step <- 10 #sec;
	int nb_people <- 500;
	int current_hour update: (time / #hour) mod 24 ;
	int min_work_start <-4 ;
	int max_work_start <- 10;
	int min_lunch_start <- 11;
	int max_lunch_start <- 13;
	int min_rework_start <- 14;
	int max_rework_start <- 16;
	int min_dinner_start <- 18;
	int max_dinner_start <- 20;
	int min_work_end <- 21; 
	int max_work_end <- 22; 
	float min_speed <- 4 #km / #h;
	float max_speed <- 6 #km / #h; 
	graph the_graph;
	float angle <--9.74;
	
	//////////// GRID //////////////
	map<string, unknown> cityMatrixData;
	list<map<string, int>> cityMatrixCell;
	list<float> density_array;
	map<int,list> citymatrix_map_settings<- [0::["R","L"],1::["R","M"],2::["R","S"],3::["O","L"],4::["O","M"],5::["O","S"]];
		
	//////// AMENITIES  ///////////
	map<string,list> amenities_map_settings<- ["arts_centre"::[rgb(255,255,255),triangle(50)], "bar"::[rgb(255,0,0),square(50)], "cafe"::[rgb(255,125,0),square(50)], "cinema"::[rgb(225,225,225),triangle(50)], 
	"fast_food"::[rgb(255,255,0),square(50)] ,"market_place"::[rgb(0,255,0),square(75)] , "music_club"::[rgb(255,105,180),hexagon(50)], "nightclub"::[rgb(255,182,193),hexagon(50)],
	 "pub"::[rgb(255,99,71),square(50)], "restaurant"::[rgb(255,215,0),square(50)], "theatre"::[rgb(255,255,255),triangle(50)]];
	list amenity_type <-["arts_centre", "bar", "cafe", "cinema","fast_food","market_place","music_club","night_club","pub","restaurant","theatre"];
	//list category_color<- [rgb(14,106,187), rgb(224,185,68), rgb(187,26,14)];
	map<string,rgb> usage_color<- ["R"::#white, "O"::#gray]; 
	map<string,rgb> scale_color<- ["S"::#gamablue, "M"::#gamaorange, "L"::#gamared];
	list scale_string<- ["S", "M", "L"];
	list usage_string<- ["R", "O"]; 
	// MOBILE DATA //
	float lenghtMax <-0;
	file my_csv_file <- csv_file("../includes/mobility/pp.csv",",");
	matrix data <- matrix(my_csv_file);
	 
	//INTERACTION GRAPH 
	graph my_graph;
	int degreeMax <- 1;
	
	//PARAMETERS
	bool moveOnRoadNetworkGlobal <- true parameter: "Move on road network:" category: "Simulation";
	int distance parameter: 'distance ' category: "Visualization" min: 1 <- 100#m;	
	bool drawInteraction <- false parameter: "Draw Interaction:" category: "Visualization";
	bool onlineGrid <-false parameter: "Online Grid:" category: "Environment";
	bool dynamicGrid <-false parameter: "Update Grid:" category: "Environment";
	bool realAmenity <-false parameter: "Real Amenities:" category: "Environment";
	int refresh <- 1000 min: 1 max:1000 parameter: "Refresh rate (cycle):" category: "Environment";
	
	init {
		create building from: buildings_shapefile with: [usage::string(read ("Usage")),scale::string(read ("Scale"))];
		create road from: roads_shapefile ;
		the_graph <- as_edge_graph(road);
		create table from: table_bound_shapefile;
        
        if(realAmenity){
          create amenity from: amenities_shapefile with: [type::string(read ("amenity"))]{
	        color <- rgb(amenities_map_settings[type][0]);
		    shape <- geometry(amenities_map_settings[type][1]) at_location location;
		    usage <- usage_string[rnd(1)];
		    scale <- scale_string[rnd(2)];	
		    fromGrid<-false;
		  }		
        }
	    	
		do initGrid;
		
		create people number: nb_people {
			speed <- min_speed + rnd (max_speed - min_speed) ;
			initialSpeed <-speed;
			time_to_work <- min_work_start + rnd (max_work_start - min_work_start) ;
			time_to_lunch <- min_lunch_start + rnd (max_lunch_start - min_lunch_start) ;
			time_to_rework <- min_rework_start + rnd (max_rework_start - min_rework_start) ;
			time_to_dinner <- min_dinner_start + rnd (max_dinner_start - min_dinner_start) ;
			time_to_sleep <- min_work_end + rnd (max_work_end - min_work_end) ;
			scale<-scale_string[rnd(2)];			
			living_place <- one_of(building where (each.usage="R" and each.scale=scale)) ;
			working_place <- one_of(building  where (each.usage="O" and each.scale=scale)) ;
			eating_place <- one_of(amenity where (each.scale=scale )) ;
			//eating_place <- one_of(amenity where (each.scale=scale and (each.type="fast_food" or each.type="restaurant" or each.type="cafe"))) ;
			//dining_place <- one_of(amenity where ((each.type="arts_centre" or each.type="theatre" or each.type="bar")));
			dining_place <- one_of(amenity where (each.scale=scale )) ;
			objective <- "resting";
			location <- any_location_in (living_place); 
			if (flip(0.1)){
				moveOnRoad <-false;
			}
		}	
	}
	
	
	
  action initGrid{
  		ask amenity where (each.fromGrid=true){
  			do die;
  		}
		if(onlineGrid = true){
		  cityMatrixData <- json_file("https://cityio.media.mit.edu/table/citymatrix_volpe").contents;
	    }
	    else{
	      cityMatrixData <- json_file("../includes/cityIO_Kendall.json").contents;
	    }	
		cityMatrixCell <- cityMatrixData["grid"];
		density_array <- cityMatrixData["objects"]["density"];		
		point center <-{3305,2075};
		loop l over: cityMatrixCell { 
		      create amenity {
		      	  id <-int(l["type"]);
		      	  x<-l["x"];
		      	  y<-l["y"];
				  location <- {	center.x + (13-l["x"])*world.shape.width*0.00942,	center.y+ l["y"]*world.shape.height*0.0113};  
				  location<- {(location.x * cos(angle) + location.y * sin(angle)),-location.x * sin(angle) + location.y * cos(angle)};
				  shape <- square(60) at_location location;	
				  type <- one_of(amenity_type);
				  fromGrid<-true;  
				  if(id!=-1 and id!=-2 and id!=6){
				  	usage <- citymatrix_map_settings[id][0];
				  	scale <- citymatrix_map_settings[id][1];
				  	color<-scale_color[scale];
				    density <-density_array[id];
				  }
				  if(id=-1){
				  	color <-#green;
				  }
				  if(id=6){
				  	color<-#gray;
				  }
				  
				 	
              }				        
        }
	}
	
	reflex updateGrid when: ((cycle mod refresh) = 0) and (dynamicGrid = true){	
		do initGrid;
	}
	
	
    reflex updateDegreeMax when:(drawInteraction=true){
		do degreeMax_computation;
	}

	action degreeMax_computation {
		my_graph <- people as_distance_graph(distance);
		degreeMax <- 1;
		ask people {
			if ((my_graph) degree_of (self) > degreeMax) {
				degreeMax <- (my_graph) degree_of (self);
			}
		}
	}
	
	 action initMobileData{
       loop i from: 1 to: data.rows -1{
	     create mobileData{
	  	   location <- point(to_GAMA_CRS({ float(data[6,i]), float(data[7,i]) }, "EPSG:4326"));
		   lenght<-float(data[4,i]);
		 }	
	   }
	 }
}

species building schedules: []{
	string usage;
	string scale;
	rgb color <- #gray  ;
	float depth;
	
	aspect base {	
     	draw shape color: rgb(50,50,50,125);// depth:depth*shape.area*0.00005;	
	}
	aspect usage{
		  draw shape color: usage_color[usage];
	}
	aspect scale{
		draw shape color: scale_color[scale];
	}
}

species road  schedules: []{
	rgb color <- #red ;
	aspect base {
		draw shape color: rgb(125,125,125,75) ;
	}
}

species people skills:[moving]{
	rgb color <- #yellow ; 
	float initialSpeed;
	building living_place <- nil ;
	building working_place <- nil ;
	amenity eating_place<-nil;
	amenity dining_place<-nil;
	int time_to_work ;
	int time_to_lunch;
	int time_to_rework;
	int time_to_dinner;
	int time_to_sleep;
	string objective ;
	string curMovingMode<-"travelling";	
	string scale;
	string usage; 
	point the_target <- nil ;
	int degree;
	float radius;
	bool moveOnRoad<-true;
	
	reflex time_to_work when: current_hour > time_to_work and current_hour < time_to_lunch  and objective = "resting"{
			objective <- "working" ;
			curMovingMode <- "travelling";
			the_target <- any_location_in (working_place);
			speed <-initialSpeed;	
	}
	
	reflex time_to_go_lunch when: current_hour > time_to_lunch and current_hour < time_to_rework and objective = "working"{
			objective <- "eating" ;
			curMovingMode <- "travelling";
			the_target <- any_location_in (eating_place); 
			speed <-initialSpeed;
	} 
	
	reflex time_to_go_rework when: current_hour > time_to_rework and current_hour < time_to_dinner  and objective = "eating"{
			objective <- "reworking" ;
			curMovingMode <- "travelling";
			the_target <- any_location_in (working_place);
			speed <-initialSpeed;
	} 
	reflex time_to_go_dinner when: current_hour > time_to_dinner and current_hour < time_to_sleep  and objective = "reworking"{
			objective <- "dinning" ;
			curMovingMode <- "travelling";
			the_target <- any_location_in (dining_place);
			speed <-initialSpeed; 
	} 
	
	reflex time_to_go_home when: current_hour > time_to_sleep and (current_hour < 24) and objective = "dinning"{
			objective <- "resting" ;
			curMovingMode <- "travelling";
			the_target <- any_location_in (living_place);
			speed <-initialSpeed; 
	} 
	 
	reflex move {//when: the_target != nil {
	    if(moveOnRoad = true){
	      do goto target: the_target on: the_graph ; 
	    }else{
	      do goto target: the_target ;//on: the_graph ; 
	    }
		
		if (the_target = location) {
			the_target <- nil ;
			//if(objective = "eating" or objective = "dinning"){
				curMovingMode <- "wandering";
			//}
			
		}
		if(curMovingMode = "wandering"){
			do wander speed:0.5 #km / #h;
		}
	}
	
	reflex compute_degree when:(drawInteraction=true){
		degree <- my_graph = nil ? 0 : (my_graph) degree_of (self);
		radius <- ((((degree + 1) ^ 1.4) / (degreeMax))) * 5;
		color <- hsb(0.66,degree / (degreeMax + 1), 0.5);
	}
	
	aspect base {
		draw circle(5#px) color: color;
	}
	
	aspect dynamic {
		draw circle(20) color: scale_color[scale];
	}	
	aspect dynamicTable {
		draw circle(5) color: scale_color[scale];
	}
	aspect scale{
      draw circle(20) color: scale_color[scale];
	}
}

species amenity schedules:[]{
	int id;
	string type;
	string usage;
	string scale;
	bool fromGrid;
	float density <-0.0;
	int pop;
	rgb color;
	int x;
	int y;
	
	reflex countPeople{
		pop <-length(people overlapping self.shape);
	}
	aspect base {
		if(fromGrid){
			draw shape rotated_by -angle color: rgb(color.red, color.green, color.blue,75);//depth:pop*10;	
		}
		else{
		  draw circle(30) empty:true border:#white color: #white;//depth:pop*10;
		  draw circle(30) color: rgb(255,255,255,125);//depth:pop*10;	
		}
			
	}
}

species table{
	aspect base {
		draw shape empty:true border:rgb(125,125,125,75) color: rgb(125,125,125,75) ;
	}	
}

species mobileData schedules:[]{
	rgb color <- #red;
	float lenght;
	aspect base {
		draw circle(5) color:rgb((255 * lenght/50) / 100,(255 * (100 - lenght/50)) / 100 ,0) depth:lenght/100;
	}
}


experiment CityScopeDev type: gui {	
	float minimum_cycle_duration <- 0.02;
	output {
		
		display CityScope  type:opengl background:#black {
			species table aspect:base;
			species building aspect: usage refresh:false position:{0,0,-0.001};
			species building aspect: scale refresh:false position:{0,0,-0.001};
			species road aspect: base refresh:false;
			species amenity aspect: base ;
			species people aspect: scale;
			graphics "text" 
			{
               draw string(current_hour) + "h" color: # white font: font("Helvetica", 25, #italic) at: { 5700, 6200};
               draw imageRaster size:40#px at: { 7000, 6000};
            }
		}
	}
}


experiment CityScopeVolpeDemo type: gui {	
	float minimum_cycle_duration <- 0.02;
	output {
		
		display CityScope  type:opengl background:#black {
			species table aspect:base;
			species building aspect: base refresh:false position:{0,0,-0.001};
			species road aspect: base refresh:false;
			species amenity aspect: base ;
			species people aspect: scale;
			graphics "text" 
			{
               draw string(current_hour) + "h" color: # white font: font("Helvetica", 25, #italic) at: { 5700, 6200};
               draw imageRaster size:40#px at: { 7000, 6000};
            }
		}
			
		display CityScopeTable  type:opengl background:#black
	    fullscreen:2
	    rotate:180
		camera_pos: {4463.617380353552,3032.955173460968,4033.5415243977554} 
		camera_look_pos: {4464.718608885005,3026.0022901525017,0.1794988227075576} 
		camera_up_vector: {0.15643422677690633,0.9876868362601618,0.0017453283655837362}{
			//species building aspect: base refresh:false position:{0,0,-0.001};
			//species road aspect: base refresh:false;
			species amenity aspect: base ;
			species people aspect: scale;
			//species mobileData aspect:base;

           
            graphics "edges" {
		      //Creation of the edges of adjacence
				if (my_graph != nil  and drawInteraction = true ) {
					loop eg over: my_graph.edges {
						geometry edge_geom <- geometry(eg);
						float val <- 255 * edge_geom.perimeter / distance; 
						draw line(edge_geom.points)  color:#white;
					}
				}	
			}	
		}
	}
}