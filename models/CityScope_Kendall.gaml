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
	file table_bound_shapefile <- file("../includes/table_bounds.shp");
	file bound_shapefile <- file("../includes/Bounds.shp");
	file buildings_shapefile <- file("../includes/Buildings.shp");
	file roads_shapefile <- file("../includes/Roads.shp");
	file amenities_shapefile <- file("../includes/Amenities.shp");
	file amenities_volpe_shapefile <- file("../includes/volpe_amenities.shp");
	file my_csv_file <- csv_file("../includes/mobility/pp.csv",",");
	matrix data <- matrix(my_csv_file);
	geometry shape <- envelope(bound_shapefile);
	file imageRaster <- file('../images/gama_black.png') ;
	float step <- 10 #sec;
	int nb_people <- 1000;
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
	
	//////////// GRID //////////////
	map<string, unknown> matrixData;
    map<int,rgb> buildingColors <-[0::#blue, 1::#blue, 2::"yellow",3::"yellow", 4::"red", 5::"red",6::rgb(40,40,40)];
    list<map<string, int>> legos;
	list<float> density_array;
	
		
	//////// AMENITIES  ///////////
	map<string,list> amenities_map_settings<- ["arts_centre"::[rgb(255,255,255),triangle(50)], "bar"::[rgb(255,0,0),square(50)], "cafe"::[rgb(255,125,0),square(50)], "cinema"::[rgb(225,225,225),triangle(50)], 
	"fast_food"::[rgb(255,255,0),square(50)] ,"market_place"::[rgb(0,255,0),square(75)] , "music_club"::[rgb(255,105,180),hexagon(50)], "nightclub"::[rgb(255,182,193),hexagon(50)],
	 "pub"::[rgb(255,99,71),square(50)], "restaurant"::[rgb(255,215,0),square(50)], "theatre"::[rgb(255,255,255),triangle(50)]];
	 	 
	list category_color<- [rgb(0,0,255), rgb(255,255,0), rgb(255,0,0)];
	list amenity_type <-["arts_centre", "bar", "cafe", "cinema","fast_food","market_place","music_club","night_club","pub","restaurant","theatre"]; 
	 
	//INTERACTION GRAPH 
	graph my_graph;
	int degreeMax <- 1;
	
	//PARAMETERS
	bool moveOnRoadNetworkGlobal <- true parameter: "Move on road network:" category: "Simulation";
	int distance parameter: 'distance ' category: "Visualization" min: 1 <- 100#m;	
	bool drawInteraction <- false parameter: "Draw Interaction:" category: "Visualization";
	int scenario <-3 parameter: "Scenario:" category: "Experiment" min:1 max:3 ;
	bool onlineGrid <-false parameter: "Online Grid:" category: "Environment";
	bool dynamicGrid <-false parameter: "Update Grid:" category: "Environment";
	int refresh <- 100 min: 1 max:1000 parameter: "Refresh rate (cycle):" category: "Environment";
	
	init {
		create building from: buildings_shapefile with: [type::string(read ("TYPE"))]{
			type <- (location.x < world.shape.width*0.4  and location.y > world.shape.height*0.6) ? "Industrial" : "Residential";
		}	
		create road from: roads_shapefile ;
		the_graph <- as_edge_graph(road);
		
		create table from: table_bound_shapefile;
		
		//FROM FILE
		if(scenario = 1){
	      create amenity from: amenities_shapefile with: [type::string(read ("amenity"))]{
			color <- rgb(amenities_map_settings[type][0]);
			shape <- geometry(amenities_map_settings[type][1]) at_location location;
			category<-rnd(2);	
		  }		
		}
		
		if(scenario = 2){
	      create amenity from: amenities_volpe_shapefile with: [type::string(read ("amenity"))]{
			color <- rgb(amenities_map_settings[type][0]);
			shape <- geometry(amenities_map_settings[type][1]) at_location location;
			category<-rnd(2);	
		  }		
		}
		
		if(scenario = 3){
		  do initGrid;
		}
		
		create people number: nb_people {
			speed <- min_speed + rnd (max_speed - min_speed) ;
			initialSpeed <-speed;
			time_to_work <- min_work_start + rnd (max_work_start - min_work_start) ;
			time_to_lunch <- min_lunch_start + rnd (max_lunch_start - min_lunch_start) ;
			time_to_rework <- min_rework_start + rnd (max_rework_start - min_rework_start) ;
			time_to_dinner <- min_dinner_start + rnd (max_dinner_start - min_dinner_start) ;
			time_to_sleep <- min_work_end + rnd (max_work_end - min_work_end) ;
			category<-rnd(2);			
			living_place <- one_of(building where (each.type="Residential")) ;
			working_place <- one_of(building  where (each.type="Industrial")) ;
			eating_place <- one_of(amenity where (each.category=category and (each.type="fast_food" or each.type="restaurant" or each.type="cafe"))) ;
			dining_place <- one_of(amenity where ((each.type="arts_centre" or each.type="theatre" or each.type="bar"))) ;
			objective <- "resting";
			location <- any_location_in (living_place); 
			/*if (flip(0.1)){
				moveOnRoad <-false;
			}*/	
		}	
		
	  	
	  	loop i from: 1 to: data.rows -1{
	  		create mobileData{
	  			location <- point(to_GAMA_CRS({ float(data[6,i]), float(data[7,i]) }, "WGS_1984"));
	  		}	
		}
	  	 	
	}
	
	
	
  action initGrid{
		if(onlineGrid = true){
		  matrixData <- json_file("http://45.55.73.103/table/citymatrix_volpe").contents;
	    }
	    else{
	      matrixData <- json_file("../includes/cityIO_Kendall.json").contents;
	    }	
		legos <- matrixData["grid"];
		density_array <- matrixData["objects"]["density"];
		write density_array;
		loop l over: legos { 
			int id <-int(l["type"]);
			
			if(id != -1 and id != -2)   {
		      create amenity {
				  location <- {	2800 + (13-l["x"])*world.shape.width*0.01,	2800+ l["y"]*world.shape.height*0.01};
				  //color <-buildingColors[id];
				  shape <- square(60) at_location location;
				  type <- one_of(amenity_type);
				 // 
				  //LARGE
				  if(id=0 or id =3){
				  	category <-0;
				  	color<-#red;
				  	density <-density_array[id];
				  }
				  //MEDIUM
				  if(id=1 or id =4){
				  	category <-1;
				  	color<-#yellow;
				  	density <-density_array[id];
				  }
				  
				  if(id=2 or id =5){
				  	category <-2;
				  	color<-#blue;
				  	density <-density_array[id];
				  }
				 	
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
}

species building schedules: []{
	string type;
	rgb color <- #gray  ;
	float depth;
	
	aspect base {	
     	draw shape color: rgb(50,50,50,125);// depth:depth*shape.area*0.00005;	
	}
}

species road  schedules: []{
	rgb color <- #red ;
	aspect base {
		draw shape color: rgb(125,125,125) ;
	}
}

species mobileData {
	rgb color <- #red ;
	aspect base {
		draw shape color: rgb(125,125,125) ;
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
	int category; 
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
			if(objective = "eating" or objective = "dinning"){
				//curMovingMode <- "wandering";
			}
			
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
		draw circle(14) color: category_color[category];
	}
}

species amenity {
	string type;
	int category;
	float density <-0.0;
	int pop;
	rgb color;
	
	reflex countPeople{
		pop <-length(people overlapping self.shape);
	}
	aspect base {
		if(scenario = 3){
			draw shape color: rgb(color.red, color.green, color.blue,75);//depth:pop*10;
			//draw circle(50) empty:true color: rgb(125,125,125) border: rgb(125,125,125);
		    //draw circle(50) color: rgb(75,75,75,125);	
		}else{
		  draw circle(50) empty:true border: #white;
		  draw circle(50) color: rgb(125,125,125,125);	
		}
		
	}
}

species table{
	aspect base {
		draw shape empty:false border:#green color: #green ;
	}	
}


experiment CityScope type: gui {	
	float minimum_cycle_duration <- 0.02;
	output {
		
		display CityScope  type:opengl background:#black keystone:true synchronized:false {
			species table aspect:base;
			species building aspect: base refresh:false position:{0,0,-0.001};
			//species road aspect: base refresh:false;
			species amenity aspect: base ;
			species people aspect: dynamic;
		}
	}
}


experiment CityScopeKeystone type: gui {	
	float minimum_cycle_duration <- 0.02;
	output {	
		display CityScope  type:opengl background:#black keystone:true synchronized:false rotate:180
		camera_pos: {4463.617380353552,3032.955173460968,4033.5415243977554} camera_look_pos: {4464.718608885005,3026.0022901525017,0.1794988227075576} camera_up_vector: {0.15643422677690633,0.9876868362601618,0.0017453283655837362}
		{
			//species table aspect:base;
			species building aspect: base refresh:false position:{0,0,-0.001};
			species road aspect: base refresh:false;
			species amenity aspect: base ;
			species people aspect: dynamic;
			species mobileData aspect:base;
			graphics "text" 
			{
               //draw square(100) color:#blue at: { 5000, 5200};   draw "$" color: # white font: font("Helvetica", 20, #bold) at: { 5075, 5250};
               //draw square(100) color:#yellow at: { 5300, 5200};   draw "$$" color: # white font: font("Helvetica", 20, #bold) at: { 5375, 5250};
               //draw square(100) color:#red at: { 5600, 5200};   draw "$$$" color: # white font: font("Helvetica", 20, #bold) at: { 5675, 5250};
               draw string(current_hour) + "h" color: # white font: font("Helvetica", 25, #italic) at: { 5700, 6200};
              // draw "1000 people "color: # white font: font("Helvetica", 25, #italic) at: { 5000, 5300};
               draw imageRaster size:40#px at: { 7000, 6000};
            }
           
            	graphics "edges" {
		      //Creation of the edges of adjacence
				if (my_graph != nil  and drawInteraction = true ) {
					loop eg over: my_graph.edges {
						geometry edge_geom <- geometry(eg);
						float val <- 255 * edge_geom.perimeter / distance; 
						draw line(edge_geom.points)  color: rgb(75,75,75);
					}
				}	
			}	
		}
	}
}