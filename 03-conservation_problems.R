library(tidyverse)
library(sf)
library(rlist)
library(prioritizr)



# Scenario 1: Assuming equal costs ----------------------------------------

###################
#Planning units

pu <- st_read("data/shp/pu_lockedin.shp", crs=4326) %>% 
        as.data.frame() %>% 
        mutate(cost=0
               ) %>% 
        st_as_sf(.)





# Conservation features

(features <- list.files(path = "data/features/adjusted/", 
                       pattern='_adj.tif', 
                       all.files=TRUE, 
                       full.names=TRUE)
)



# Select features of interest
features <- stack(features[c(1:3, 6, 7, 16:17, 20,  24:27)])

crs(features) <- "+proj=longlat +datum=WGS84 +no_defs" 

###################

# Select features to remove from all priority solutions (locked out)

deep <- raster("data/features/adjusted/concesiones_aguas_profundas_adj.tif")
current <- raster("data/features/adjusted/concesiones_vigentes_adj.tif")
lic <- raster("data/features/adjusted/licencia_extraccion_adj.tif")
shared <- raster("data/features/adjusted/produccion_compartida_adj.tif")

locked_out <- merge(deep, current, lic, shared)

rm(deep,
   current,
   lic,
   shared)

###################

#Select planning units with current MPAs

locked_in <- pu[pu$locked_in == 1, ]



###################

#Connectivity data: Add linear constraints



#Select Marine Heatwaves as connectivity feature (The scale is reverse, meaning that
# higher values are smaller heatwaves)
feat_con <- features[[nlayers(features)]]

#Ensure 10% of connectivity
threshold <- cellStats(feat_con, "sum")*0.03

#select conservation features

features <- features[[seq_len(11)]]


#If the solution displays lots of planning units with low connectivity
# convert the continuous values into binary values

# calculate threshold limit
## here we set a threshold limit based on the median
threshold_limit <- quantile(feat_con, probs = 0.5)

# convert continuous values to binary values
feat_con_binary <- round(feat_con <= threshold_limit)




###################


#Assign names to each feature
names(features) <- c("Biodiversity", 
                     "Chl_a_average",
                     "Cold_corals",
                     "Diving_sites",
                     "Fishing_hours_2018-2021",
                     "Kelp",
                     "Knolls",
                     "Mean_carbon_stock",
                     "Seagrasses",
                     "Seamounts",
                     "Warm_corals")


# Create minimum conservation target for each feature
t1 <- c(
        0.3, #Biodiversity
        0.3, #Chl-a_average
        0.3,#Cold corals
        # 0, #Deep water extraction concessions
        # 0, #current resource extraction concessions
        0.5, #Diving sites
        0.1, #fishing hours,
        0.3, #Kelp
        0.3, #Knolls
        # 0, #Extraction licenses
        0.3, #Mean carbon stock
        # 0, #Shared production
        0.3, #Seagrassses,
        0.3, #Seamounts
        0.3 #warmcorals
        # ,0.3 #Invert heatwaves
        
        
        
)
        
###################

# set.seed(1234)
set.seed(666)

#Base problem

p0 <- problem(pu, features, cost_column="cost") %>% 
        add_min_set_objective() %>% 
        add_relative_targets(t1) %>%
        add_binary_decisions() %>% 
        add_locked_in_constraints(locked_in) %>% 
        add_locked_out_constraints(locked_out) %>%
        add_gap_portfolio(number_solutions = 1000, pool_gap = 0.1) %>%
        add_gurobi_solver(verbose = FALSE, gap=0.1, threads = 14)


#Solve 
s0 <- solve(p0)


s0_df <- s0 %>% 
        as.data.frame()

solution_columns <- which(grepl("solution", names(s0_df)))


#Get the names string 
solution_columns <- colnames(s0_df[, solution_columns])



## Calculate selection frequency for each planning unit
s0_df $selection_frequencies <- rowSums(s0_df [, solution_columns ])


## Tranform it into simple feature
s0_df <- st_as_sf(s0_df)


solution <- s0_df %>% 
        filter(selection_frequencies==1000) %>%
        filter(locked_in!=1) %>% 
        as.data.frame() %>% 
        dplyr::select(id, locked_in, geometry) %>% 
        st_as_sf(.)


# plot(solution)

mapview::mapview(solution)





###################

# Connectivity 50% of low marine heatwaves


p1_con <- p0 %>% 
        add_linear_constraints(data=feat_con_binary, 
                               threshold=threshold,
                               sense=">=")  
#Notes: Currently the linear constraints are not working as intended
# Priority zones are not clumping together

#Solve 
s1 <- solve(p1_con)


s1_df <- s1 %>% 
        as.data.frame()

solution_columns <- which(grepl("solution", names(s1_df)))


#Get the names string 
solution_columns <- colnames(s1_df[, solution_columns])



## Calculate selection frequency for each planning unit
s1_df $selection_frequencies <- rowSums(s1_df [, solution_columns ])


## Tranform it into simple feature
s1_df <- st_as_sf(s1_df)


solution <- s1_df %>% 
        filter(selection_frequencies==1000) %>%
        filter(locked_in!=1) %>% 
        as.data.frame() %>% 
        dplyr::select(id, locked_in, geometry) %>% 
        st_as_sf(.)


# plot(solution)

mapview::mapview(solution)


###################













###################

#Add boundary penalties to reduce spatial fragmentation

# precompute the boundary data
pu_boundary_data <- boundary_matrix(pu)

# rescale boundary data
pu_boundary_data@x <- scales::rescale(pu_boundary_data@x, 
                              to = c(0.1, 100))


p2_boundary <- p0 %>% 
        add_boundary_penalties(penalty = 0.0001, 
                               data = pu_boundary_data) 

#Solve 
s2 <- solve(p2_boundary)


s2_df <- s2 %>% 
        as.data.frame()

solution_columns <- which(grepl("solution", names(s2_df)))


#Get the names string 
solution_columns <- colnames(s2_df[, solution_columns])



## Calculate selection frequency for each planning unit
s2_df $selection_frequencies <- rowSums(s2_df [, solution_columns ])


## Tranform it into simple feature
s2_df <- st_as_sf(s2_df)


solution <- s2_df %>% 
        filter(selection_frequencies==1000) %>%
        filter(locked_in!=1) %>% 
        as.data.frame() %>% 
        dplyr::select(id, locked_in, geometry) %>% 
        st_as_sf(.)


# plot(solution)

mapview::mapview(solution)

 
###################




# Connectivity penalties and constraints, and Boundaries

# compute connectivity scores
feat_con_scores <- connectivity_matrix(pu, feat_con)

# rescale scores
feat_con_scores@x <- scales::rescale(feat_con_scores@x, to = c(0.1, 100))


p3 <- p0 %>% 
        add_boundary_penalties(penalty = 0.0000001, 
                               data = pu_boundary_data) %>% 
        add_linear_constraints(data=feat_con,
                               threshold=threshold,
                               sense=">=") %>%
        add_connectivity_penalties(penalty = 0.0000000001, data = feat_con_scores)

p3
#Solve 
s3 <- solve(p3)


s3_df <- s3 %>% 
        as.data.frame()

solution_columns <- which(grepl("solution", names(s3_df)))


#Get the names string 
solution_columns <- colnames(s3_df[, solution_columns])



## Calculate selection frequency for each planning unit
s3_df $selection_frequencies <- rowSums(s3_df [, solution_columns ])


## Tranform it into simple feature
s3_df <- st_as_sf(s3_df)


solution <- s3_df %>% 
        filter(solution_1==1) %>%
        # filter(locked_in!=1) %>%
        as.data.frame() %>% 
        dplyr::select(id, locked_in, geometry) %>% 
        st_as_sf(.)


# plot(solution)

mapview::mapview(solution, zcol="locked_in")













p4 <- p0 %>% 
        add_boundary_penalties(penalty = 0.0000001, 
                               data = pu_boundary_data) %>% 
        add_linear_constraints(data=feat_con,
                               threshold=threshold,
                               sense=">=") %>%
        add_connectivity_penalties(penalty = 0.00000001, data = feat_con_scores)


#Solve 
s4 <- solve(p4)


s4_df <- s4 %>% 
        as.data.frame()

solution_columns <- which(grepl("solution", names(s4_df)))


#Get the names string 
solution_columns <- colnames(s4_df[, solution_columns])



## Calculate selection frequency for each planning unit
s4_df $selection_frequencies <- rowSums(s4_df [, solution_columns ])


## Tranform it into simple feature
s4_df <- st_as_sf(s4_df)


solution <- s4_df %>% 
        filter(selection_frequencies==1000) %>%
        # filter(locked_in!=1) %>%
        as.data.frame() %>% 
        dplyr::select(id, locked_in, geometry) %>% 
        st_as_sf(.)


# plot(solution)

mapview::mapview(solution, zcol="locked_in")
























# calculate feature representation
print(eval_feature_representation_summary(p3,
                                           s3[, "solution_500"]),
                                                width = Inf)


mapview::mapview(locked_out)



print(attr(s1, "runtime"))
print(attr(s1, "status"))



print(eval_cost_summary(p3, s3[, "solution_1"]))

class(s1)

plot(st_as_sf(s1[, "solution_2"]), main = "Solution_2")




