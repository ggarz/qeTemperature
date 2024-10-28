#### Attempting to run phipsii values through photosynthesis models

## libraries
# install.packages('R.utils')
library(R.utils)
library(ggplot2)
library(colorRamps)
library(RColorBrewer)

# Load in data frame with values
garry_data <- read.csv("../species_table.csv") # note: a is the coefficient for x2, c is intercept
head(garry_data) #looks like it is reading it OK

##load in dr. smith's model
# load necessary functions
sourceDirectory('functions', modifiedOnly = FALSE)


photosynthesis_model <- function(elevation_m = 0, ca_ppm = 420, temperature_c = 25, par = 1500,
                                 tmean = 25,
                                 vcmax25 = 100, jmax25 = 200,
                                 phi_psii = 0.6895, # rate at 25C from Bernacchi
                                 e_partitioning_coef = 4, 
                                 absorbance = 0.85, 
                                 photosystem_partitioning_coef = 0.5,
                                 theta = 0.85,
                                 phi_psii_tresp = 'no',
                                 b_tresp = 0.0474,
                                 c_tresp = 0.000859,
                                 a_tresp = -0.2){
  
  topt_posch <- b_tresp / (2 * c_tresp)
  #model_avg_phi_psii <- 0.75
  
  #a_tresp <- model_avg_phi_psii - ((b_tresp * topt_posch) - (c_tresp * topt_posch^2)) # a, given the assumption that model average phi psii is at the temperature optimum estimated from the data
  
  patm_pa <- calc_patm(elevation_m) # atmospheric pressure (Pa)
  ca_pa <- ca_ppm * 1e-6 * patm_pa # atmospheric co2 (Pa)
  
  vcmax <- vcmax25 * calc_vcmax_tresp_mult(tleaf = temperature_c, tmean = tmean, tref = 25)
  ci_pa <- 0.7 * ca_pa # intercellular co2 (Pa)
  gammastar_pa <- calc_gammastar_pa(temperature_c, elevation_m) # co2 compensation point (Pa)
  km_pa <- calc_km_pa(temperature_c, elevation_m) # michaelis-menten coefficient for rubisco (Pa)
  mc = (ci_pa - gammastar_pa) / (ci_pa + km_pa) # 
  ac = (vcmax * mc)  # rubisco-limited photosynthesis
  
  if(phi_psii_tresp == "yes"){
    # Bernacchi et al. (2003) temperature response
    phi_psii = a_tresp + (b_tresp * temperature_c) - (c_tresp * temperature_c * temperature_c)
  }else{
    phi_psii
  }
  
  jmax <- jmax25 * calc_jmax_tresp_mult(tleaf = temperature_c, tmean = tmean, tref = 25)
  m <- (ci_pa - gammastar_pa) / (ci_pa + (2 * gammastar_pa))
  psii_light <- absorbance * photosystem_partitioning_coef * par # light getting to psii
  j_a <- theta
  j_b <- -(phi_psii * psii_light + jmax) 
  j_c <- phi_psii * psii_light * jmax
  j <- (-j_b - sqrt(j_b^2 - 4 * j_a * j_c)) / (2 * j_a)
  aj <- ((j/e_partitioning_coef) * m)  # rubp regeneration-limited photosyntehsis
  
  a <- pmin(ac, aj) - (0.015 * vcmax)
  
  results <- data.frame("elevation_m" = elevation_m,
                        "ca_ppm" = ca_ppm,
                        "temperature_c" = temperature_c,
                        "par" = par,
                        "vcmax" = vcmax,
                        "jmax" = jmax,
                        "phi_psii" = phi_psii,
                        "e_partitioning_coef" = e_partitioning_coef, 
                        "absorbance" = e_partitioning_coef, 
                        "photosystem_partitioning_coef" = photosystem_partitioning_coef,
                        "theta" = theta,
                        "patm_pa" = patm_pa,
                        "ca_pa" = ca_pa,
                        "ci_pa" = ci_pa,
                        "gammastar_pa" = gammastar_pa,
                        "km_pa" = km_pa,
                        "mc" = mc,
                        "ac" = ac,
                        "m" = m,
                        "psii_light" = psii_light,
                        "j_a" = j_a,
                        "j_b" = j_b,
                        "j_c" = j_c,
                        "j" = j,
                        "aj" = aj,
                        "a" = a)
  
  results
  
}


## run model
test_model <- photosynthesis_model()

head(garry_data)

ely_model <- photosynthesis_model(phi_psii_tresp = 'yes',
                                  a_tresp = garry_data[1,4], 
                                  b_tresp = garry_data[1,3], 
                                  c_tresp = -garry_data[1,2],
                                  temperature_c = seq(18,48, 2))

pas_model <- photosynthesis_model(phi_psii_tresp = 'yes',
                                  a_tresp = garry_data[2,4], 
                                  b_tresp = garry_data[2,3], 
                                  c_tresp = -garry_data[2,2],
                                  temperature_c = seq(18,48, 2))

scz_model <- photosynthesis_model(phi_psii_tresp = 'yes',
                                  a_tresp = garry_data[3,4], 
                                  b_tresp = garry_data[3,3], 
                                  c_tresp = -garry_data[3,2],
                                  temperature_c = seq(18,48, 2))

sor_model <- photosynthesis_model(phi_psii_tresp = 'yes',
                                  a_tresp = garry_data[4,4], 
                                  b_tresp = garry_data[4,3], 
                                  c_tresp = -garry_data[4,2],
                                  temperature_c = seq(18,48, 2))

### Making graphs! ###

#creating data frame

ely_df <- ely_model
ely_df$species <- "ely"

pas_df <- pas_model
pas_df$species <- "pas"

scz_df <- scz_model
scz_df$species <- "scz"

sor_df <- sor_model
sor_df$species <- "sor"

species_df <- rbind(ely_df, pas_df, scz_df, sor_df)

options(max.print = 2000)

### chatgpt's best version of graph
aj_lineplot <- ggplot(species_df, aes(x = temperature_c, y = aj, color = species, group = species)) +
  theme(legend.position = 'top', 
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.25)),
        axis.text.y = element_text(size = rel(2.25)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey'),) +
  geom_line(size = 1) + 
  labs(x = "Temperature (°C)",
       y = "Aj",
       color = "Species")

species_colors <- c("ely" = "#1887ab", 
                    "pas" = "#87ab18", 
                    "scz" = "#d34467", 
                    "sor" = "#8518ab")

###temp optimum graph??
#create new column that is the aj value divided by the maximum aj value for that species
ajmax_lineplot <- ggplot(subset(species_df, species == 'ely'), aes(x = temperature_c, y = aj/max(aj))) +
  theme(legend.position = 'top', 
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.25)),
        axis.text.y = element_text(size = rel(2.25)),
        panel.background = element_rect(fill = 'white',),
        panel.grid.major = element_line(colour = 'grey'), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  geom_line(size = 2, colour = '#1887ab') + 
  geom_line(data = subset(species_df, species == 'pas'), size = 2, colour = '#87ab18') + 
  geom_line(data = subset(species_df, species == 'scz'), size = 2, colour = '#d34467') + 
  geom_line(data = subset(species_df, species == 'sor'), size = 2, colour = '#8518ab') + 
  geom_vline(xintercept= subset(species_df, species == 'ely' & aj == max(subset(species_df, species == 'ely')$aj))$temperature_c,
             size = 1, color = '#1887ab') +
  geom_vline(xintercept= subset(species_df, species == 'pas' & aj == max(subset(species_df, species == 'pas')$aj))$temperature_c,
             size = 1, color = '#87ab18', lty = 2) +
  geom_vline(xintercept= subset(species_df, species == 'scz' & aj == max(subset(species_df, species == 'scz')$aj))$temperature_c,
             size = 1, color = '#d34467') +
  geom_vline(xintercept= subset(species_df, species == 'sor' & aj == max(subset(species_df, species == 'sor')$aj))$temperature_c,
             size = 1, color = '#8518ab') +
  labs(x = "Temperature (°C)",
       y = "Aj")


# Ensure the directory exists
if (!dir.exists('plots')) {
  dir.create('plots')
}

# Save 'aj_lineplot' as a TIFF
ggsave(filename = 'plots/aj_lineplot.png',
       plot = aj_lineplot,  # Your ggplot object
       width = 8, height = 8, units = 'in', dpi = 300)

# Save 'ajmax_lineplot' as a TIFF
ggsave(filename = 'plots/ajmax_lineplot.png',
       plot = ajmax_lineplot,  # Your ggplot object
       width = 8, height = 8, units = 'in', dpi = 300)

