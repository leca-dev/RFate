### HEADER #####################################################################
##' @title Load a dataset
##' 
##' @name .loadData
##'
##' @author Maya Guéguen
##' 
##' @description This function loads one of the available datasets :
##' 
##' \describe{
##'   \item{Champsaur_PFG}{elements to create Plant Functional Groups (PFG) 
##'   over Champsaur valley, in Ecrins National Park (PNE) \cr
##'   \item{Champsaur_params}{all necessary files to build the \code{FATE} 
##'   simulation folder as well as the parameter files \cr
##'   \item{Champsaur_results}{results obtained from outputs of \code{FATE} 
##'   simulation \cr
##' }
##' 
##' @param data.name a \code{string} corresponding to the name of the dataset 
##' that will be loaded
##' @param format.name a \code{string} corresponding to the downloading format way 
##' (either \code{RData} or \code{7z})
##' 
##' @return 
##' 
##' \strong{\cr\cr#########################################################\cr}
##' \strong{>>> Champsaur_PFG}
##' \strong{\cr#########################################################\cr\cr}
##' 
##' A \code{list} object with 18 elements to help building the Plant Functional 
##' Group :
##'   
##'   \describe{
##'     \item{name.dataset}{\strong{Champsaur}}
##'     \item{sp.observations}{a \code{data.frame} of dimension 
##'     \code{127257 x 6} \cr
##'     containing releves data about plant species in Champsaur \cr
##'     to be used with the \code{\link{PRE_FATE.selectDominant}} function
##'     \itemize{
##'       \item \strong{sites} : sites ID
##'       \item \strong{species} : species ID
##'       \item \strong{abundBB} : Braun-Blanquet abundance
##'       \item \strong{abund} : relative abundance obtained from the 
##'       \code{abund_BB} column with the 
##'       \code{\link{PRE_FATE.abundBraunBlanquet}} function
##'       \item \strong{habitat} : habitat classes from 
##'       [CESBIO2018](http://osr-cesbio.ups-tlse.fr/~oso/posts/2019-03-25-carte-s2-2018%20/)
##'       (with some classes gathering)
##'       \item \strong{TYPE} : type of records, exhaustive (\code{COMMUNITY}) 
##'       or single occurrences (\code{OCCURRENCE})
##'     }
##'     }
##'     \item{rules.selectDominant}{a \code{vector} containing parameters to be 
##'     given to the \code{\link{PRE_FATE.selectDominant}} function \cr
##'     (\code{doRuleA}, \code{rule.A1}, \code{rule.A2_quantile}, \code{doRuleB}, 
##'     \code{rule.B1_percentage}, \code{rule.B1_number}, \code{rule.B2}, \code{doRuleC})
##'     }
##'     \item{sp.SELECT}{a \code{list} obtained from the 
##'     \code{\link{PRE_FATE.selectDominant}} function run with 
##'     \code{sp.observations} and \code{rules.selectDominant}
##'     }
##'     \item{sp.traits}{a \code{data.frame} of dimension \code{250 x 23} \cr
##'     containing traits for dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function
##'     \itemize{
##'       \item \strong{species} : species ID
##'       \item \strong{GROUP} : rough generalization of Raunkier life-forms 
##'       (Phanerophyte, Chamaephyte, Herbaceous)
##'       \item \strong{MATURITY / LONGEVITY / HEIGHT / LDMC / LNC / SEEDM / 
##'       SLA} : numerical values
##'       \item \strong{LONGEVITY_log / HEIGHT_log / LDMC_log / LNC_log / 
##'       SEEDM_log / SLA_log} : log-transformed numerical values
##'       \item \strong{DISPERSAL / LIGHT / NITROGEN / NITROGEN_TOLERANCE / 
##'       MOISTURE / GRAZ_MOW_TOLERANCE / HABITAT / STRATEGY} : categorical 
##'       values
##'     }
##'     }
##'     \item{sp.traits.P}{a \code{data.frame} of dimension \code{19 x 9} \cr
##'     a subset of \code{sp.traits} (LONGEVITY_log / HEIGHT_log / SEEDM_log 
##'     / SLA_log / DISPERSAL / LIGHT / NITROGEN) for Phanerophyte dominant 
##'     species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function
##'     }
##'     \item{sp.traits.C}{a \code{data.frame} of dimension \code{39 x 10} \cr
##'     a subset of \code{sp.traits} (LONGEVITY_log / HEIGHT_log / SEEDM_log 
##'     / SLA_log / LNC_log / DISPERSAL / LIGHT / NITROGEN) for Chamaephyte 
##'     dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function
##'     }
##'     \item{sp.traits.H}{a \code{data.frame} of dimension \code{192 x 7} \cr
##'     a subset of \code{sp.traits} (HEIGHT_log / SLA_log / LDMC_log / 
##'     LNC_log / LIGHT) for Herbaceous dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function
##'     }
##'     \item{mat.habitat}{\code{matrix} of dimension \code{240 x 240} \cr
##'     containing habitat dissimilarity distance (1 - Schoeners' D, obtained 
##'     with \code{\link[FD]{gowdis}}) for dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function}
##'     \item{tab.dom.PA}{\code{matrix} of dimension \code{13654 x 264} \cr
##'     containing dominant species occurrences and absences (obtained from 
##'     \code{sp.SELECT} object and absences corrected with the \code{TYPE} 
##'     information of \code{sp.observations}), to build \code{mat.overlap}}
##'     \item{tab.env}{\code{matrix} of dimension \code{13590 x 5} \cr
##'     containing environmental values (bio1, bio12, slope, dem, CESBIO2018) 
##'     for sites, to build \code{mat.overlap}}
##'     \item{mat.overlap}{\code{matrix} of dimension \code{243 x 243} \cr
##'     containing niche overlap distance (1 - Schoeners' D, obtained with 
##'     \code{\link[ecospat]{ecospat.niche.overlap}}) for dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function}
##'     \item{sp.DIST.P}{a \code{list} obtained from the 
##'     \code{\link{PRE_FATE.speciesDistance}} function run for Phanerophyte 
##'     dominant species with \code{mat.habitat}, \code{mat.overlap} and 
##'     \code{sp.traits.P} parameters}
##'     \item{sp.DIST.C}{a \code{list} obtained from the 
##'     \code{\link{PRE_FATE.speciesDistance}} function run for Chamaephyte 
##'     dominant species with \code{mat.habitat}, \code{mat.overlap} and 
##'     \code{sp.traits.C} parameters}
##'     \item{sp.DIST.H}{a \code{list} obtained from the 
##'     \code{\link{PRE_FATE.speciesDistance}} function run for Herbaceous 
##'     dominant species with \code{mat.habitat}, \code{mat.overlap} and 
##'     \code{sp.traits.H} parameters}
##'     \item{PFG.species}{\code{data.frame} of dimension \code{232 x 5} \cr
##'     containing dominant species information relative to PFG \cr
##'     obtained from the \code{\link{PRE_FATE.speciesClustering_step2}} 
##'     function
##'     \itemize{
##'       \item \strong{PFG} : name of assigned Plant Functional Group
##'       \item \strong{DETERMINANT} : is the species kept as determinant 
##'       species within the PFG ? \cr (see 
##'       \code{\link{PRE_FATE.speciesClustering_step2}} function for details)
##'       \item \strong{species} : species ID
##'       \item \strong{species_name} : species name (taxonomic)
##'       \item \strong{species_genus} : species genus (taxonomic)
##'     }
##'     }
##'     \item{PFG.traits}{\code{data.frame} of dimension \code{15 x 12} \cr
##'     containing traits for plant functional groups \cr
##'     obtained from the \code{\link{PRE_FATE.speciesClustering_step3}} 
##'     function
##'     \itemize{
##'       \item \strong{PFG} : Plant Functional Group short name
##'       \item \strong{no.species} : number of species within each group
##'       \item \strong{maturity} : MEAN age of maturity
##'       \item \strong{longevity} : MEAN age of lifespan
##'       \item \strong{height} : MEAN height (cm)
##'       \item \strong{light} : MEDIAN Landolt indicator value for light 
##'       preference (from 1 to 5)
##'       \item \strong{dispersal} : MEDIAN classes (from 1 to 7) based on 
##'       dispersal distances and types (Vittoz & Engler)
##'       \item \strong{soil_contrib} : MEAN Landolt indicator value for 
##'       nitrogen preference (from 1 to 5)
##'       \item \strong{soil_tol_min} : MIN tolerance value for nitrogen 
##'       preference (based on Landolt indicators, from 1 to 5)
##'       \item \strong{soil_tol_max} : MAX tolerance value for nitrogen 
##'       preference (based on Landolt indicators, from 1 to 5)
##'       \item \strong{LDMC} : MEAN LDMC
##'       \item \strong{LNC} : MEAN LNC
##'     }
##'     }
##'     \item{PFG.PA}{\code{data.frame} of dimension \code{13654 x 15} \cr
##'     containing PFG occurrences and absences obtained from 
##'     \code{tab.dom.PA} object and the 
##'     \code{\link{PRE_FATE.speciesClustering_step3}} function}
##'   }
##'   
##' \strong{\cr\cr#########################################################\cr}
##' \strong{>>> Champsaur_params}
##' \strong{\cr#########################################################\cr\cr}
##'   
##' A \code{list} object with 12 elements to help building the simulation 
##' files and folders to run a \code{FATE} simulation :
##'   
##'   \describe{
##'     \item{name.dataset}{\strong{Champsaur}}
##'     \item{tab.occ}{a \code{data.frame} of dimension \code{13590 x 15} \cr
##'     containing presence / absence (\code{NA}, \code{0} or \code{1}) values 
##'     per site for each PFG}
##'     \item{tab.env}{a \code{data.frame} of dimension \code{13590 x 5} \cr
##'     containing environmental values (bio1, bio12, slope, dem, CESBIO2018)  
##'     for each site}
##'     \item{tab.xy}{a \code{data.frame} of dimension \code{13590 x 2} \cr
##'     containing coordinates for each site}
##'     \item{stk.var}{a \code{\link[raster]{stack}} object of dimension 
##'     \code{358 x 427} with a resolution of \code{100m} and ETRS89 
##'     projection, containing 3 layers with environmental values and to be 
##'     used to produce PFG SDM}
##'     \item{tab.SUCC}{a \code{data.frame} of dimension \code{15 x 5} \cr
##'     containing PFG characteristics (type, height, maturity, longevity) \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGsuccession}} 
##'     function}
##'     \item{tab.DISP}{a \code{data.frame} of dimension \code{15 x 4} \cr
##'     containing PFG characteristics (d50, d99, ldd) \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGdispersal}} 
##'     function}
##'     \item{tab.DIST}{a \code{data.frame} of dimension \code{15 x 3} \cr
##'     containing PFG characteristics (strategy_tol) \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGdisturbance}} 
##'     function}
##'     \item{tab.LIGHT}{a \code{data.frame} of dimension \code{15 x 3} \cr
##'     containing PFG characteristics (strategy_tol) \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGlight}} 
##'     function}
##'     \item{tab.SOIL}{a \code{data.frame} of dimension \code{15 x 5} \cr
##'     containing PFG characteristics (type, soil_contrib, soil_tol_min, 
##'     soil_tol_max) \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGsoil}} 
##'     function}
##'     \item{stk.wmean}{a \code{\link[raster]{stack}} object of dimension 
##'     \code{358 x 427} with a resolution of \code{100m} and ETRS89 
##'     projection, containing 15 layers with habitat suitability values 
##'     (between 0 and 1) for each PFG}
##'     \item{stk.mask}{a \code{\link[raster]{stack}} object of dimension 
##'     \code{358 x 427} with a resolution of \code{100m} and ETRS89 
##'     projection, containing 3 mask layers with binary values (0 or 1) to be 
##'     used in a \code{FATE} simulation :
##'     \itemize{
##'       \item \strong{Champsaur} : simulation map, where occurs succession
##'       \item \strong{noDisturb} : perturbation map, when there is none
##'       \item \strong{mowing} : perturbation map, where occurs mowing
##'     }
##'     }
##'   }
##' 
##'   
##' \strong{\cr\cr#########################################################\cr}
##' \strong{>>> Champsaur_results}
##' \strong{\cr#########################################################\cr\cr}
##' 
##'
##' @export
##'
##' @importFrom utils download.file
##'
## END OF HEADER ###############################################################

.loadData = function(data.name, format.name)
{
  .testParam_notInValues.m("data.name", data.name, c("Champsaur_PFG", "Champsaur_params"))
  .testParam_notInValues.m("format.name", format.name, c("RData", "7z"))
  full.name = paste0(data.name, ".", format.name)
  
  if (!file.exists(full.name))
  {
    cat("\n > Downloading `",data.name,"` dataset...\n")
    download.file(url = paste0("https://raw.githubusercontent.com/leca-dev/RFate/master/data-raw/", full.name)
                  , destfile = full.name
                  , method = "curl")
    if (!file.exists(full.name))
    {
      stop(paste0("Download of `", data.name,"` dataset failed!"))
    } else
    {
      message(paste0("Download of `", data.name,"` dataset succeeded!"))
      if (format.name == "RData")
      {
        res = get(load(file = paste0("./", full.name)))
        return(res)
      }
    }
  } else
  {
    message(paste0("Download of `", data.name,"` dataset succeeded!"))
    if (format.name == "RData")
    {
      res = get(load(file = paste0("./", full.name)))
      return(res)
    }
  }
}

