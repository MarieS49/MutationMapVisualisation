rm(list=ls())

########################################################################################################
########################################################################################################
########################################################################################################


#############################################################
############### Mutation Mapp code ##########################
##############################################################

#This code provide an application to manipulate and add a graphic view of output of 
# the mutation map obtained with phytime





#############################################################
############ Library and package used in this code ##########
#############################################################

if (!require("shiny")) {
  install.packages("shiny", dependencies = TRUE)
  library(shiny)
}
if (!require("shinythemes")) {
  install.packages("shinythemes", dependencies = TRUE)
library(shinythemes)
}
if (!require("pillar")) {
  install.packages("pillar", dependencies = TRUE)
  library(pillar)
}
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}
if (!require("ggforce")) {
  install.packages("ggforce", dependencies = TRUE)
  library(ggforce)
}
if (!require("reshape2")) {
  install.packages("reshape2", dependencies = TRUE)
  library(reshape2)
}
if (!require("plotrix")) {
  install.packages("plotrix", dependencies = TRUE)
  library(plotrix)
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard", dependencies = TRUE)
  library(shinydashboard)
}
if (!require("ggtree"))
{ source("https://bioconductor.org/biocLite.R")
  biocLite("ggtree")
  library(ggtree)
}
if (!require("treeio")){
  install.packages("treeio", dependencies = TRUE)
  library(treeio)
}
if (!require("ape")){
  install.packages("ape", dependencies = TRUE)
  library(ape)
}
if (!require("grid")){
  install.packages("grid", dependencies = TRUE)
  library(grid)
}
if (!require("gridExtra")){
  install.packages("gridExtra", dependencies = TRUE)
  library(gridExtra)
}
if (!require("seqinr")){
  install.packages("seqinr", dependencies = TRUE)
  library(seqinr)
}
if (!require("dplyr")){
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}
if(!require("stringr")){
  install.packages("stringr", dependencies = TRUE)
  library(stringr)
}
if(!require("phangorn")){
  install.packages("phangorn", dependencies = TRUE)
  library(phangorn)
}
if(!require("phytools")){
  install.packages("phytools", dependencies = TRUE)
  library(phytools)
}
if(!require("data.table")){
  install.packages("data.table")
  library(data.table)
}
if(!require("microbenchmark")){
  install.packages("microbenchmark")
  library(microbenchmark)
}
if(!require("kimisc")){
  install.packages("kimisc")
  library(kimisc)
}
if(!require("doBy")){
  install.packages("doBy")
  library(doBy)
}


##################################################################################################
##################################################################################################
##################################################################################################

#################################################################
###################     FUNCTIONS       #########################
#################################################################



############ Function to calculate coord of point B ##############################################
coord_pointB <- function (x0, y0, angle, dist)
{
  epsilon = 0.001
  aleat = sample(1:10, 1)
  if (aleat <= 5)
  {
   # dist = 1.0
    x1 = x0 + dist*cos(angle) + epsilon
    y1 = y0 + dist*sin(angle) + epsilon
  }else{
    x1 = x0 + dist*cos(angle) - epsilon
    y1 = y0 + dist*sin(angle) - epsilon
  }
  return (list(x1, y1))
}
##################################################################################################


# #########  Function to determine angle of the arrow according to mutation type on a complete circle #####################

# type_de_mut <- function(type)
# {
#   if(type == 0 | type == 11)
#   {
#     #    0 => A<->C
#     deg = 90
#     rad = pi/2
#   }
#   if (type == 1| type == 8)
#   {
#     #   1 => A<->G
#     deg = 45
#     rad = pi/4
#   }
#   if (type == 2| type == 3)
#   {
#     #   2 => A<->T
#     deg = 315
#     rad = 7*pi/4
#   }
#   if (type == 9| type == 6)
#   {
#     #   3 => C<->G
#     deg = 135
#     rad = 3*pi/4
#   }
#   if (type == 10| type == 5)
#   {
#     #   4 => C<->T
#     deg = 225
#     rad = 5*pi/4
#   }
#   if (type == 7| type == 4)
#   {
#     #   5 => G<->T
#     deg = 270
#     rad = 3*pi/2
#   }
#   #  angle_mut = deg_en_rad(deg)
#   angle_mut = rad
#   
#   return (angle_mut)
# }



#########  Function to determine angle of the arrow according to mutation type one, on half circle #####################
type_de_mut <- function(type)
{
  if(type == 0 | type == 11)
  {
    #    0 => A<->C
    deg = 45
    rad = pi/4
  }
  if (type == 1| type == 8)
  {
    #   1 => A<->G
    deg = 30
    rad = pi/6
  }
  if (type == 2| type == 3)
  {
    #   2 => A<->T
    deg = 15
    rad = pi/12
  }
  if (type == 9| type == 6)
  {
    #   3 => C<->G
    deg = 360
    rad = 2*pi
  }
  if (type == 10| type == 5)
  {
    #   4 => C<->T
    deg = 345
    rad = (23*pi)/12
  }
  if (type == 7| type == 4)
  {
    #   5 => G<->T
    deg = 330
    rad = (11*pi)/6
  }
  #  angle_mut = deg_en_rad(deg)
  angle_mut = rad
  
  return (angle_mut)
}
#####################################################################################################




############ Function to add label to mutation type ##################################################
color_mut <- function(type)
{
  if(type == 0)
  {
    lab = "A->C"
  }
  if (type == 1)
  {
    lab = "A->G"
    
  }
  if (type == 2)
  {
    lab = "A->T"
    
  }
  if (type == 3)
  {
    lab = "T->A"
    
  }
  if (type == 4)
  {
    lab = "T->G"
    
  }
  if (type == 5)
  {
    lab = "T->C"
    
  }
  if(type == 6)
  {
    lab = "G->C"
    
  }
  if (type == 7)
  {
    lab = "G->T"
    
  }
  if (type == 8)
  {
    lab = "G->A"
    
  }
  if (type == 9)
  {
    lab = "C->G"
    
  }
  if (type == 10)
  {
    lab = "C->T"
    
  }
  if (type == 11)
  {
    lab = "C->A"
    
  }
  return (lab)
}

## fonctions pour attribuer une couleur a un type de mutation exemple mut == O correspond à la mutation de A vers C
## http://www.sthda.com/french/wiki/couleurs-dans-r#les-noms-de-couleurs-reconnus-par-r

couleurParMut <- function(mutmut)
{
  if (mutmut == 0 )
  {
    col = "#0000CC"
  }
  else if (mutmut == 1)
  {
    col = "#33CCFF"
  }
  else if (mutmut == 2)
  {
    col = "#6699FF"
  }
  else if (mutmut == 3)
  {
    col = "#FF00FF"
  }
  else if (mutmut == 4)
  {
    col = "#660066"
  }
  else if (mutmut == 5)
  {
    col = "#FF99FF"
  }
  else if (mutmut == 6)
  {
    col = "#FF3300"
  }
  else if (mutmut == 7)
  {
    col = "#FF0000"
  }
  else if (mutmut == 8)
  {
    col = "#FF66FF"
  }
  else if (mutmut == 9)
  {
    col = "#00CC66"
  }
  else if (mutmut == 10)
  {
    col = "#006600"
  }
  else if (mutmut == 11)
  {
    col = "#00FF66"
  }
  else
  {
    col = "#000000"
  }
  return(col)
}
## Cette fonctions a le meme role que la precedente a la difference pres que la couleur est differente selon que se soit une transition ou une transversion
## http://www.sthda.com/french/wiki/couleurs-dans-r#les-noms-de-couleurs-reconnus-par-r
couleurGC <- function(mutmut)
{
  if (mutmut == 0  )
  {
    col = "#000000"
  }
  else if (mutmut == 1)
  {
    col = "#000000"
  }
  else if (mutmut == 2)
  {
    col = "#000000"
  }
  else if (mutmut == 3)
  {
    col = "#000000"
  }
  else if (mutmut == 4)
  {
    col = "#000000"
  }
  else if (mutmut == 5)
  {
    col = "#000000"
  }
  else if (mutmut == 6)
  {
    col = "#FF0000"
  }
  else if (mutmut == 7)
  {
    col = "#000000"
  }
  else if (mutmut == 8)
  {
    col = "#000000"
  }
  else if (mutmut == 9)
  {
    col = "#FF0000"
  }
  else if (mutmut == 10)
  {
    col = "#000000"
  }
  else if (mutmut == 11)
  {
    col = "#000000"
  }
  else 
  {
    col = "#000000"
  }
  return(col)
}
## Ici, la couleur est donnee au type de mutation G vers C et C vers G 
## http://www.sthda.com/french/wiki/couleurs-dans-r#les-noms-de-couleurs-reconnus-par-r
couleurTransVs <- function(mutmut)
{
  if (mutmut == 0  )
  {
    col = "#000000"
  }
  else if (mutmut == 1)
  {
    col = "#FF0000"
  }
  else if (mutmut == 2)
  {
    col = "#000000"
  }
  else if (mutmut == 3)
  {
    col = "#000000"
  }
  else if (mutmut == 4)
  {
    col = "#000000"
  }
  else if (mutmut == 5)
  {
    col = "#FF0000"
  }
  else if (mutmut == 6)
  {
    col = "#000000"
  }
  else if (mutmut == 7)
  {
    col = "#000000"
  }
  else if (mutmut == 8)
  {
    col = "#FF0000"
  }
  else if (mutmut == 9)
  {
    col = "#000000"
  }
  else if (mutmut == 10)
  {
    col = "#FF0000"
  }
  else if (mutmut == 11)
  {
    col = "#000000"
  }
  else 
  {
    col = "#000000"
  }
  return(col)
}


# Fonction utilisee dans le plot de l'arbre pour colorier la racine de l'arbre
couleurRacine <- function(mutmut)
{
  if (mutmut == 0 || mutmut == 1 || mutmut == 2 )
  {
#    col = "#0099FF"
    col = "Blue"
  }
  else if (mutmut == 3 || mutmut == 4 || mutmut ==5 )
  {
#    col = "#FF0000"
    col = "Purple"
  }
  else if (mutmut == 6 || mutmut == 7 || mutmut == 8 )
  {
#    col = "#993300"
    col = "Green" 
  }
  else if (mutmut == 9 || mutmut == 10 || mutmut == 11 )
  {
#    col = "#33FFCC"
    col = "Yellow"
  }
  else 
  {
    col = "#000000"
  }
  return(col)
}

# fonction permettat de colorier les descendant dans l'arbre apres une mutation
couleurDescendant <- function(mutmut)
{
  if (mutmut == 3 || mutmut == 8 || mutmut == 11 )
  {
#    col = "#0099FF"
    col = "Blue"
  }
  else if (mutmut == 2 || mutmut == 7 || mutmut == 10 )
  {
  #  col = "#FF0000"
    col = "Purple"
  }
  else if (mutmut == 1 || mutmut == 4 || mutmut == 9 )
  {
  #  col = "#993300"
    col = "Green" 
  }
  else if (mutmut == 0 || mutmut == 5 || mutmut == 6 )
  {
  #  col = "#33FFCC"
    col = "Yellow"
  }
  else 
  {
    col = "#000000"
  }
  return(col)
}

###################################################################################################################



########### Function to know the page number according to ############################
########### the inpoutfile number and the number of graphique per page ###############
nbpage <- function(nrow, ncol, data){
  entier = floor(data/(nrow*ncol))
  rest = data %% (nrow*ncol)
  if (rest == 0)
  {
    count = entier
  }
  else
  {
    count = entier+1
  }
  return (count)
}
##################################################################################################################



############# Function to build the histogram tab   #################################
histoo <- function (tab){
  tableau = data.frame()
  for (h in unique(tab$filename))
  {
    hh = subset.data.frame(tab, tab$filename == h)
    kk = as.data.frame(table(hh$num_carte))
    position = unique(hh$real_pos)
    kk[,3]= position[2] 
    tableau = rbind(tableau, kk)
  }
  colnames(tableau)=c("class", "count", "real_pos")
  return(tableau)
}
################################################################################################################
mean_histo<- function(histo){
  tot = 0
  for (i in 1:nrow(histo))
  {
    denominateur = (histo[i,1]*histo[i,2])
    tot = tot + denominateur
  }
  numerateur = sum(histo$count)
  mean = tot/numerateur
  return(mean)
}


###################################################################################################################

########### Function to return limit of plot #######################################

lim_min <- function(data)
{
  l_mn = floor(min(data$x_1,data$y_1)-1)
  return(l_mn)
}

lim_max <- function(data)
{
  l_mx = floor(max(data$x_1,data$y_1)+1)
  return(l_mx)
}

###################################################################################################################
#####################################      Generic Data     #######################################################
###################################################################################################################


######### Data Tab to compute the DNA legend one one circle ###########################################
# names = c("m_d", "m_a", "X_0", "Y_0", "X_1", "Y_1", "OneColorByMut", "Transvs", "GC")
# c1 = c("C",           "A",            "G",        "A",          "T",            "A",          "G",            "C",         "T",    "C",           "T",             "G")
# c2 = c("A",            "C",          "A",          "G",         "A",            "T",          "C",            "G",         "C",    "T",             "G",            "T")
# c3 = c(0.00,          0.00,        0.10,     0.3535534,       0.10,        0.3535534,        -0.1,          -0.3535534, -0.1,    -0.3535534,             0,          0)
# c4 = c(0.15,           0.5,       0.10,       0.3535534,     -0.10,         -0.3535534,       0.1,          0.3535534,  -0.1,     -0.3535534,           -0.15,        -0.5)
# c5 = c(6.123032e-17, 6.123032e-17, 0.3535534, 7.071068e-01,   0.3535534,   7.071068e-01,  -0.3535534,  -7.071068e-01, -0.3535534, -7.071068e-01, -1.836910e-16, -1.836910e-16)
# c6 =c(0.5,           1.0000000,    0.3535534, 0.7071068,    -0.3535534,  -0.7071068,      0.3535534,     0.7071068,   -0.3535534,   -0.7071068 ,        -0.5,              -0.98)
# c7 = c("#00FF66",          "#0000CC",    "#FF66FF",    "#33CCFF",     "#FF00FF",  "#6699FF",      "#FF99FF",        "#006600",    "#FF3300",      "#00CC66",      "#660066",            "#FF0000")
# c8 = c("#000000",          "#000000",    "#FF0000",    "#FF0000",           "#000000",            "#000000",      "#000000",        "#000000",    "#FF0000",      "#FF0000",      "#000000",            "#000000")
# c9 = c("#000000",   "#000000",     "#000000",    "#000000",    "#000000",    "#000000", "#FF0000"  ,  "#FF0000"  , "#000000", "#000000", "#000000", "#000000")
# d <- data.frame(c1, c2, c3, c4, c5, c6, c7, c8,c9)
# colnames(d)<- names



######### Data Tab to compute the DNA legend one an half circle ###########################################
names = c("m_d", "m_a", "X_0", "Y_0", "X_1", "Y_1",  "OneColorByMut", "Transvs", "GC")
c1 = c("A",            "C",             "A",           "G",              "A",            "T",             "C",            "T",            "C",            "G",             "T",             "G"          )
c2 = c("C",            "A",             "G",           "A",              "T",            "A",             "T",            "C",            "G",            "C",             "G",             "T"          )
c3 = c(0.1414214,      0.4242641,        0.1732051,     0.519615,        0.1931852,       0.5795555,       0.1931852,      0.5795555,      0.2,            0.6,             0.1732051,       0.5196152   )
c4 = c(0.1414214,      0.4242641,        0.1,           0.3,             0.05176381,      0.1552914,      -0.05176381,    -0.1552914,     -4.898587e-17,  -1.469576e-16,   -0.1,            -0.3         )
c5 = c(0.4242641,      0.8485281,        0.519615,      1.03923,         0.5795555,       1.159111,        0.5795555,      1.159111,       0.6,            1.2,             0.5196152,       1.03923     )
c6 = c(0.4242641,      0.8485281,        0.3,           0.6,             0.1552914,       0.3105829,      -0.1552914,     -0.3105829,     -1.469576e-16,  -2.939152e-16 ,  -0.3,            -0.6         )
c7 = c("#0000CC",     "#00FF66",        "#33CCFF",    "#FF66FF",         "#6699FF",       "#FF00FF",      "#006600",        "#FF99FF",     "#00CC66",      "#FF3300",      "#660066",        "#FF0000"   )
c8 = c("#000000",     "#000000",        "#FF0000",    "#FF0000",         "#000000",       "#000000",      "#FF0000",        "#FF0000",     "#000000",      "#000000",      "#000000",        "#000000"   )
c9 = c("#000000",     "#000000",        "#000000",    "#000000",         "#000000",       "#000000",       "#000000",       "#000000",     "#FF0000"  ,    "#FF0000",      "#000000",        "#000000"   )
d <- data.frame(c1, c2, c3, c4, c5, c6, c7, c8,c9)
colnames(d)<- names



######################################################################################################################
######################################################################################################################

AA.palette <- c(
  "A" = "Blue",
  "G" = "Orange",
  "C" = "Pink", 
  "D" = "Magenta", 
  "E" = "Magenta", 
  "N" = "Green", 
  "Q" = "Green", 
  "I" = "Blue",
  "L" = "Blue",
  "M" = "Blue",
  "V" = "Blue",
  "F" = "Blue", 
  "W" = "Blue",
  "Y" = "Cyan",
  "H" = "Cyan", 
  "K" = "Red", 
  "R" = "Red", 
  "P" = "Yellow", 
  "S" = "Green", 
  "T" = "Green", 
  "-" = "black"
)

DNA.palette=c(
  "A"= "Blue", 
  "C" = "Yellow", 
  "G" = "Green", 
  "T" = "Purple", 
  "-" = "black", 
  "N" = "red",
  "a"= "Blue", 
  "c" = "Yellow", 
  "g" = "Green", 
  "t" = "Purple", 
  "-" = "black", 
  "n" = "red",
  "d" = "red",
  "m" = "red",
  "r" = "red",
  "s" = "red",
  "t" = "red",
  "v" = "red",
  "w" = "red",
  "y" = "red")



######################################################################################################################
######################################################################################################################
##########################################       UI         ##########################################################
######################################################################################################################
######################################################################################################################



ui<- navbarPage("MutMapp",
      theme = shinytheme("slate"),


  tabPanel("Welcome", h2(" Welcome in the Mutation Map vizualisation application"),
                      h3 (" Utilities "),
                      h5(" This application permit to draw mutation map(s) for different sites along your DNA alignment.
                                       Your Mutation Map file(s) should be issue from phytime and should be a tab separator table.
                         yours fasta files should be fasta with .fasta extension. Tree files should be in Nexus format"),
                      h3 ("Description"),
                      h5("One the first page you could download your different files."), 
                      h5( "The alignment plot could be draw on this page and you will be able to navigate along your sequences with the different buttons"),
                      h5("The second page named Diagramm allows to plot the mutation mapps according to there time from the present and there type."),
                      h5("Some options are available:"),
                      h5("You can choose to plot one or more diagramm on one page"), 
                      h5("You can choose how to present the plot (on one or more rows, one or more columns)"),
                      h5("You can choose to put all the diagramm at the same scale or not"),
                      h5("If you choose the view 'One_Site', you will be able to draw the iterations one by one"),
                      h5("You can choose the color with three possibilities :"),
                      h5("  - One color by mutation type (A to C with one color and C to A with one other color for exemple)"),
                      h5("  - All the mutation in black except the transition in red"),
                      h5("  - All the mutation in black except the G to C or C to G in red to check the potentially enrichissment in GC"),
                      h5(" At the end, the third page"), 
                      h5(" On this page you can plot the phylogentic tree with the branch colored by the mutation(s)"), 
                      h5(" This one is possible only if you have dowload alignment and mutation mapp files"),
                      h5(" You will be able to plot the differents iterations one by one "),
                      h4("More Information"),
                      h5(" If you need more informations you could contact me : suez@lirmm.fr"), 
                      h3(" Acknowledgement"),
                      h5(" Thank you to M. Mariadassou for the construction of the alignement plot and for help in general on other functions "),
                      h5 ("Thanks a lot to A. Behdenna for the constant advice and help in general")
           
  ),


  tabPanel("Data", 
         fluidRow( "On this page your files will be treat for the next step",
                   sidebarLayout(
                     sidebarPanel(
                       
                      fileInput("Align", label = h3("Alignment Fasta File(s) :"),multiple = TRUE, accept = c(
                           "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv", "")),
                      numericInput("Begin", label = h5("Begin alignement position view"), value = 1), 
                      numericInput("End", label = h5("Begin alignement position view"), value = 50),
                       
                   
                      fileInput("file", label = h3("Mutation File(s) input:"),multiple = TRUE, accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")) ,
                      
                      fileInput("fileTree", label = h3("Tree file(s) input:"),multiple = TRUE, accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"), buttonLabel = "Download tree files")
                       ),
                     
                 mainPanel(
                         plotOutput("Alignement", height = "100%", width = "100%")
                       )
                     
                   )
         )
  ),
                       
  tabPanel("Diagramme", tags$head(tags$style("#MutMap{height:100vh !important;}")), ## la petite ligne tags$head permet d'avoir le graph sur TOUTE la hauteur de page :) 
           fluidRow( "On this page you can plot diagramme of mutation map",
            sidebarLayout(position = "left",
             sidebarPanel( 
                          radioButtons("Data_option", label = h4("Data Option"),
                                       c("Complete_data", "choose_individuals")),
                          conditionalPanel("input.Data_option === 'choose_individuals'",
                                           uiOutput("clade")),

                          radioButtons("Plot_option", label = h4("Graphical Option"),
                                   c("General_view", "One_site"), inline = TRUE),
                            
                          conditionalPanel("input.Plot_option === 'One_site'",
                                        selectInput("Iteration_Print", label = h6("How to print Iterations?"),
                                        c("Iteration_One_by_One", 
                                          "All_iterations"))),
                          #                 ),
                           conditionalPanel("input.Iteration_Print === 'Iteration_One_by_One'",
                                            uiOutput("siteAal"), uiOutput("iteraation")),
                          conditionalPanel("input.Iteration_Print === 'All_iterations'",
                                          uiOutput("site")
                                          ),
                          conditionalPanel("input.Plot_option === 'General_view'",
                                          selectInput("n_row", label = h6("Option : number of row"),
                                          c("1" = "1",
                                            "2" = "2",
                                            "3" = "3",
                                            "4" = "4",
                                            "5" = "5",
                                            "6" = "6",
                                            "7" = "7",
                                            "8" = "8", 
                                            "9" = "9"),
                                          selectize = FALSE)),
                         conditionalPanel("input.Plot_option === 'General_view'",
                                          selectInput("n_col", label = h6("Option : number of column"),
                                          c("1" = "1",
                                            "2" = "2",
                                            "3" = "3",
                                            "4" = "4",
                                            "5" = "5",
                                            "6" = "6",
                                            "7" = "7",
                                            "8" = "8", 
                                            "9" = "9"),
                                          selectize = FALSE
                                          )),
                        conditionalPanel("input.Plot_option === 'General_view'",
                                          radioButtons("scale_option", label = h6("graphic scale"),
                                          c("free_scale", "coord_fixed", "ratio_1/1"), inline = TRUE)),
                        checkboxInput("PlotHistogram", label = "Histogram", value = FALSE),
                        uiOutput("page"),
                        br(),
                        radioButtons("legend_type", label = h4("Type of display"),
                                     c("One color by mutation", "View Transition vs Transversion", "View GC"), 
                                     selected = "One color by mutation", inline = FALSE),
                        plotOutput("Legend", height = 300 )
                        ),
                mainPanel(
                          plotOutput("MutMap")
                          )
                )
         )
    ),

  tabPanel("Tree", tags$head(tags$style("#Tree{height:100vh !important;}")),
       fluidRow( "On this page you can plot the nexus tree issue from phytime",
          sidebarLayout(position = "left",
              sidebarPanel(
                   uiOutput("site_al"),
                   uiOutput("Iterations")
                   ),
              mainPanel(
                plotOutput("Tree")
              )
          )
       )
    
  )

  )







########################################################################################################################
########################################################################################################################
######################################        SERVER        ############################################################
########################################################################################################################
########################################################################################################################



server <- function(input, output){
  
  dictionary <- c("AC" = 0,
                  "AG" = 1,
                  "AT" = 2,
                  "TA" = 3, 
                  "TG" = 4, 
                  "TC" = 5,
                  "GC" = 6, 
                  "GT" = 7, 
                  "GA" = 8,
                  "CG" = 9, 
                  "CT" = 10, 
                  "CA" = 11,
                  "-1-1.0" = -1)
  
  
  ## First part, Read Rosetta File and treat the data
  
  Mutation_map <- reactive({
    inFile <- input$file
    if (is.null(inFile))
    {
#      print("PLease load at least one Rosetta file")
      return(NULL)
    }else{
      #      print(inFile$name)
      carte_de_mut_tot = data.frame()
      #      withProgress(message = 'files processing', value = 0, {
      # Number of times we'll go through the loop
      #      n <- length(inFile$name)
      
      for (i in 1: (length(inFile$name)))
      {
        k = 0
        # Increment the progress bar, and update the detail text.
        #        incProgress(1/n, detail = paste("Doing part", i))
        
        filename=inFile$name[i]
        carte_de_mut=read.table(inFile$name[i], header=FALSE, stringsAsFactor=FALSE, fill = TRUE)
        carte_de_mut <- carte_de_mut %>% mutate(Nuc12 = paste0(carte_de_mut[,2], carte_de_mut[,3]),
                                                Res   = dictionary[Nuc12])
        carte_de_mut <- carte_de_mut[,c(1,8,4,5,6)]
        carte_de_mut[,6:15]=0
        colnames(carte_de_mut)=c("nb_mut", "muttype", "muttime", "pos", "Tip_label", "x_0", "y_0", "x_1", "y_1", "col_Transv", "color_lab", "real_pos", "num_carte", "filename", "col_GC")
        nb_row = nrow(carte_de_mut)
        if(nb_row > 1) ## if the file is full
        {
          
          nb_graph = 1
          debut= list()
          j=1
          
          ## Cette partie du code rajoute a la carte de mutation des colonnes comportant les coordonnees de l origine et
          # de l arrivee necessaire a la construction du diagramme ainsi que les couleurs et labels des mutations
          while (j <= nb_row)
          {
            while (carte_de_mut[j, 1] == -1 )
            {
              
              k = k+1
              debut[k]=j
              carte_de_mut[j, 6:12] = 0
              carte_de_mut[j, 13] = k
              carte_de_mut[j, 14] = filename
              carte_de_mut[j, 15] = 0
              
              if (nb_row>2 && j != nb_row)
              {
                j = j+1
              }
              else
              {
                break
              }
            }
            if (j == nb_row && carte_de_mut[j, 1] == -1)
            {
              break
            }
            if (carte_de_mut[j-1,"muttime"] == -1.0)
            {
              dist = 0+ carte_de_mut[j, "muttime"]
            }else{
              dist = (carte_de_mut[j, "muttime"]-carte_de_mut[j-1,"muttime"])
            }
            carte_de_mut[j, 6] = carte_de_mut[j-1, 8]
            carte_de_mut[j, 7] = carte_de_mut[j-1, 9]
            carte_de_mut[j, 8] = coord_pointB(carte_de_mut[j, 6],carte_de_mut[j, 7], type_de_mut(carte_de_mut[j, 2]),dist)[[1]]
            carte_de_mut[j, 9] = coord_pointB(carte_de_mut[j, 6],carte_de_mut[j, 7], type_de_mut(carte_de_mut[j, 2]), dist)[[2]]
            carte_de_mut[j, 10] = couleurTransVs(carte_de_mut[j, 2])
            carte_de_mut[j, 11] = couleurParMut(carte_de_mut[j, 2])
            carte_de_mut[j, 12] = carte_de_mut[j, 4]+1
            carte_de_mut[j, 13] = k
            carte_de_mut[j,14] = filename
            carte_de_mut[j, 15] = couleurGC(carte_de_mut[j, 2])
            
            j=j+1
            
          }
          carte_de_mut_tot = rbind(carte_de_mut_tot, carte_de_mut)
          
          
          # Pause for 0.1 seconds to simulate a long computation.(bar de progression)
          #          Sys.sleep(0.1)
        }
      }
      carte_de_mut_tot$muttime <- -(carte_de_mut_tot$muttime)
      #      })
      Mutation_map <- carte_de_mut_tot
    }
  })
  
  ## Pour recuperer un fichier de donnees sans les lignes -1  
  dataset <- reactive({
    subset(Mutation_map(), muttype != -1)
  })
  
  
  
  ## Pour fabriquer un fichier histogram avec le nombre de mutation et le nombre de fois ou ce nombre de mutations est calcule
  data_histo <- reactive({
    histoo(Mutation_map())
  })
  
  
  ## Pour extraire une partie du tableau de donnees correspondant a une position dans l'alignement  
  data_plot <- reactive({
    subset(dataset(), real_pos %in% input$site)
  })
  
  ## Pour extraire une partie du fichier histogram correspondant a une position dans l'alignement  
  data_histo_plot <- reactive({
    subset(data_histo(), real_pos %in% input$site)
  })

    
  ## Read Alignment file and build a matrix with one line for one species and one column for each position of the alignment
  dataSeq <- reactive({
    inFileSeq <- input$Align
    if(is.null(inFileSeq))
    {
      return(NULL)
    }else{
      tt <- read.alignment(inFileSeq$name, format = "fasta")
      gg <- as.matrix.alignment(tt)
    } 
    dataSeq <- melt(gg, value.name = "Residue", varnames = c("Name", "Position"))
  })
  
  # Permet de n'afficher ensuite qu une portion de l alignement afin que le graph soit lisible
  dataPlotAlign <- reactive({
    dataPlotAlign <- subset(dataSeq(), Position %in% seq(input$Begin, input$End, 1))
  })
  

  ## Read tree file  
  dataTree <- reactive({
    inFileTree <- input$fileTree
    if (is.null(inFileTree)){
      return(NULL)
    }else{
      dataTree = read.nexus(inFileTree$name, force.multi = FALSE)
   }
    })
  
  output$iteraation <- renderUI({
    
    if (length(unique(dataset()$num_carte)) > 0)
    {
      choice <-  unique(dataset()$num_carte)
      selectizeInput(
        'ChoixDeLiteration', "Select iteration(s)", choices = choice, selected = choice[1],
        multiple = TRUE
      )
    }
    else
    {
      return(NULL)
    }
  })
  
  
  # Affiche l'alignement de sequences
  output$Alignement <- renderPlot({
    plot <- ggplot(dataPlotAlign(), aes(x = Position, y = Name, fill = Residue, label = Residue)) + 
      geom_tile() + 
      geom_text() + 
      scale_x_continuous(breaks = function(x) { seq(0, max(x), by = 10)}) + 
      scale_fill_manual(values = DNA.palette) + theme_bw() + 
      theme(legend.position = "n", axis.title.y = element_blank(), axis.text.y = element_text(size = 6))+theme_bw() + theme(legend.position="none")
    plot
  }, height = 1000)  
  
  
#######################################################################################
#########################       PLOT DIAGRAM        ###################################
#######################################################################################  
  
  
  ## fonction reactive utiliser dans la sortie diagramme permettant a l'utilisateur de preciser le nombre de facet quil veut
  ## sur combien de lignes et combien de colonnes
  nrow_count <- reactive({input$n_row})
  ncol_count <- reactive({input$n_col})
  
  ## Find edge for diagram plot for all the data (dataset() or for a subset of the data (dataplot())) 
  min_l <- reactive({
    lim_min(dataset())
  })
  max_l <- reactive({ 
    lim_max(dataset())
  })
  mn_l <- reactive({
    lim_min(data_plot())
  })
  mx_l <- reactive({
    lim_max(data_plot())
  })
  
  ## Return the max number of page possible according to the number of data and the number of plot per page    
  output$page <- renderUI({
    if (input$Plot_option == "General_view")
    {
      if (length(unique(dataset()$real_pos)) > 0)
      {
        maxPage <- nbpage(as.numeric(nrow_count()), as.numeric(ncol_count()), length(unique(dataset()$real_pos)))
        numericInput("page","page",value= 1, min = 1, max = maxPage, step= 1 )
      }
      else
      {
        return(NULL)
      }
    }
  })

  ## Return the list of the DNA position for which mutation map is available      
  output$site <- renderUI({
    if (length(unique(dataset()$real_pos)) > 0)
    {
      selectInput("site", label = h3("site number"), list("site number" = unique(dataset()$real_pos)))
    }
    else
    {
      return(NULL)
    }
  })

  # Return legend plot for DNA data     
  output$Legend <- renderPlot({
     if (input$legend_type == "One color by mutation")
     {
       pp <- ggplot(d, aes(x = X_0, y = Y_0)) +
         geom_segment(aes(xend = X_1, yend = Y_1), linetype = "dotdash",
                      arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "both"),
                      size = 1, col = d$OneColorByMut) +
         theme_void() +
         theme(legend.position="none") +
         annotate(geom = "text", x = c(0.13,   0.16,   0.18,  0.19,  0.18,  0.16, 0.85,        1.05,      1.165,         1.165,        1.22,          1.045 ),
                  y = c(0.12,   0.1,    0.05,  0.01, -0.05, -0.1,  0.8485281,   0.6,      0.3105829,    -0.3105829,   -2.939152e-16,  -0.6 ),
                  label = c("A",    "A",    "A",   "C",    "C",  "T",     "C",      "G",       "T",          "T",          "G",            "G"), size = 3.5)+
         xlim(0,1.5)+ylim(-1.2,1.2)
       plot(pp)
      
     }else if(input$legend_type == "View Transition vs Transversion"){
       pp <- ggplot(d, aes(x = X_0, y = Y_0)) +
         geom_segment(aes(xend = X_1, yend = Y_1), linetype = "dotdash",
                      arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "both"),
                      size = 1, col = d$Transvs) +
         theme_void() +
         theme(legend.position="none") +
         annotate(geom = "text", x = c(0.13,   0.16,   0.18,  0.19,  0.18,  0.16, 0.85,        1.05,      1.165,         1.165,        1.22,          1.045 ),
                  y = c(0.12,   0.1,    0.05,  0.01, -0.05, -0.1,  0.8485281,   0.6,      0.3105829,    -0.3105829,   -2.939152e-16,  -0.6 ),
                  label = c("A",    "A",    "A",   "C",    "C",  "T",     "C",      "G",       "T",          "T",          "G",            "G"), size = 3.5)+
         xlim(0,1.5)+ylim(-1.2,1.2)
       plot(pp)
  
     }else{
       pp <- ggplot(d, aes(x = X_0, y = Y_0)) +
         geom_segment(aes(xend = X_1, yend = Y_1), linetype = "dotdash",
                      arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "both"),
                      size = 1, col = d$GC) +
         theme_void() +
         theme(legend.position="none") +
         annotate(geom = "text", x = c(0.13,   0.16,   0.18,  0.19,  0.18,  0.16, 0.85,        1.05,      1.165,         1.165,        1.22,          1.045 ),
                  y = c(0.12,   0.1,    0.05,  0.01, -0.05, -0.1,  0.8485281,   0.6,      0.3105829,    -0.3105829,   -2.939152e-16,  -0.6 ),
                  label = c("A",    "A",    "A",   "C",    "C",  "T",     "C",      "G",       "T",          "T",          "G",            "G"), size = 3.5)+
         xlim(0,1.5)+ylim(-1.2,1.2)
       plot(pp)
    }
    
  })
  
  
  
########################## MAIN DIAGRAM PLOT ##################################################  
  
  output$iteraation <- renderUI({
    if (input$Plot_option == "One_site")
    {
      if (length(unique(dataset()$num_carte)) > 0)
      {
        choice <-  unique(dataset()$num_carte)
         selectizeInput(
          'ChoixDeLiteration', "Select iteration(s)", choices = choice, selected = choice[1],
           multiple = TRUE
        )
      }
      else
     {
        return(NULL)
     }
    }else{
      return(NULL)
    }
  })
  
  output$siteAal <- renderUI({
    if (input$Plot_option == "One_site")
    {
    if (length(unique(dataset()$real_pos)) > 0)
    {
      selectInput("SiteDeLal", label = h5("Position in the alignment"), list("site number" = unique(dataset()$real_pos)))
    }
    else
    {
      return(NULL)
    }
    }else{
      return(NULL)
    }
  })
  
  dataTest <- reactive({
    dataT <- subset(dataset(), real_pos %in% input$SiteDeLal)
    dataTest <- subset(dataT, num_carte %in% input$ChoixDeLiteration)
  })
  
  # pour ne garder que les mutations presentes sur les clades d interets
  output$clade <- renderUI({
    
    if (length(unique(dataset()$Tip_label)) > 0)
    {
      choice <-  unique(dataset()$Tip_label)
      selectizeInput(
        'ChoixDuClade', "Select specie(s)", choices = choice, selected = choice[1],
        multiple = TRUE
      )
    }
    else
    {
      return(NULL)
    }
  })
  
  
  
  #### Jeu de donnees filtres sur les clades 
  DataClade <- reactive({
    DataClade <- subset(dataset(), Tip_label %in% input$ChoixDuClade)
    print(dataset())
    print("pkoi es tu vide?")
    print(DataClade)
  })
  
  DataClade_iterOne <- reactive({
    DataClade_iterOne <- subset(dataTest(), Tip_label %in% input$ChoixDuClade)
    print(DataClade_iterOne)
  })
  
  DataClade_allIter <- reactive({
    DataClade_allIter <- subset(data_plot(), Tip_label %in% input$ChoixDuClade)
    print(DataClade_allIter)
  })
    
  
    output$MutMap <- renderPlot({
      if(input$Data_option == "Complete_data")
      {
      if (input$legend_type == "One color by mutation")
      {    
         if (input$Plot_option == "General_view")
         {
          ## trace le graph des carte de mutations
          p<- ggplot(dataset(), aes(x = x_0, y = y_0)) +
              geom_segment(aes(xend = x_1, yend = y_1),
                          arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                          size = 1.2, alpha = 0.25, color = dataset()$color_lab) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
              theme_bw()+
              theme(legend.position="none")+
              labs(x = "", y = "")

        
          ## pour tracer les histogrammes
         s <- ggplot(data_histo(), aes(x=count))+
              geom_histogram(color = "black", fill = "white")
        
          ## selon les conditions d'echelle choisi par l'utilisateur
          ##
          if (input$scale_option == "free_scale")
          {
            p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)# page = curr_page())+
            s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
          }
          else if (input$scale_option == "coord_fixed")
          {
            p<- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page)# page = curr_page())+
            s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), page = input$page)
          }
          else if (input$scale_option == "ratio_1/1")
          {
           p<- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page) # page = curr_page())+
           p <- p+ xlim(min_l(), max_l())+ 
                   ylim(min_l(), max_l())
           s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
       
           s_grob = ggplotGrob(s)
           p + annotation_custom(s_grob)
         }
        
          ## Parametre d affichage des plots
          if (input$PlotHistogram == TRUE)
          {
             ## Si l'option d'affichage des histo des choisie
             ## Selon le nombre de graph a afficher sur une page
             if (nrow_count() == 1 & ncol_count() ==1)
             {
            # pour mettre l'histogramme en petit en bas de la carte de mutation
                 full(p,s) 
              }else{
          # pour afficher les cartes de mut et les histo en dessous
                 grid.arrange(p ,s)  
              }
           }else{  
            ## Si l'option d'affichage des histo n'est pas choisie
            p
           }
         }
      
        #### Si les sites sont affiches un a un
        else if (input$Plot_option =="One_site")
        {
          if (input$Iteration_Print == "All_iterations")
          {
             p<- ggplot(data_plot(), aes(x = x_0, y = y_0)) +
                 geom_segment(aes(xend = x_1, yend = y_1),
                            arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                            size = 1.2, alpha = 0.35, color = data_plot()$color_lab) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
                 theme_bw()+
                 theme(legend.position="none")+
                 coord_equal(ratio=1)+
                 labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(data_plot()$pos)+1))
              p <- p + xlim(mn_l(), mx_l())+
                       ylim(mn_l(), mx_l())
              p <- p + annotate("point",x =0, y = 0, size = length(unique(data_plot()$num_carte)), alpha = 0.1, color = "red")
              s <- ggplot(data_histo_plot(), aes(x = count))+
                   geom_histogram(color = "black", fill = "white")
          }else{
              p<- ggplot(dataTest(), aes(x = x_0, y = y_0)) +
                  geom_segment(aes(xend = x_1, yend = y_1),
                             arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                             size = 1.2, alpha = 0.25, color = dataTest()$color_lab) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
                  theme_bw()+
                  theme(legend.position="none")+
                  labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(dataTest()$pos)+1))
          }
           p
        }
      }
      else if (input$legend_type == "View Transition vs Transversion")
      {
        if (input$Plot_option == "General_view")
        {
          ## trace le graph des carte de mutations
          p<- ggplot(dataset(), aes(x = x_0, y = y_0)) +
            geom_segment(aes(xend = x_1, yend = y_1),
                         arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                         size = 1.2, alpha = 0.25, color = dataset()$col_Transv) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
            
            theme_bw()+
            theme(legend.position="none")+
            labs(x = "", y = "")
         print(subset(dataset(), real_pos==29))
   
          ## pour tracer les histogrammes
          s <- ggplot(data_histo(), aes(x=count))+
            geom_histogram(color = "black", fill = "white")
          
          ## selon les conditions d'echelle choisi par l'utilisateur
          ##
          if (input$scale_option == "free_scale")
          {
            p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', drop = FALSE, page = input$page)# page = curr_page())+
            s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
          }
          else if (input$scale_option == "coord_fixed")
          {
            p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page)# page = curr_page())+
            s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), page = input$page)
          }
          else if (input$scale_option == "ratio_1/1")
          {
            p<- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page) # page = curr_page())+
            p <- p+ xlim(min_l(), max_l())+ 
              ylim(min_l(), max_l())
            s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
            
            s_grob = ggplotGrob(s)
            p + annotation_custom(s_grob)
          }
          
          ## Parametre d affichage des plots
          if (input$PlotHistogram == TRUE)
          {
            ## Si l'option d'affichage des histo des choisie
            ## Selon le nombre de graph a afficher sur une page
            if (nrow_count() == 1 & ncol_count() ==1)
            {
              # pour mettre l'histogramme en petit en bas de la carte de mutation
              full(p,s) 
            }else{
              # pour afficher les cartes de mut et les histo en dessous
              grid.arrange(p ,s)  
            }
          }else{  
            ## Si l'option d'affichage des histo n'est pas choisie
           # print(dataset())
            p
          }
        }
        
        #### Si les sites sont affiches un a un
        else if (input$Plot_option =="One_site")
        {
          if (input$Iteration_Print == "All_iterations")
          {
            p<- ggplot(data_plot(), aes(x = x_0, y = y_0)) +
              geom_segment(aes(xend = x_1, yend = y_1),
                           arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                           size = 1.2, alpha = 0.35, color = data_plot()$col_Transv) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
              theme_bw()+
              theme(legend.position="none")+
              coord_equal(ratio=1)+
              labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(data_plot()$pos)+1))
            p <- p + xlim(mn_l(), mx_l())+
              ylim(mn_l(), mx_l())
            p <- p + annotate("point",x =0, y = 0, size = length(unique(data_plot()$num_carte)), alpha = 0.1, color = "red")
            s <- ggplot(data_histo_plot(), aes(x = count))+
              geom_histogram(color = "black", fill = "white")
          }else{
            p<- ggplot(dataTest(), aes(x = x_0, y = y_0)) +
              geom_segment(aes(xend = x_1, yend = y_1),
                           arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                           size = 1.2, alpha = 0.25, color = dataTest()$col_Transv) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
              theme_bw()+
              theme(legend.position="none")+
              labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(dataTest()$pos)+1))
          }
          p
        }
      } else if (input$legend_type == "View GC")
      {
        if (input$Plot_option == "General_view")
        {
          ## trace le graph des carte de mutations
          p<- ggplot(dataset(), aes(x = x_0, y = y_0)) +
            geom_segment(aes(xend = x_1, yend = y_1),
                         arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                         size = 1.2, alpha = 0.25, color = dataset()$col_GC) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
            
            theme_bw()+
            theme(legend.position="none")+
            labs(x = "", y = "")
          print(subset(dataset(), real_pos==29))

          ## pour tracer les histogrammes
          s <- ggplot(data_histo(), aes(x=count))+
            geom_histogram(color = "black", fill = "white")

          ## selon les conditions d'echelle choisi par l'utilisateur
          ##
          if (input$scale_option == "free_scale")
          {
            p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', drop = FALSE, page = input$page)# page = curr_page())+
            s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
          }
          else if (input$scale_option == "coord_fixed")
          {
            p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page)# page = curr_page())+
            s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), page = input$page)
          }
          else if (input$scale_option == "ratio_1/1")
          {
            p<- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page) # page = curr_page())+
            p <- p+ xlim(min_l(), max_l())+ 
              ylim(min_l(), max_l())
            s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
            
            s_grob = ggplotGrob(s)
            p + annotation_custom(s_grob)
          }
          
          ## Parametre d affichage des plots
          if (input$PlotHistogram == TRUE)
          {
            ## Si l'option d'affichage des histo des choisie
            ## Selon le nombre de graph a afficher sur une page
            if (nrow_count() == 1 & ncol_count() ==1)
            {
              # pour mettre l'histogramme en petit en bas de la carte de mutation
              full(p,s) 
            }else{
              # pour afficher les cartes de mut et les histo en dessous
              grid.arrange(p ,s)  
            }
          }else{  
            ## Si l'option d'affichage des histo n'est pas choisie
            # print(dataset())
            p
          }
        }
        
        #### Si les sites sont affiches un a un
        else (input$Plot_option =="One_site")
        {
          if (input$Iteration_Print == "All_iterations")
          {
            p<- ggplot(data_plot(), aes(x = x_0, y = y_0)) +
              geom_segment(aes(xend = x_1, yend = y_1),
                           arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                           size = 1.2, alpha = 0.35, color = data_plot()$col_GC) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
              theme_bw()+
              theme(legend.position="none")+
              coord_equal(ratio=1)+
              labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(data_plot()$pos)+1))
            p <- p + xlim(mn_l(), mx_l())+
              ylim(mn_l(), mx_l())
            p <- p + annotate("point",x =0, y = 0, size = length(unique(data_plot()$num_carte)), alpha = 0.1, color = "red")
            s <- ggplot(data_histo_plot(), aes(x = count))+
              geom_histogram(color = "black", fill = "white")
          }else{
            p<- ggplot(dataTest(), aes(x = x_0, y = y_0)) +
              geom_segment(aes(xend = x_1, yend = y_1),
                           arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                           size = 1.2, alpha = 0.25, color = dataTest()$col_GC) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
              theme_bw()+
              theme(legend.position="none")+
              labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(dataTest()$pos)+1))
          }
          p
        }
        
    #   ### S il ny a pas de fichiers
       } 
      }else if (input$Data_option == "choose_individuals"){
      
        if (input$legend_type == "One color by mutation")
        {    
          if (input$Plot_option == "General_view")
          {
            ## trace le graph des carte de mutations
            p<- ggplot(DataClade(), aes(x = x_0, y = y_0)) +
              geom_segment(aes(xend = x_1, yend = y_1),
                           arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                           size = 1.2, alpha = 0.25, color = DataClade()$color_lab) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
              theme_bw()+
              theme(legend.position="none")+
              labs(x = "", y = "")
            
            
            ## pour tracer les histogrammes
            s <- ggplot(data_histo(), aes(x=count))+
              geom_histogram(color = "black", fill = "white")
            
            ## selon les conditions d'echelle choisi par l'utilisateur
            ##
            if (input$scale_option == "free_scale")
            {
              p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)# page = curr_page())+
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
            }
            else if (input$scale_option == "coord_fixed")
            {
              p<- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page)# page = curr_page())+
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), page = input$page)
            }
            else if (input$scale_option == "ratio_1/1")
            {
              p<- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page) # page = curr_page())+
              p <- p+ xlim(min_l(), max_l())+ 
                ylim(min_l(), max_l())
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
              
              s_grob = ggplotGrob(s)
              p + annotation_custom(s_grob)
            }
            
            ## Parametre d affichage des plots
            if (input$PlotHistogram == TRUE)
            {
              ## Si l'option d'affichage des histo des choisie
              ## Selon le nombre de graph a afficher sur une page
              if (nrow_count() == 1 & ncol_count() ==1)
              {
                # pour mettre l'histogramme en petit en bas de la carte de mutation
                full(p,s) 
              }else{
                # pour afficher les cartes de mut et les histo en dessous
                grid.arrange(p ,s)  
              }
            }else{  
              ## Si l'option d'affichage des histo n'est pas choisie
              p
            }
          }
          
          #### Si les sites sont affiches un a un
          else if (input$Plot_option =="One_site")
          {
            if (input$Iteration_Print == "All_iterations")
            {
              p<- ggplot(DataClade_allIter(), aes(x = x_0, y = y_0)) +
                geom_segment(aes(xend = x_1, yend = y_1),
                             arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                             size = 1.2, alpha = 0.35, color = DataClade_allIter()$color_lab) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
                theme_bw()+
                theme(legend.position="none")+
                coord_equal(ratio=1)+
                labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(DataClade_allIter()$pos)+1))
              p <- p + xlim(mn_l(), mx_l())+
                ylim(mn_l(), mx_l())
              p <- p + annotate("point",x =0, y = 0, size = length(unique(DataClade_allIter()$num_carte)), alpha = 0.1, color = "red")
              s <- ggplot(data_histo_plot(), aes(x = count))+
                geom_histogram(color = "black", fill = "white")
            }else{
              p<- ggplot(DataClade_iterOne(), aes(x = x_0, y = y_0)) +
                geom_segment(aes(xend = x_1, yend = y_1),
                             arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                             size = 1.2, alpha = 0.25, color = DataClade_iterOne()$color_lab) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
                theme_bw()+
                theme(legend.position="none")+
                labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(DataClade_iterOne()$pos)+1))
            }
            p
          }
        }
        else if (input$legend_type == "View Transition vs Transversion")
        {
          if (input$Plot_option == "General_view")
          {
            ## trace le graph des carte de mutations
            p<- ggplot(DataClade(), aes(x = x_0, y = y_0)) +
              geom_segment(aes(xend = x_1, yend = y_1),
                           arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                           size = 1.2, alpha = 0.25, color = DataClade()$col_Transv) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
              
              theme_bw()+
              theme(legend.position="none")+
              labs(x = "", y = "")
            print(subset(DataClade(), real_pos==29))
            
            ## pour tracer les histogrammes
            s <- ggplot(data_histo(), aes(x=count))+
              geom_histogram(color = "black", fill = "white")
            
            ## selon les conditions d'echelle choisi par l'utilisateur
            ##
            if (input$scale_option == "free_scale")
            {
              p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', drop = FALSE, page = input$page)# page = curr_page())+
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
            }
            else if (input$scale_option == "coord_fixed")
            {
              p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page)# page = curr_page())+
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), page = input$page)
            }
            else if (input$scale_option == "ratio_1/1")
            {
              p<- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page) # page = curr_page())+
              p <- p+ xlim(min_l(), max_l())+ 
                ylim(min_l(), max_l())
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
              
              s_grob = ggplotGrob(s)
              p + annotation_custom(s_grob)
            }
            
            ## Parametre d affichage des plots
            if (input$PlotHistogram == TRUE)
            {
              ## Si l'option d'affichage des histo des choisie
              ## Selon le nombre de graph a afficher sur une page
              if (nrow_count() == 1 & ncol_count() ==1)
              {
                # pour mettre l'histogramme en petit en bas de la carte de mutation
                full(p,s) 
              }else{
                # pour afficher les cartes de mut et les histo en dessous
                grid.arrange(p ,s)  
              }
            }else{  
              ## Si l'option d'affichage des histo n'est pas choisie
              # print(dataset())
              p
            }
          }
          
          #### Si les sites sont affiches un a un
          else if (input$Plot_option =="One_site")
          {
            if (input$Iteration_Print == "All_iterations")
            {
              p<- ggplot(DataClade_allIter(), aes(x = x_0, y = y_0)) +
                geom_segment(aes(xend = x_1, yend = y_1),
                             arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                             size = 1.2, alpha = 0.35, color = DataClade_allIter()$col_Transv) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
                theme_bw()+
                theme(legend.position="none")+
                coord_equal(ratio=1)+
                labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(DataClade_allIter()$pos)+1))
              p <- p + xlim(mn_l(), mx_l())+
                ylim(mn_l(), mx_l())
              p <- p + annotate("point",x =0, y = 0, size = length(unique(DataClade_allIter()$num_carte)), alpha = 0.1, color = "red")
              s <- ggplot(data_histo_plot(), aes(x = count))+
                geom_histogram(color = "black", fill = "white")
            }else{
              p<- ggplot(DataClade_iterOne(), aes(x = x_0, y = y_0)) +
                geom_segment(aes(xend = x_1, yend = y_1),
                             arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                             size = 1.2, alpha = 0.25, color = DataClade_iterOne()$col_Transv) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
                theme_bw()+
                theme(legend.position="none")+
                labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(DataClade_iterOne()$pos)+1))
            }
            
            p
          }
        } else if (input$legend_type == "View GC")
        {
          if (input$Plot_option == "General_view")
          {
            ## trace le graph des carte de mutations
            p<- ggplot(DataClade(), aes(x = x_0, y = y_0)) +
              geom_segment(aes(xend = x_1, yend = y_1),
                           arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                           size = 1.2, alpha = 0.25, color = DataClade()$col_GC) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
              
              theme_bw()+
              theme(legend.position="none")+
              labs(x = "", y = "")
            print(subset(DataClade(), real_pos==29))
            
            ## pour tracer les histogrammes
            s <- ggplot(data_histo(), aes(x=count))+
              geom_histogram(color = "black", fill = "white")
            
            ## selon les conditions d'echelle choisi par l'utilisateur
            ##
            if (input$scale_option == "free_scale")
            {
              p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', drop = FALSE, page = input$page)# page = curr_page())+
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
            }
            else if (input$scale_option == "coord_fixed")
            {
              p <- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page)# page = curr_page())+
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), page = input$page)
            }
            else if (input$scale_option == "ratio_1/1")
            {
              p<- p + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'fixed', page = input$page) # page = curr_page())+
              p <- p+ xlim(min_l(), max_l())+ 
                ylim(min_l(), max_l())
              s <- s + facet_wrap_paginate( ~real_pos, nrow = nrow_count(), ncol = ncol_count(), scales = 'free', page = input$page)
              
              s_grob = ggplotGrob(s)
              p + annotation_custom(s_grob)
            }
            
            ## Parametre d affichage des plots
            if (input$PlotHistogram == TRUE)
            {
              ## Si l'option d'affichage des histo des choisie
              ## Selon le nombre de graph a afficher sur une page
              if (nrow_count() == 1 & ncol_count() ==1)
              {
                # pour mettre l'histogramme en petit en bas de la carte de mutation
                full(p,s) 
              }else{
                # pour afficher les cartes de mut et les histo en dessous
                grid.arrange(p ,s)  
              }
            }else{  
              ## Si l'option d'affichage des histo n'est pas choisie
              # print(dataset())
              p
            }
          }
          
          #### Si les sites sont affiches un a un
          else if (input$Plot_option =="One_site")
          {
            if (input$Iteration_Print == "All_iterations")
            {
              p<- ggplot(DataClade_allIter(), aes(x = x_0, y = y_0)) +
                geom_segment(aes(xend = x_1, yend = y_1),
                             arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                             size = 1.2, alpha = 0.35, color = DataClade_allIter()$col_GC) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
                theme_bw()+
                theme(legend.position="none")+
                coord_equal(ratio=1)+
                labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(DataClade_allIter()$pos)+1))
              p <- p + xlim(mn_l(), mx_l())+
                ylim(mn_l(), mx_l())
              p <- p + annotate("point",x =0, y = 0, size = length(unique(DataClade_allIter()$num_carte)), alpha = 0.1, color = "red")
              s <- ggplot(data_histo_plot(), aes(x = count))+
                geom_histogram(color = "black", fill = "white")
            }else{
              p<- ggplot(DataClade_iterOne(), aes(x = x_0, y = y_0)) +
                geom_segment(aes(xend = x_1, yend = y_1),
                             arrow = arrow(angle = 30,length=unit(0.10, "inches"), ends = "first"),
                             size = 1.2, alpha = 0.25, color = DataClade_iterOne()$col_GC) + ## on a rajoute alpha ici = parametre de transparence (0 transparent 1 opaque)
                theme_bw()+
                theme(legend.position="none")+
                labs(x = "", y = "", title = paste0("Mutation map for the site ", unique(DataClade_iterOne()$pos)+1))
            }
            p
          }
          
          #   ### S il ny a pas de fichiers
        }
        
        
        
      }else{
      return(NULL)
    }
    })
    
 
  #######################################################################################
  #########################       PLOT TREE        ######################################
  #######################################################################################  
  
  output$site_al <- renderUI({
    if (length(unique(dataset()$real_pos)) > 0)
    {
      selectInput("site_al", label = h3("Alignment position"), list("site number" = unique(dataset()$real_pos)))
    }
    else
    {
      return(NULL)
    }
  })
  
  
  It_number <- reactive({
    max(Mutation_map()$num_carte)
  })
  
  Burnin <- reactive({
    (It_number() * 10)/100
  })
  
  

  ## Pour extraire une partie du tableau de donnees correspondant a une position dans l'alignement  
  dataset_spe <- reactive({
    ds <- subset(dataset(), num_carte >= Burnin() )
    dataset_spe <- subset(ds, real_pos %in% input$site_al)
  })
  
  output$Iterations <- renderUI({
    if (length(unique(dataset_spe()$num_carte)) > 0)
    {
       choice <-  unique(dataset_spe()$num_carte)
       selectizeInput(
        'Iterations', "Select Iteration(s)", choices = choice, selected = choice[1],
        multiple = TRUE
      )
    }
    else
    {
      return(NULL)
    }
  })
  
  Iter <- reactive({
    Iter <- input$Iterations
  })
  
  dataSeq_spe <- reactive({
    subset(dataSeq(), Position %in% input$site_al)
  })
  

    ########################## MAIN TREE PLOT ##################################################  
    
    pTree <- reactive({
        arbres <- dataTree()[c(Iter())]
        p <- ggtree(arbres, mrsd = "0000-1-1", alpha = 0.1, ladderize = FALSE)
        p <- p + geom_tiplab() + geom_text2(aes(subset=!isTip, label = node), hjust = 1.3, vjust = -0.8, size = 2) 
        pTree <- ggplot_build(p)
    })
   
 
    #####  constrution du tableau de donnees permettant de construire l arbre
    
    # La fonction de R donne 3 tableaux , 1 pour les branches horizontales, 1 pour les branches vertiales et
    # 1 pour les labels des feuilles
    # Je concatene et ordonne a ma facons les branches horizontales et verticales en un tableau 
  
  # Les braches horizontales
  # Les branches horizontales
  HorizontalEgdeDF <- reactive({  
    
    noeudPH <- pTree()$data[[1]]$parent
    noeudEH <- pTree()$data[[1]]$node
    xEndH <- pTree()$data[[1]]$xend
    yEndH <- pTree()$data[[1]]$yend
    xBegH <- pTree()$data[[1]]$x
    yBegH <- pTree()$data[[1]]$y
    iterationH <- pTree()$data[[1]]$group
    HorizontalEgdeDF <- data.frame(Parent = noeudPH, Enfant = noeudEH, x = xBegH, xEnd = xEndH, 
                                   y = yBegH, yEnd = yEndH, iteration = iterationH)
    HorizontalEgdeDF$iteration <- recodeVar(HorizontalEgdeDF$iteration, unique(HorizontalEgdeDF$iteration), c(Iter()) )
   
    print(HorizontalEgdeDF)
  })
  
  # Les branches verticales
  VerticalEdgeDF <- reactive({ 

    noeudPV <- pTree()$data[[2]]$parent
    noeudEV <- pTree()$data[[2]]$node
    xBegV <- pTree()$data[[2]]$x
    xEndV <- pTree()$data[[2]]$xend
    yBegV <- pTree()$data[[2]]$y
    yEndV <- pTree()$data[[2]]$yend
    iterationV <- pTree()$data[[1]]$group
    VerticalEdgeDF <- data.frame(Parent = noeudPV, Enfant = noeudEV, x = xBegV, xEnd = xEndV, y = yBegV, yEnd = yEndV, iteration = iterationV)
    VerticalEdgeDF$iteration <- recodeVar(VerticalEdgeDF$iteration, unique(VerticalEdgeDF$iteration), c(Iter()) )
    print(VerticalEdgeDF)
  })
    
    
    # Concatenation des branches horizontales + verticales
    tabDataTree <- reactive({
      tabDataTree <- rbind.data.frame(HorizontalEgdeDF(), VerticalEdgeDF())  
    })
    

  tabDataTipLab <- reactive({
    tabDataTipLab <- data.frame(x = pTree()$data[[3]]$x, y = pTree()$data[[3]]$y, Name = pTree()$data[[3]]$label) 
   })
  
  
  DataSeqTip <- reactive({
    dataSeq2 <- dataSeq_spe()
    dataSeq2<- dplyr::left_join(dataSeq2, tabDataTipLab()[,2:3], by = "Name") 
    DataSeqTip <- dataSeq2
  })
 
  tabResumeNode <- reactive({
    ResumeNode<-HorizontalEgdeDF()
    
    ResumeNode<-dplyr::full_join(ResumeNode, tabDataTipLab()[,-1], by = "y" )
    ResumeNode$Name <- as.character(ResumeNode$Name)
    ResumeNode[is.na(ResumeNode[,"Name"]), "Name"] <- "NA"
    
    vectLab = which(ResumeNode$Name == "NA")
    while (length(vectLab) > 0 )
    {
      for (i in unique(ResumeNode$Parent))
      {
        subb = subset(ResumeNode, ResumeNode$Parent == i| ResumeNode$Enfant == i )
        
        for (iter in unique(subb$iteration))
        {
          suubb = subset(subb, iteration == iter)
          q = which(suubb$Name == "NA")
          r = which(suubb$Name != "NA")
          if (length(q) == 1)
          {
            seqname = paste(c("", suubb$Name[r]), collapse = " ")
            suubb$Name[q] = seqname
          }
          ResumeNode[rownames(suubb),] = suubb
        }
      }
      vectLab = which((ResumeNode$Name) == "NA")
    }
    tabResumeNode <- ResumeNode
  })
  
  
  TabdataMut <- reactive({
    TabMut <- dataset_spe()
    TabMut$ypos = "NA"

    for ( l in 1:nrow(TabMut))
    {
      nmFeuille = TabMut[l, "Tip_label"]
      xpos = TabMut[l, "muttime"]
      iter = TabMut[l, "num_carte"]
      tmp0 = subset(tabResumeNode(), tabResumeNode()$iteration == iter)
      tmp = subset(tmp0, str_detect(tmp0$Name, nmFeuille)==TRUE)
      
      for (li in 1:nrow(tmp))
      {
        if (in.interval.lo(xpos, tmp[li, "x"],tmp[li, "xEnd"] ) == TRUE)
        {
          TabMut[l, "ypos"] = tmp[li, "y"]
        }
      }
    }
    TabdataMut <- TabMut
  })
  
 
  # Tout ça permet de :
  # - parcourir le tableau d'arbre
  # - parcourir la carte de mutation
  # pour une meme position et une meme iteration
  # - s il y a une mutation sur une branche de l'arbre
  # - couper la branche à l'endroit de la mutation = faire d'une ligne de tableau 2 lignes 

  TabPresqueFinal <- reactive({
    NewTab <- data.frame()
      Carte = TabdataMut()
   
      for (It in (unique(tabDataTree()$iteration)))
      {
        Tab = subset(tabDataTree(), tabDataTree()$iteration == It )
   
        Tab$pos <- Carte$real_pos[1]
        Tab$col <- "black"
        Carte2 = subset(Carte, Carte$num_carte == It)
        Tab <- Tab %>% mutate(id = row_number())
        nr <- nrow(Tab)
        for (i in 1:nr)
        {
         TabTmp <- data.frame(Parent = numeric(), Enfant = numeric(), x = numeric(), xEnd = numeric(),
                             y = numeric(), yEnd = numeric(), iteration = numeric(), pos = numeric(), col = character(), stringsAsFactors = FALSE)
          if (nrow(Carte2)>0)
          {
            for (j in 1:nrow(Carte2))
            {
              if (Tab$y[i] == Tab$yEnd[i] && Tab$y[i] == Carte2$ypos[j] && Carte2$muttime[j] < Tab$xEnd[i] && Carte2$muttime[j] >= Tab$x[i])
              {
                TabTmp[1, "Parent"] <- Tab$Parent[i]
                TabTmp[1, "Enfant"] <- Tab$Enfant[i]
                TabTmp[1, "x"] <- Carte2$muttime[j]
                TabTmp[1,"xEnd"] <- Tab$xEnd[i]
                TabTmp[1, "y"] <- Tab$y[i]
                TabTmp[1, "yEnd"] <- Tab$yEnd[i]
                TabTmp[1,"iteration"] <- Tab$iteration[i]
                TabTmp[1,"pos"] <- Tab$pos[i]
                TabTmp[1,"col"] <- couleurDescendant(Carte2$muttype[j])
                TabTmp[1, "id"] <- Tab$id[i]
                
                TabTmp[2,"Parent"] <- Tab$Parent[i]
                TabTmp[2,"Enfant"]<- Tab$Enfant[i]
                TabTmp[2, "x"] <- Tab$x[i]
                TabTmp[2,"xEnd"] <- Carte2$muttime[j]
                TabTmp[2,"y"] <- Tab$y[i]
                TabTmp[2,"yEnd"] <- Tab$yEnd[i]
                TabTmp[2,"iteration"] <- Tab$iteration[i]
                TabTmp[2,"pos"] <- Tab$pos[i]
                TabTmp[2,"col"] <- Tab$col[i]
                TabTmp[2, "id"] <- nr + 1
              
                MonArbre <- dataTree()[[It]]
                  
                for (gD in getDescendants(MonArbre, Tab$Parent[i]))
                {
                  po = which(Tab$Parent == gD)
                  if (length(po)>0)
                  {
                    Tab$col[po] = couleurDescendant(Carte2$muttype[j])
                  }
                }
                Tab <- Tab[-i,]
                Tab <- rbind(Tab, TabTmp)
                Tab <- Tab[order(Tab[,"id"], decreasing = FALSE),]
              }else{
                next
              }
             }
            Tab$col[Tab$col == "black"] <- couleurRacine(Carte2$muttype[which(Carte2$muttime == min(Carte2$muttime))])
          }
        }
        nr2 <- nrow(Tab)
        for (i in nr:nr2)
        {
          TabTmp <- data.frame(Parent = numeric(), Enfant = numeric(), x = numeric(), xEnd = numeric(),
                               y = numeric(), yEnd = numeric(), iteration = numeric(), pos = numeric(), col = character(), stringsAsFactors = FALSE)
          if (nrow(Carte2)>0)
          {
            for (j in 1:nrow(Carte2))
            {
              if (Tab$y[i] == Tab$yEnd[i] && Tab$y[i] == Carte2$ypos[j] && Carte2$muttime[j] < Tab$xEnd[i] && Carte2$muttime[j] >= Tab$x[i])
              {
                TabTmp[1, "Parent"] <- Tab$Parent[i]
                TabTmp[1, "Enfant"] <- Tab$Enfant[i]
                TabTmp[1, "x"] <- Carte2$muttime[j]
                TabTmp[1,"xEnd"] <- Tab$xEnd[i]
                TabTmp[1, "y"] <- Tab$y[i]
                TabTmp[1, "yEnd"] <- Tab$yEnd[i]
                TabTmp[1,"iteration"] <- Tab$iteration[i]
                TabTmp[1,"pos"] <- Tab$pos[i]
                TabTmp[1,"col"] <- couleurDescendant(Carte2$muttype[j])
                TabTmp[1, "id"] <- Tab$id[i]
                
                TabTmp[2,"Parent"] <- Tab$Parent[i]
                TabTmp[2,"Enfant"]<- Tab$Enfant[i]
                TabTmp[2, "x"] <- Tab$x[i]
                TabTmp[2,"xEnd"] <- Carte2$muttime[j]
                TabTmp[2,"y"] <- Tab$y[i]
                TabTmp[2,"yEnd"] <- Tab$yEnd[i]
                TabTmp[2,"iteration"] <- Tab$iteration[i]
                TabTmp[2,"pos"] <- Tab$pos[i]
                TabTmp[2,"col"] <- Tab$col[i]
                TabTmp[2, "id"] <- nr + 1
                
                MonArbre <- dataTree()[[It]]
                
                for (gD in getDescendants(MonArbre, Tab$Parent[i]))
                {
                  po = which(Tab$Parent == gD)
                  if (length(po)>0)
                  {
                    Tab$col[po] = couleurDescendant(Carte2$muttype[j])
                  }
                }
                Tab <- Tab[-i,]
                Tab <- rbind(Tab, TabTmp)
                Tab <- Tab[order(Tab[,"id"], decreasing = FALSE),]
              }else{
                next
              }
            }
            Tab$col[Tab$col == "black"] <- couleurRacine(Carte2$muttype[which(Carte2$muttime == min(Carte2$muttime))])
          }
        }
        NewTab <- rbind(NewTab, Tab)
      }

    TabPresqueFinal <- NewTab
  })
  

  
  ##### Pour avoir les borne de l'axe de l'arbre
  xAxis <- reactive({
    seq(from = 0, to = min(TabPresqueFinal()$xEnd), -50)
  })
  
  ## Pour appliquer une transparence croissante selon le nombre d'itérations à afficher
  transparence <- reactive({
    1/length(Iter())
  })
  
  #### PLOT DE L'ARBRE
## Code pour tracer l'arbre
  TreeGraph <- reactive({
     if (is.null(dataTree()))
      {
        paste("Please upload Nexus file")
      }else{
        plotTree <- ggplot(TabPresqueFinal(), aes(x = x, y = y, color = TabPresqueFinal()$col)) +
          geom_segment(aes(xend = xEnd, yend = yEnd),
                       size = 0.5, color = TabPresqueFinal()$col, alpha = transparence())+ #color = NewTab$col,
           annotate(geom = "text", x = tabDataTipLab()$x, 
                   y = tabDataTipLab()$y, hjust = 0,
                   label = tabDataTipLab()$Name, fontface = 1)+
          theme_gray()+
          theme(panel.background = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
                axis.title.y = element_blank(), legend.position = "none")+
          scale_x_continuous("Time (Ma)",  limits = c(NA, 0.25))
      }
 })
# code pour afficher l alignement du site correspondant 
  AlignGraph <- reactive({
    plot <- ggplot(dataSeq_spe(), aes(x = Position, y = Name, fill = Residue, label = Residue)) + 
      geom_tile() + 
      geom_text() + 

      scale_fill_manual(values = DNA.palette) + theme_bw() + 
      theme(legend.position="none", axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), axis.text.y = element_text(colour="grey20",size=8))
  })
# code final pour l'affichage des plot  
  output$Tree <- renderPlot({
         grid.arrange(TreeGraph(), AlignGraph(), ncol = 2, nrow = 1, widths = c(6, 1))
  })
   
 #####################################################################
 ###########     FIN SERVER     #####################################
    
}



    
   
########################################################################################################################
########################################################################################################################
######################################        POUR LANCER L'APPLI         ##############################################
########################################################################################################################
########################################################################################################################
  
shinyApp(ui = ui, server = server, options = list(height = 1200))  

