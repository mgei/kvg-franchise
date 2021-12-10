library(tidyverse)
library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


split_string <- function(str, sep = ",") {
  strsplit(str, sep)
  
  return(strsplit(str, sep)[[1]])
}


jscode <- "
shinyjs.toggleBox = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
};
shinyjs.expandBox = function(boxid) {
if (document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}};
shinyjs.collapseBox = function(boxid) {
if (!document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}}"