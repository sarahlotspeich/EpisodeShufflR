#' @title Episode shuffler
#'
#' @description This package randomly samples episodes from TV shows. Select shows are included in the package, but the user can also feed their own episode list.
#'
#' @param show
#' @param last_season_happened
#' @param episode_list
#'
#' @return random_episode_output
#'
#' @examples EpisodeShufflR(show = "Gilmore Girls", last_season_happened = FALSE)
#'
#' @export
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

EpisodeShufflR <- function(show, last_season_happened = TRUE, episode_list = NULL)
{
  if(is.null(episode_list))
  {
    if(show == "Gilmore Girls")
    {
      episode_list <- read.csv("https://raw.githubusercontent.com/sarahlotspeich/episode_shufflR/master/Episode_Lists/GilmoreGirls_Episode_List_Wikipedia.csv?token=AFA4HNLJN3MCDLY2TKBCKN26FW7L4", header=TRUE, stringsAsFactors = FALSE)
    }
    if(show == "Friends")
    {
      episode_list <- read.csv("https://raw.githubusercontent.com/sarahlotspeich/episode_shufflR/master/Episode_Lists/Friends_Episode_List_Wikipedia.csv?token=AFA4HNPCNQZT4EU44QQM75K6FW7OO", header=TRUE, stringsAsFactors = FALSE)
    }
    if(!last_season_happened)
    {
      episode_list <- subset(episode_list, !Last_Season)
    }
  }
  random_episode_num <- sample(x = 1:nrow(episode_list), size = 1)
  random_episode_output <- paste0(show, " Season ", episode_list$Season[random_episode_num], " Episode ", episode_list$Episode[random_episode_num], ": ", episode_list$Title[random_episode_num])
  return(random_episode_output)
}
