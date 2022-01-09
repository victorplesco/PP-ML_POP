require(streamR)
require(ROAuth)
require(rjson)
require(bit64)
require(httr)
require(devtools)
require(twitteR)
require(RCurl)
require(rtweet)
require(readr)
require(tidyverse)
require(reshape2)
require(ggridges)
require(lubridate)
require(maps)
require(quanteda)
require(feather)
require(rworldmap)
require(imputeTS)
require(stringr)
require(RJSONIO)
require(rworldxtra)
require(fitdistrplus)
require(logspline)
require(MASS)
require(dplyr)
require(gamlss)
require(gamlss.dist)
require(gamlss.add)
require(GeneralizedHyperbolic)
require(statmod)
require(poweRlaw)
require(Hmisc)
require(caret)
require(ggalt)

options(scipen = 999)
set.seed(123)

######################################################################################################################################################################################################################################################################################################################################
## TOKEN ACTIVATION ##################################################################################################################################################################################################################################################################################################################
######################################################################################################################################################################################################################################################################################################################################

{ # {} START HERE;
  
k <- c(1:5) # DEFINE THE APP YOU WANT TO USE:
for(x in k){
  
AppNum <- x # ML-POP = 1; ML-POP-BIS = 2; ML-POP-TRIS = 3; ML-POP-QUADRO = 4; ML-POP-QUINTET = 5;

  if(AppNum == 1 | AppNum == 2 | AppNum == 3 | AppNum == 4 | AppNum == 5){
    if(AppNum == 1)
    {
       # CONSUMER API KEYS (1)
       consumerKey    <- "INSERT KEY"
       consumerSecret <- "INSERT KEY"

       app_name <- "ML-POP"
       token    <- create_token(app             = app_name,
                                consumer_key    = consumerKey,
                                consumer_secret = consumerSecret)
    }

    if(AppNum == 2)
    {
       # CONSUMER API KEYS (2)
       consumerKey    <- "INSERT KEY"
       consumerSecret <- "INSERT KEY"

       app_name <- "ML-POP-BIS"
       tokenBIS    <- create_token(app             = app_name,
                                   consumer_key    = consumerKey,
                                   consumer_secret = consumerSecret)
    }
    if(AppNum == 3)
    { 
       # CONSUMER API KEYS (3)
       consumerKey    <- "INSERT KEY"
       consumerSecret <- "INSERT KEY"

       app_name <- "ML-POP-TRIS"
       tokenTRIS    <- create_token(app             = app_name,
                                    consumer_key    = consumerKey,
                                    consumer_secret = consumerSecret)
    }

    if(AppNum == 4)
    { 
       # CONSUMER API KEYS (4)
       consumerKey    <- "INSERT KEY"
       consumerSecret <- "INSERT KEY"
  
       app_name <- "ML-POP-QUADRO"
       tokenQUADRO    <- create_token(app             = app_name,
                                      consumer_key    = consumerKey,
                                      consumer_secret = consumerSecret)
    }
  
    if(AppNum == 5)
    { 
      # CONSUMER API KEYS (5)
      consumerKey    <- "INSERT KEY"
      consumerSecret <- "INSERT KEY"
    
      app_name <- "ML-POP-QUINTET"
      tokenQUINTET    <- create_token(app             = app_name,
                                      consumer_key    = consumerKey,
                                      consumer_secret = consumerSecret)
    }
  }
}

} # {} END HERE;

######################################################################################################################################################################################################################################################################################################################################
## DOWNLOADING TWEET - REST API ######################################################################################################################################################################################################################################################################################################
######################################################################################################################################################################################################################################################################################################################################

# TAGS: https://hashtagify.me/hashtag/tbt - NO MORE THAN TEN TAGS;
#                 
# -ITALIANO: "vino OR cucina OR ricette OR cena OR cibo OR colazione OR aperitivo OR pranzo OR fame OR birra"
# -INGLESE : "food OR coffee OR delicious OR hungry OR breakfast OR recipe OR dinner OR beer OR lunch OR cooking"
#

# LOCATIONS: https://www.gpsvisualizer.com/calculators & https://thegpscoordinates.net
#
# - USA   : 39.11141,-94.6275,2500km
# - EUROPA: 50.7663,  15.0543,2500km
#

rtweet_rest <- unique(search_tweets(q                = c("food OR coffee OR delicious OR hungry OR breakfast OR recipe OR dinner OR beer OR lunch OR cooking"), # "trump OR president OR potus";
                                    n                = 300000, 
                                    type             = "mixed", # "recent", "mixed" or "popular";
                                    include_rts      = FALSE,
                                    geocode          = "50.7663,15.0543,2500km", # "latitude,longitude,radius(mi | km)" - "37.78,-122.40,1mi";
                                    parse            = TRUE,
                                    token            = token,
                                    retryonratelimit = TRUE, # >18.000;
                                    verbose          = TRUE,
                                    lang             = "en",
                                    until            = 2019-12-28 # 2015-07-19, returns tweets created before the given date. The search index has a 7-day limit;
                                    ))

save_as_csv(rtweet_rest, "~/ML-POP/Twitter_Rest_Data/rtweet_rest_europe",
            prepend_ids = TRUE, na = "",
            fileEncoding = "UTF-8")
# View(rtweet_rest)

######################################################################################################################################################################################################################################################################################################################################
## {STEP ONE: DATA PREPROCESSING} - SELECTING CRITICAL TWEETS & BUILDING SECONDARY FEATURES ##########################################################################################################################################################################################################################################
######################################################################################################################################################################################################################################################################################################################################

rtweet_rest <- read_twitter_csv("~/ML-POP/Twitter_Rest_Data/rtweet_rest_europe.csv", unflatten = T)

# View(rtweet_rest)

##
## MASTER DATASET Sys.Date(){15Jan} - RETWEETS {50 < x < 100} & FOLLOWERS {x < 75000};
##

{ # {} START HERE;
  
master_rtweet_rest <- rtweet_rest[c(which(rtweet_rest$retweet_count >= 50 & rtweet_rest$retweet_count <= 100 & rtweet_rest$followers_count <= 75000)), c(which(colnames(rtweet_rest) == c("user_id", "status_id", "created_at")), which(colnames(rtweet_rest) == "followers_count"), which(colnames(rtweet_rest) == "retweet_count"))]
# summary(master_rtweet_rest$retweet_count)

colnames(master_rtweet_rest)[colnames(master_rtweet_rest) == "retweet_count"] <- "retweet_count_28Dic"
colnames(master_rtweet_rest)[colnames(master_rtweet_rest) == "followers_count"] <- "followers_count_28Dic"

master_rtweet_rest$created_at <- as.POSIXct(master_rtweet_rest$created_at)
# View(master_rtweet_rest)

} # {} END HERE;

##
## SECONDARY FEATURES Sys.Date(){15Jan} - IDs, TIME and FOLLOWERs;
##

{ # {} START HERE;
  
retweeters_id          <- data.frame() # IDs OF WHO RESHARED THE TWEET;
retweeters_id[100, 1:dim(master_rtweet_rest)[1]]          <- NA
retweeters_time        <- data.frame() # TIME OF EACH RESHARE (CONNECTED BY ROW TO "retweeters_id");
retweeters_time[100, 1:dim(master_rtweet_rest)[1]]        <- NA
retweeters_followers   <- data.frame() # FOLLOWERS OF IDs IN "retweeters_id" (CONNECTED BY ROW TO "retweeters_id"); 
retweeters_followers[100, 1:dim(master_rtweet_rest)[1]]   <- NA

n <- c(1:as.integer(dim(master_rtweet_rest)[1]))
for(i in n){
  
  print(paste0("Iteration: ", i));
  colnames(retweeters_id)[i]          = master_rtweet_rest[i, 2];
  colnames(retweeters_time)[i]        = master_rtweet_rest[i, 2];
  colnames(retweeters_followers)[i]   = master_rtweet_rest[i, 2];

  # GETTING THE NUMBER OF TOTAL RESHARES FOR EACH TWEET - MASTER DATASET;
  retweets  = get_retweets(as.character(master_rtweet_rest[i, 2]), 
                           n     = 100,
                           parse = TRUE,
                           token = token);
  if(dim(retweets)[1] != 0)
  {
    print(paste0("TOKEN ONE {RETWEETS}: ", round(i/(75 * 1) * 100, 2), "% ", "Retweets: ", paste0(dim(retweets)[1])));
  }
  
  if(dim(retweets)[1] == 0) # EACH TOKEN HAS A LIMIT OF ~75 RETWEETS;
  {
    retweets = get_retweets(as.character(master_rtweet_rest[i, 2]), 
                            n     = 100,
                            parse = TRUE,
                            token = tokenBIS);
    
    if(dim(retweets)[1] != 0)
    {
      print(paste0("TOKEN TWO {RETWEETS}: ", round((i - 75)/75 * 100, 2), "% ", "Retweets: ", paste0(dim(retweets)[1])));
    }
    
    if(dim(retweets)[1] == 0)
    {
      
      retweets = get_retweets(as.character(master_rtweet_rest[i, 2]), 
                              n     = 100,
                              parse = TRUE,
                              token = tokenTRIS);
      
      if(dim(retweets)[1] != 0)
      {
        print(paste0("TOKEN THREE {RETWEETS}: ", round((i - 150)/75 * 100, 2), "% ", "Retweets: ", paste0(dim(retweets)[1])));
      }
      
      if(dim(retweets)[1] == 0)
      {
        
        retweets = get_retweets(as.character(master_rtweet_rest[i, 2]), 
                                n = 100,
                                parse = TRUE,
                                token = tokenQUADRO);
        
        if(dim(retweets)[1] != 0)
        {
          print(paste0("TOKEN FOUR {RETWEETS}: ", round((i - 225)/75 * 100, 2), "% ", "Retweets: ", paste0(dim(retweets)[1])));
        }
        
        if(dim(retweets)[1] == 0)
        {
          
          retweets = get_retweets(as.character(master_rtweet_rest[i, 2]), 
                                  n = 100,
                                  parse = TRUE,
                                  token = tokenQUINTET);
          
          if(dim(retweets)[1] != 0)
          {
            print(paste0("TOKEN FIVE {RETWEETS}: ", round((i - 300)/75 * 100, 2), "% ", "Retweets: ", paste0(dim(retweets)[1])));
          }
          j = i + 1;
          if(round((i - 300)/75 * 100, 2) > 100)
          {
            Sys.sleep(900);
          }
        }
      }
    }
  }
  
  # DERIVING THE FEATURES - SLAVE DATASETS;
  retweets$created_at = as.character(retweets$created_at)
  
  if(dim(retweets)[1] == 0)
  {
    master_rtweet_rest$RESHARES[i] = NA;
    print("DEAD!");
  }
  else
  {
    if(retweets$retweet_count > 100)
    {
      master_rtweet_rest$RESHARES[i] = NA;
    }
    else
    {
      master_rtweet_rest$RESHARES[i]  = retweets$retweet_count[1];
      master_rtweet_rest$FOLLOWERS[i] = retweets$retweet_followers_count[1];
      
      k = c(1:as.integer(dim(retweets)[1]))
      for(j in k)
      {
        retweeters_id[j, i] = retweets[j, 1];
        # print(retweeters_id[j, i]);
      }
      for(u in k)
      {
        retweeters_time[u, i] = retweets[u, 3];
        # print(retweeters_time[u, i]);
      }
      for(r in k)
      {
        retweeters_followers[r, i] = retweets[r, 78];
        # print(retweeters_followers[r, i]);
      }
    }
  }
}

flagID                            <- which(is.na(master_rtweet_rest$RESHARES))
master_rtweet_rest                <- master_rtweet_rest[-flagID,]
retweeters_id                     <- retweeters_id[, -flagID]
retweeters_time                   <- retweeters_time[, -flagID]
retweeters_followers              <- retweeters_followers[, -flagID]

# Sys.Date <- "2020-01-15"
# write_feather(master_rtweet_rest, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/master_", Sys.Date(), ".feather"))
# write_feather(retweeters_id, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/id_", Sys.Date(), ".feather"))
# write_feather(retweeters_time, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/time_", Sys.Date(), ".feather"))
# write_feather(retweeters_followers, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/followers_", Sys.Date(), ".feather"))

} # {} END HERE;

##
## SECONDARY FEATURES Sys.Date(){15Jan} - REACTIONs;
##

{ # {} START HERE;
  
retweeters_PREreaction <- data.frame()
retweeters_PREreaction[100, 1:dim(master_rtweet_rest)[1]] <- NA
retweeters_reaction    <- data.frame()
retweeters_reaction[100, 1:dim(master_rtweet_rest)[1]]    <- NA

n <- c(1:as.integer(dim(master_rtweet_rest)[1]))
k <- c(1:as.integer(dim(retweeters_reaction)[1]))

for(i in n){
  colnames(retweeters_reaction)[i]    = master_rtweet_rest[i, 2];
  colnames(retweeters_PREreaction)[i] = master_rtweet_rest[i, 2];
  date.list                           = retweeters_time[, i];
  for(j in as.list(date.list))
  {
    retweeters_PREreaction[, i] = as.POSIXct(strptime(as.character(j), format = "%Y-%m-%d %H:%M:%S"), optional = TRUE)
  }
  for(r in k)
  {
    retweeters_reaction[r, i] = difftime(retweeters_PREreaction[r, i], master_rtweet_rest$created_at[i], units = "secs");
  }
}

# Sys.Date <- "2020-01-15"
# write_feather(retweeters_PREreaction, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/PREreaction_", Sys.Date(), ".feather"))
# write_feather(retweeters_reaction, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/reaction_", Sys.Date(), ".feather"))

} # {} END HERE;

##
## SECONDARY FEATURES Sys.Date(){15Jan} - FATHER & SONs;
##

{ # {} START HERE;
  
DIM                <- 70000 
master_rtweet_rest <- data.frame(master_rtweet_rest %>% mutate(SUM_FOLLOWERS   = cumsum(master_rtweet_rest$FOLLOWERS),
                                                               GROUP_FOLLOWERS = SUM_FOLLOWERS/DIM));

private_intervals <- {function(x)
                        {
                          if(max(master_rtweet_rest$SUM_FOLLOWERS)/DIM %% as.numeric(substr(max(master_rtweet_rest$SUM_FOLLOWERS)/DIM, start = 1, stop = 2)) < 0.5) 
                          {
                          round(max(master_rtweet_rest$SUM_FOLLOWERS)/DIM, 0) + 1;
                          }
                          else
                          {
                          round(max(master_rtweet_rest$SUM_FOLLOWERS)/DIM, 0);
                          }
                        }
                      }
vector_intervals  <-   {data.frame(token        = seq(1, private_intervals(), 5),
                                  
                                   tokenBIS     = if(length(seq(2, private_intervals(), 5)) != length(seq(1, private_intervals(), 5)))
                                                  {
                                                    c(seq(2, private_intervals(), 5), NA);
                                                  }
                                                  else
                                                  {
                                                    seq(2, private_intervals(), 5);
                                                  },
                                   
                                   tokenTRIS    = if(length(seq(3, private_intervals(), 5)) != length(seq(1, private_intervals(), 5)))
                                                  {
                                                    c(seq(3, private_intervals(), 5), NA);
                                                  }
                                                  else
                                                  {
                                                    seq(3, private_intervals(), 5);
                                                  },
                                   
                                   tokenQUADRO  = if(length(seq(4, private_intervals(), 5)) != length(seq(1, private_intervals(), 5)))
                                                  {
                                                    c(seq(4, private_intervals(), 5), NA);
                                                  }
                                                  else
                                                  {
                                                    seq(4, private_intervals(), 5);
                                                  },
                                   
                                   tokenQUINTET = if(length(seq(5, private_intervals(), 5)) != length(seq(1, private_intervals(), 5)))
                                                  {
                                                    c(seq(5, private_intervals(), 5), NA);
                                                  }
                                                  else
                                                  {
                                                    seq(5, private_intervals(), 5);
                                                  }
                                   )}

m <- c(1:dim(master_rtweet_rest)[1])
for(i in m){
  if(any(master_rtweet_rest[i, 9] <= vector_intervals[, 1] & master_rtweet_rest[i, 9] > c(vector_intervals[, 1] - 1)) == TRUE)
  {
    master_rtweet_rest$Tag[i] = "token";
  }
  if(any(master_rtweet_rest[i, 9] <= as.numeric(na.omit(vector_intervals[, 2])) & master_rtweet_rest[i, 9] > c(as.numeric(na.omit(vector_intervals[, 2] - 1)))) == TRUE)
  {
    master_rtweet_rest$Tag[i] = "tokenBIS";
  }
  if(any(master_rtweet_rest[i, 9] <= as.numeric(na.omit(vector_intervals[, 3])) & master_rtweet_rest[i, 9] > c(as.numeric(na.omit(vector_intervals[, 3] - 1)))) == TRUE)
  {
    master_rtweet_rest$Tag[i] = "tokenTRIS";
  }
  if(any(master_rtweet_rest[i, 9] <= as.numeric(na.omit(vector_intervals[, 4])) & master_rtweet_rest[i, 9] > c(as.numeric(na.omit(vector_intervals[, 4] - 1)))) == TRUE)
  {
    master_rtweet_rest$Tag[i] = "tokenQUADRO";
  }
  if(any(master_rtweet_rest[i, 9] <= as.numeric(na.omit(vector_intervals[, 5])) & master_rtweet_rest[i, 9] > c(as.numeric(na.omit(vector_intervals[, 5] - 1)))) == TRUE)
  {
    master_rtweet_rest$Tag[i] = "tokenQUINTET";
  }
}

retweeters_father <- data.frame() # FOLLOWERS OF FATHERs IN "master_rtweet_rest$status_id";
retweeters_father[1:75000, 1:dim(master_rtweet_rest)[1]]  <- NA

warning("No error.")

for(i in m){
  
  colnames(retweeters_father)[i] = master_rtweet_rest[i, 2];
  
  # GETTING THE FATHER's FOLLOWERS IDs;
  if(master_rtweet_rest[i, 10] == "token")
  {
    followers  = get_followers(as.character(master_rtweet_rest[i, 1]), 
                               n     = 75000,
                               parse = TRUE,
                               token = token);
    print(paste0("Iteration: ", i));
    print(paste0("TOKEN ONE {FOLLOWERS}: ", round(master_rtweet_rest[i, 9]/1 * 100, 2), "% ", "Followers: ", dim(followers)[1]));
    Sys.sleep(3);
  }
  
  if(master_rtweet_rest[i, 10] == "tokenBIS")
  {
    followers  = get_followers(as.character(master_rtweet_rest[i, 1]), 
                               n     = 75000,
                               parse = TRUE,
                               token = tokenBIS);
    print(paste0("Iteration: ", i));
    print(paste0("TOKEN TWO {FOLLOWERS}: ", round(master_rtweet_rest[i, 9]/2 * 100, 2), "% ", "Followers: ", dim(followers)[1]));
    Sys.sleep(3);
  }
  
  if(master_rtweet_rest[i, 10] == "tokenTRIS")
  {
    followers  = get_followers(as.character(master_rtweet_rest[i, 1]), 
                               n     = 75000,
                               parse = TRUE,
                               token = tokenTRIS);
    print(paste0("Iteration: ", i));
    print(paste0("TOKEN THREE {FOLLOWERS}: ", round(master_rtweet_rest[i, 9]/3 * 100, 2), "% ", "Followers: ", dim(followers)[1]));
    Sys.sleep(3);
  }
  
  if(master_rtweet_rest[i, 10] == "tokenQUADRO")
  {
    followers  = get_followers(as.character(master_rtweet_rest[i, 1]), 
                               n     = 75000,
                               parse = TRUE,
                               token = tokenQUADRO);
    print(paste0("Iteration: ", i));
    print(paste0("TOKEN FOUR {FOLLOWERS}: ", round(master_rtweet_rest[i, 9]/4 * 100, 2), "% ", "Followers: ", dim(followers)[1]));
    Sys.sleep(3);
  }
  
  if(master_rtweet_rest[i, 10] == "tokenQUINTET")
  {
    followers  = get_followers(as.character(master_rtweet_rest[i, 1]), 
                               n     = 75000,
                               parse = TRUE,
                               token = tokenQUINTET);
    print(paste0("Iteration: ", i));
    print(paste0("TOKEN FIVE {FOLLOWERS}: ", round(master_rtweet_rest[i, 9]/5 * 100, 2), "% ", "Followers: ", dim(followers)[1]));
    Sys.sleep(3);
    
    exhaustion = i + 1;
    if(master_rtweet_rest[exhaustion, 10] == "token")
    {
      print(paste0("Sleeping for 15Mins", Sys.time()));
      Sys.sleep(900);
    }
  }

  # DERIVING THE FEATURES - SLAVE DATASETS;
  if(length(grep("Not authorized.", names(last.warning))) != 0 | length(grep("Sorry, that page does not exist.", names(last.warning))) != 0 | dim(followers)[1] == 0)
  {
    retweeters_father[, i]         = NA;
    master_rtweet_rest$RESHARES[i] = NA;
    assign("last.warning", NULL, envir = baseenv());
  }
  else
  {
    z = c(1:dim(followers)[1])
    for(f in z)
    {
      retweeters_father[f, i] = followers[f, 1];
      # print(followers[f, i]);
    }
  }
}

# Sys.Date <- "2020-01-15"
# write_feather(retweeters_father, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/father_", Sys.Date(), ".feather"))

} # {} END HERE;

##
## SECONDARY FEATURES Sys.Date(){15Jan} - SONs SUBSET;
##

{ # {} START HERE;

retweeters_sons <- data.frame() # SONs OF FATHERs IN "master_rtweet_rest$status_id";
retweeters_sons[1:100, 1:dim(master_rtweet_rest)[1]]  <- NA

k <- c(1:dim(master_rtweet_rest)[1])
for(i in k){
  
  colnames(retweeters_sons)[i] = master_rtweet_rest[i, 2];
  SONs = c(1:dim(as.data.frame(na.omit(retweeters_id[, i]))));
  for(j in SONs){
    if(is.na(retweeters_father[1, i]) != TRUE)
    {
      
      if(any(as.character(retweeters_id[j, i]) == na.omit(retweeters_father[, i])) == TRUE) 
      {
        retweeters_sons[j, i] = 1;
      }
      else
      {
        retweeters_sons[j, i] = 0;
      }
      
    }
    else
    {
      retweeters_sons[, i] = 0;
      break;
    }
  }
}

colnames(master_rtweet_rest)[colnames(master_rtweet_rest) == "RESHARES"] <- "retweet_count_15Jan"
colnames(master_rtweet_rest)[colnames(master_rtweet_rest) == "FOLLOWERS"] <- "followers_count_15Jan"
master_rtweet_rest <- master_rtweet_rest[, c(1, 2, 3, 4, 5, 7, 6)]

flagID                      <- which(colSums(is.na(retweeters_sons)) <= 100 & rowSums(retweeters_sons, na.rm = TRUE) >= 1)
master_rtweet_rest_SONs     <- master_rtweet_rest[flagID,]

retweeters_id_SONs          <- data.frame()
retweeters_id_SONs[100, 1:dim(master_rtweet_rest_SONs)[1]]          <- NA
retweeters_time_SONs        <- data.frame()
retweeters_time_SONs[100, 1:dim(master_rtweet_rest_SONs)[1]]        <- NA
retweeters_followers_SONs   <- data.frame()
retweeters_followers_SONs[100, 1:dim(master_rtweet_rest_SONs)[1]]   <- NA
retweeters_PREreaction_SONs <- data.frame()
retweeters_PREreaction_SONs[100, 1:dim(master_rtweet_rest_SONs)[1]] <- NA
retweeters_reaction_SONs    <- data.frame()
retweeters_reaction_SONs[100, 1:dim(master_rtweet_rest_SONs)[1]]    <- NA

n <- c(1:as.integer(dim(master_rtweet_rest_SONs)[1]))
for(i in n){
  
  colnames(retweeters_id_SONs)[i]          = master_rtweet_rest_SONs[i, 2];
  colnames(retweeters_time_SONs)[i]        = master_rtweet_rest_SONs[i, 2];
  colnames(retweeters_followers_SONs)[i]   = master_rtweet_rest_SONs[i, 2];
  colnames(retweeters_PREreaction_SONs)[i] = master_rtweet_rest_SONs[i, 2];
  colnames(retweeters_reaction_SONs)[i]    = master_rtweet_rest_SONs[i, 2];
  
  ROW_RETWEETS                             = which(retweeters_sons[, i] == 1);
  ROW_NAMES                                = c(1:length(ROW_RETWEETS));
  
  for(j in ROW_NAMES)
  {
    retweeters_id_SONs[j, i]               = retweeters_id[j, i]
    retweeters_time_SONs[j, i]             = retweeters_time[j, i]
    retweeters_followers_SONs[j, i]        = retweeters_followers[j, i]
    retweeters_PREreaction_SONs[j, i]      = retweeters_PREreaction[j, i]
    retweeters_reaction_SONs[j, i]         = retweeters_reaction[j, i]
  }
}

# Sys.Date <- "2020-01-15"
# write_feather(retweeters_sons, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/sons_", Sys.Date(), ".feather"))
# write_feather(master_rtweet_rest, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/", Sys.Date(), "/master_", Sys.Date(), ".feather"))

# write_feather(master_rtweet_rest_SONs, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/SONs/master_SONs_", Sys.Date(), ".feather"))
# write_feather(retweeters_id_SONs, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/SONs/id_SONs_", Sys.Date(), ".feather"))
# write_feather(retweeters_time_SONs, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/SONs/time_SONs_", Sys.Date(), ".feather"))
# write_feather(retweeters_followers_SONs, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/SONs/followers_SONs_", Sys.Date(), ".feather"))
# write_feather(retweeters_reaction_SONs, paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/SONs/reaction_SONs_", Sys.Date(), ".feather"))

} # {} END HERE;

######################################################################################################################################################################################################################################################################################################################################
## {STEP TWO: MODEL FITTING} - ESTIMATING REACTION TIME DISTRIBUTION #################################################################################################################################################################################################################################################################
######################################################################################################################################################################################################################################################################################################################################

# Sys.Date <- "2020-01-15"
master_rtweet_rest_SONs   <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/SONs/master_SONs_", Sys.Date, ".feather"))
retweeters_reaction_SONs  <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/SONs/reaction_SONs_", Sys.Date, ".feather"))

## REACTION DENSITY PLOT - SONs SAMPLE;
density_reaction <- data.frame(secs = unlist(retweeters_reaction_SONs, use.names = FALSE))
density_reaction <- density_reaction %>%
  mutate(minutes = secs/60,
         hours   = minutes/60)
density_reaction <- as.data.frame(na.omit(density_reaction))
# BREAKS = 100;
# hist(density_reaction$minutes, breaks = 100)
# BREAKS = MAX;
# hist(density_reaction$minutes, breaks = c(unique(density_reaction$minutes)), freq = TRUE)

INTERVALS                      <- 5
density_summarized             <- data.frame(table(cut(density_reaction$minutes, breaks = seq(0, max(density_reaction$minutes), INTERVALS)))) 
density_summarized$Classe      <- seq(INTERVALS, max(density_reaction$minutes), INTERVALS)
density_summarized$log_Classe  <- log(density_summarized$Classe)
density_summarized$Density     <- density_summarized$Freq/sum(density_summarized$Freq)
density_summarized$log_Density <- log(density_summarized$Density)
density_summarized             <- density_summarized[-c(1, which(density_summarized$Freq == 0)),]
## NORMAL;
## ggplot(data = density_summarized, aes(x = Classe, y = Density)) + geom_point(colour = "indianred") + theme_bw()
## LOG;
## ggplot(data = density_summarized, aes(x = log(Classe), y = log(Density))) + geom_point(colour = "indianred") + theme_bw()

##
## ESTIMATING REACTION TIME DISTRIBUTION
##

COSTANT_INT         <- seq(10, 200, 10)
TRAIN_CONTROL       <- trainControl(method = "cv", number = 10)

ERROR_RESULTS_CV    <- data.frame(RMSE       = rep(NA, length(COSTANT_INT)),
                                  Rsquared   = rep(NA, length(COSTANT_INT)),
                                  MAE        = rep(NA, length(COSTANT_INT)),
                                  RMSESD     = rep(NA, length(COSTANT_INT)),
                                  RsqauredSD = rep(NA, length(COSTANT_INT)),
                                  MAESD      = rep(NA, length(COSTANT_INT)))
ERROR_RESULTS_FINAL <- data.frame(RMSE       = rep(NA, length(COSTANT_INT)),
                                  MAE        = rep(NA, length(COSTANT_INT)))

FITTED_VECTOR        <- data.frame()
FITTED_VECTOR[c(1:dim(density_summarized)[1]), c(1:length(COSTANT_INT))]        <- NA
FITTED_RESIDUALS_ABS <- data.frame()
FITTED_RESIDUALS_ABS[c(1:dim(density_summarized)[1]), c(1:length(COSTANT_INT))] <- NA
FITTED_RESIDUALS_SQR <- data.frame()
FITTED_RESIDUALS_SQR[c(1:dim(density_summarized)[1]), c(1:length(COSTANT_INT))] <- NA

EQUATION_FIT    <- function(x) { as.numeric(fit_plus$finalModel[[1]])[1] + as.numeric(fit_plus$finalModel[[1]])[2] * log(x) }
PREDICTED_FINAL <- function(x) { ifelse(x <= COSTANT_INT[r], exp(EQUATION_FIT(COSTANT_INT[r])), exp(EQUATION_FIT(COSTANT_INT[r])) * (x/COSTANT_INT[r])^(-1 * (1 + as.numeric(fit_plus$finalModel[[1]])[2] * (-1) - 1))) }
SQR_RESIDUALS   <- function(x) { sqrt(density_summarized$Density[l] - x) }
ABS_RESIDUALS   <- function(x) {  abs(density_summarized$Density[l] - x) }

LENGTH_INT      <- c(1:length(COSTANT_INT))
LENGTH_DATA     <- c(1:dim(density_summarized)[1])
j               <- 1;

for(r in LENGTH_INT)
{
    
  density_plus = density_summarized[which(density_summarized$Classe > as.numeric(COSTANT_INT[r])),];
  fit_plus     = train(log_Density ~ log_Classe, data = density_plus, method = "lm", trControl = TRAIN_CONTROL);
    
  ERROR_RESULTS_CV[r, 1]         = as.numeric(fit_plus$results[2]);      
  ERROR_RESULTS_CV[r, 2]         = as.numeric(fit_plus$results[3]);
  ERROR_RESULTS_CV[r, 3]         = as.numeric(fit_plus$results[4]);
  ERROR_RESULTS_CV[r, 4]         = as.numeric(fit_plus$results[5]);
  ERROR_RESULTS_CV[r, 5]         = as.numeric(fit_plus$results[6]);
  ERROR_RESULTS_CV[r, 6]         = as.numeric(fit_plus$results[7]);
    
  colnames(FITTED_VECTOR)[r]        = COSTANT_INT[r];
  colnames(FITTED_RESIDUALS_SQR)[r] = COSTANT_INT[r];
  colnames(FITTED_RESIDUALS_ABS)[r] = COSTANT_INT[r];
    
  for(l in LENGTH_DATA)
  {
      
    FITTED_VECTOR[l, r]        = PREDICTED_FINAL(density_summarized$Classe[l]);
    FITTED_RESIDUALS_SQR[l, r] = SQR_RESIDUALS(FITTED_VECTOR[l, r]);
    FITTED_RESIDUALS_ABS[l, r] = ABS_RESIDUALS(FITTED_VECTOR[l, r]);
      
  }
    
  ERROR_RESULTS_FINAL[r, 1] = sum(FITTED_RESIDUALS_SQR[, r], na.rm = TRUE)/dim(density_summarized)[1];    
  ERROR_RESULTS_FINAL[r, 2] = sum(FITTED_RESIDUALS_ABS[, r], na.rm = TRUE)/dim(density_summarized)[1];
}

# ggplot() + geom_point(data = ERROR_RESULTS_FINAL, aes(x = c(1:20), y = scale(RMSE)), colour = "indianred") + geom_line(data = ERROR_RESULTS_FINAL, aes(x = c(1:20), y = scale(MAE)), colour = "dodgerblue4") + theme_bw()

##
## PLOTTING THE FITTED DISTRIBUTION - 140MINs COSTANT;
##

LENGTH_DATA <- c(1:dim(density_summarized)[1])
for(i in LENGTH_DATA){
  density_summarized$Fitted[i] = PREDICTED_FINAL(density_summarized$Classe[i]);
}
## NORMAL;
## ggplot() + geom_point(data = density_summarized, aes(x = Classe, y = Density), colour = "indianred", size = 1.5, alpha = 0.75) + geom_line(data = density_summarized, aes(x = Classe, y = Fitted), colour = "dodgerblue4", size = 1) + theme_bw()
## LOG;
## ggplot() + geom_point(data = density_summarized, aes(x = log(Classe), y = log(Density)), colour = "indianred") + geom_line(data = density_summarized, aes(x = log(Classe), y = log(Fitted)), colour = "dodgerblue4") + theme_bw()

######################################################################################################################################################################################################################################################################################################################################
## {STEP THREE: IMPLEMENTATION} - ESTIMATING INFECTIOUSNESS AND RUNNING THE ALGORITHM ################################################################################################################################################################################################################################################
######################################################################################################################################################################################################################################################################################################################################

# Sys.Date <- "2020-01-15"
master_rtweet_rest   <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/master_", Sys.Date, ".feather"))
retweeters_followers <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/followers_", Sys.Date, ".feather"))
retweeters_reaction  <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPONE/2020-01-15/reaction_", Sys.Date, ".feather"))

infectiousness_t                  <- c(seq(5, 360, 5), seq(370, 720, 10), seq(740, 1420, 20), seq(1440, 21600, 360))
retweeters_infectiousness_DYNAMIC <- data.frame()        
retweeters_infectiousness_DYNAMIC[1:dim(master_rtweet_rest)[1], 1:length(infectiousness_t)] <- NA
retweeters_final_reshares_DYNAMIC <- data.frame()        
retweeters_final_reshares_DYNAMIC[1:dim(master_rtweet_rest)[1], 1:length(infectiousness_t)] <- NA
retweeters_NET_DYNAMIC            <- data.frame()        
retweeters_NET_DYNAMIC[1:dim(master_rtweet_rest)[1], 1:length(infectiousness_t)]            <- NA
retweeters_R_DYNAMIC              <- data.frame()        
retweeters_R_DYNAMIC[1:dim(master_rtweet_rest)[1], 1:length(infectiousness_t)]              <- NA
  
##
## C;
##
  
THRESHOLD <- 3;
DELTA     <- (-0.5380841 * (-1)) - 1; # (-0.5380841 * (-1)) - 1; # (-0.9480841 * (-1)) - 1; 
c_FINAL   <- DELTA/(THRESHOLD * (DELTA + 1 - (7200/THRESHOLD)^(-1 * (DELTA))))
  
##
## N-E-T - Effective cumulative degree of resharers by time t - & DYNAMIC Infectiousness;
##
  
INTEGRAND <- function(x) 
{ 
  LEFT  = ifelse(x >= 0 & x <= infectiousness_t[j]/2, (1 - 2 * x)/infectiousness_t[j], 0);
  RIGHT = ifelse(x <= THRESHOLD, c_FINAL, ifelse(x > THRESHOLD & x <= 7200, c_FINAL * (x/THRESHOLD)^(-1 - DELTA), 0)); # DYNAMIC INFECTIOUSNESS
  return(LEFT * RIGHT);
}
  
estimate_R <- function(x) 
{ 
  LEFT  = ifelse(x >= 0 & x <= infectiousness_t[j]/2, (1 - 2 * 50)/infectiousness_t[j], 0);
}
  
k         <- c(1:dim(master_rtweet_rest)[1])
p         <- c(1:length(infectiousness_t))

for(j in p){
    
  print(c("Time (mins) analyzed:", infectiousness_t[j], c("Missing iterations:", max(p) - j)));
  colnames(retweeters_infectiousness_DYNAMIC)[j] = infectiousness_t[j];
  colnames(retweeters_final_reshares_DYNAMIC)[j] = infectiousness_t[j];
  colnames(retweeters_NET_DYNAMIC)[j]            = infectiousness_t[j];
  colnames(retweeters_R_DYNAMIC)[j]              = infectiousness_t[j];
    
  for(i in k)
  {
  
    if(dim(na.omit(retweeters_reaction[, i]))[1] == 0)
    {
      
      if(j == 1)
      {
        print(c("Tweet number", i, "is dead."))
      }
        next;
    }
      
    retweets_total = c(1:dim(na.omit(retweeters_reaction[, i]))[1]);
    NET            = data.frame(VALUES = (rep(NA, max(retweets_total))));
    R              = data.frame(VALUES = (rep(NA, max(retweets_total))));
    N              = data.frame(VALUES = (rep(NA, max(retweets_total))))
      
    for(l in retweets_total)
    {
        
      if(is.na(retweeters_reaction[l, i]))
      {
        next;
      }
      if(retweeters_reaction[l, i] <= infectiousness_t[j])
      {
        NET[l, 1] = retweeters_followers[l, i] * integrate(INTEGRAND, lower = as.numeric(data.frame(retweeters_reaction[l, i]/60)), upper = infectiousness_t[j])$value;
        R[l, 1]   = estimate_R(as.numeric(retweeters_reaction[l, i]));
        N[l, 1]   = retweeters_followers[l, i]; 
      }
      else
      {
        NET[l, 1] = NA;
        R[l, 1]   = NA;
        N[l, 1]   = NA;
      }
    }
    if(length(which(is.na(NET))) == max(retweets_total))
    {
      retweeters_NET_DYNAMIC[i, j] = NA;
      retweeters_R_DYNAMIC[i, j]   = NA;
    }
    else
    {
      retweeters_NET_DYNAMIC[i, j] = sum(NET[, 1], na.rm = TRUE);
      retweeters_R_DYNAMIC[i, j]   = sum(R[,1], na.rm = TRUE);
    }
    if(is.na(retweeters_NET_DYNAMIC[i, j]))
    {
      retweeters_infectiousness_DYNAMIC[i, j] = NA;
    }
    else
    {
      retweeters_infectiousness_DYNAMIC[i, j] = retweeters_R_DYNAMIC[i, j] / retweeters_NET_DYNAMIC[i, j];
    }
    if(length(which(is.na(retweeters_followers[, j]) == TRUE)) == dim(retweeters_followers[, j])[1])
    {
      retweeters_final_reshares_DYNAMIC[i, j] = NA;
    }
    else
    {
      retweeters_final_reshares_DYNAMIC[i, j] = max(retweets_total) + (retweeters_infectiousness_DYNAMIC[i, j] * (sum(N[, 1], na.rm = TRUE) - retweeters_NET_DYNAMIC[i, j]))/(1 - retweeters_infectiousness_DYNAMIC[i, j] * (sum(retweeters_followers[, j], na.rm = TRUE)/max(retweets_total)));
    }
  }
}

##
## SUMMARIZING RESULTS;
##

{ # {} START HERE;

k   <- c(1:dim(retweeters_infectiousness_DYNAMIC)[2])
m   <- c(1:dim(master_rtweet_rest)[1])
  
APE <- data.frame(W_API                    = rep(NA, max(m)))
N   <- data.frame(time                     = infectiousness_t,
                  infectiousness           = rep(NA, max(k)),
                  ESTIMATED_FINAL_RESHARES = rep(NA, max(k)),
                  quantile_5               = rep(NA, max(k)),
                  quantile_10              = rep(NA, max(k)),
                  quantile_25              = rep(NA, max(k)),
                  quantile_50              = rep(NA, max(k)),
                  quantile_75              = rep(NA, max(k)),
                  quantile_90              = rep(NA, max(k)),
                  quantile_95              = rep(NA, max(k)))
for(i in k){
    
  N[i, 2] = mean(retweeters_infectiousness_DYNAMIC[, i], na.rm = TRUE);
  N[i, 3] = sum(retweeters_final_reshares_DYNAMIC[, i], na.rm = TRUE);
  
  for(j in m)
  {
    retweets_total = c(1:dim(na.omit(retweeters_reaction[, j]))[1]);
    APE[j, 1] = (retweeters_final_reshares_DYNAMIC[j, i] - master_rtweet_rest[j, 7])/master_rtweet_rest[j, 7];
  }
  
  N[i, 4]  = quantile(abs(APE[, 1]), 0.05, na.rm = TRUE);
  N[i, 5]  = quantile(abs(APE[, 1]), 0.10, na.rm = TRUE);
  N[i, 6]  = quantile(abs(APE[, 1]), 0.25, na.rm = TRUE);
  N[i, 7]  = quantile(abs(APE[, 1]), 0.50, na.rm = TRUE);
  N[i, 8]  = quantile(abs(APE[, 1]), 0.75, na.rm = TRUE);
  N[i, 9]  = quantile(abs(APE[, 1]), 0.90, na.rm = TRUE);
  N[i, 10] = quantile(abs(APE[, 1]), 0.95, na.rm = TRUE);
}
  
O = data.frame(VALUES = rep(NA, max(m)))
for(i in m){
  O[i, 1] = dim(na.omit(retweeters_reaction[, i]))[1];
}

# Sys.Date <- "2020-01-15"
## SEISMIC ORIGINAL;
# write_feather(retweeters_infectiousness_DYNAMIC, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/infectiousness_DYNAMIC_", Sys.Date, ".feather"))
# write_feather(retweeters_final_reshares_DYNAMIC, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/reshares_DYNAMIC_", Sys.Date, ".feather"))
# write_feather(retweeters_NET_DYNAMIC, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/NET_DYNAMIC_", Sys.Date, ".feather"))
# write_feather(retweeters_R_DYNAMIC, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/R_DYNAMIC_", Sys.Date, ".feather"))
# write_feather(N, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/N_", Sys.Date, ".feather"))
# write_feather(O, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/O_", Sys.Date, ".feather"))

# Sys.Date <- "2020-01-15"
## SEISMIC MODIFIED;
# write_feather(retweeters_infectiousness_DYNAMIC, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/infectiousness_DYNAMIC_", Sys.Date, ".feather"))
# write_feather(retweeters_final_reshares_DYNAMIC, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/reshares_DYNAMIC_", Sys.Date, ".feather"))
# write_feather(retweeters_NET_DYNAMIC, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/NET_DYNAMIC_", Sys.Date, ".feather"))
# write_feather(retweeters_R_DYNAMIC, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/R_DYNAMIC_", Sys.Date, ".feather"))
# write_feather(N, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/N_", Sys.Date, ".feather"))
# write_feather(O, paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/O_", Sys.Date, ".feather"))

} # {} END HERE;
  
##
## COMPARING MODELS - MODIFIED SEISMIC vs ORIGINAL SEISMIC;
##

## (1) ORIGINAL
# Sys.Date <- "2020-01-15"
N_ORIGINAL      <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/N_", Sys.Date, ".feather"))
O_ORIGINAL      <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/O_", Sys.Date, ".feather"))
FINALS_ORIGINAL <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/SEISMIC/reshares_DYNAMIC_", Sys.Date, ".feather"))

## (2) MODIFIED
# Sys.Date <- "2020-01-15"
N_MODIFIED      <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/N_", Sys.Date, ".feather"))
O_MODIFIED      <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/O_", Sys.Date, ".feather"))
FINALS_MODIFIED <- read_feather(paste0("~/ML-POP/Twitter_Rest_Data/STEPTWO/2020-01-15/reshares_DYNAMIC_", Sys.Date, ".feather"))

RESULTS_DYNAMIC <- list(
    
## PREDICTED RESHARES vs TOTAL REAL RESHARES at TIME(t);
a = {
     ggplot() +
     # geom_line(data = N_ORIGINAL, aes(x = time, y = ESTIMATED_FINAL_RESHARES), colour = "indianred", size = 1) +
     geom_line(data = N_MODIFIED, aes(x = time, y = ESTIMATED_FINAL_RESHARES), colour = "indianred", size = 1) +
     geom_line(aes(x = c(0:max(infectiousness_t)), y = rep(sum(O), length(c(0:max(infectiousness_t))))), colour = "dodgerblue4", size = 1) +
    
     # LEGEND;
     # geom_line(aes(x = seq(3, 4, 0.01), y = rep(100000, 101)), colour = "indianred", size = 2) +
     # geom_text(aes(x = 7.5, y = 100000), label = "SEISMIC", hjust = 1.2, size = 3) +
     # geom_line(aes(x = seq(3, 4, 0.01), y = rep(70000, 101)), colour = "chartreuse4", size = 2) +
     # geom_text(aes(x = 8, y = 70000), label = "MODIFIED", hjust = 1.2, size = 3) +
     
     # 30MINs;
     geom_line(aes(x = rep(30, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 30, y = 5), label= "30m" , hjust = 1.2, size = 5) +
    
     # 1HOURs;
     geom_line(aes(x = rep(60, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 60, y = 5), label= "1h" , hjust = 1.2, size = 5) +
    
    # 5HOURs;
     geom_line(aes(x = rep(300, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 300, y = 5), label= "5h" , hjust = 1.2, size = 5) +
        
     # 24HOURs;
     geom_line(aes(x = rep(1440, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 1440, y = 5), label= "24h" , hjust = 1.2, size = 5) +
        
     # 15DAYs;
     geom_line(aes(x = rep(21600, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 21600, y = 5), label= "15d" , hjust = 1.2, size = 5) +
        
     theme_bw() +
     labs(subtitle = "Prediction by SEISMIC",
                 y = "Retweets",
                 x = "Time since original tweet (mins)") +
     theme(text = element_text(size = 14)) +
     scale_x_log10() +
     scale_y_log10() 
    },
    
## AVERAGE INFECTIOUSNESS vs TIME(t);
b = {
     ggplot() +
     geom_line(data = N_ORIGINAL, aes(x = time, y = infectiousness), colour = "indianred", size = 1) +
     geom_line(data = N_MODIFIED, aes(x = time, y = infectiousness), colour = "chartreuse4", size = 1) +
    
     # 30MINs;  
     geom_line(aes(x = rep(30, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 30, y = 5), label= "30m" , hjust = 1.2, size = 5) +
     
     # 1HOURs;
     geom_line(aes(x = rep(60, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 60, y = 5), label= "1h" , hjust = 1.2, size = 5) +
    
     # 5HOURs;
     geom_line(aes(x = rep(300, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 300, y = 5), label= "5h" , hjust = 1.2, size = 5) +
    
     # 24HOURs;
     geom_line(aes(x = rep(1440, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 1440, y = 5), label= "24h" , hjust = 1.2, size = 5) +
    
     # 15DAYs;
     geom_line(aes(x = rep(21600, length(c(0:sum(O)))), y = c(0:sum(O))), colour = "dodgerblue4", size = 1) +
     geom_text(aes(x = 21600, y = 5), label= "15d" , hjust = 1.2, size = 5) +
    
     theme_bw() +
     labs(subtitle = "Infectiousness Estimated by SEISMIC",
                 y = "Infectiousness",
                 x = "Time since original tweet (mins)") +
     theme(text = element_text(size = 14)) +
     scale_x_log10() +
     scale_y_log10() 
    },
    
## APE's QUANTILES vs TIME(t);
c = {
     ggplot() +
    
     ## ORIGINAL;
     # geom_line(data = N_ORIGINAL[which(N_ORIGINAL$time < 900),], aes(x = time, y = quantile_50), colour = "dodgerblue4", size = 1, linetype = "dashed") +
     # geom_line(data = N_ORIGINAL[which(N_ORIGINAL$time < 900),], aes(x = time, y = quantile_5),  colour = "chartreuse4", size = 1, linetype = "dashed") +
     # geom_line(data = N_ORIGINAL[which(N_ORIGINAL$time < 900),], aes(x = time, y = quantile_95), colour = "chartreuse4", size = 1, linetype = "dashed") +
     # geom_line(data = N_ORIGINAL[which(N_ORIGINAL$time < 900),], aes(x = time, y = quantile_10), colour = "chartreuse4", size = 1, linetype = "dashed") +
     # geom_line(data = N_ORIGINAL[which(N_ORIGINAL$time < 900),], aes(x = time, y = quantile_90), colour = "chartreuse4", size = 1, linetype = "dashed") +
     # geom_line(data = N_ORIGINAL[which(N_ORIGINAL$time < 900),], aes(x = time, y = quantile_25), colour = "indianred", size = 1, linetype = "dashed") +
     # geom_line(data = N_ORIGINAL[which(N_ORIGINAL$time < 900),], aes(x = time, y = quantile_75), colour = "indianred", size = 1, linetype = "dashed") +
    
     ## MODIFIED;
     geom_line(data = N_MODIFIED, aes(x = time, y = quantile_50), colour = "dodgerblue4", size = 1) +
     # geom_line(data = N_MODIFIED, aes(x = time, y = quantile_5),  colour = "chartreuse4",  size = 1, linetype = "dashed") +
     # geom_line(data = N_MODIFIED, aes(x = time, y = quantile_95), colour = "chartreuse4",  size = 1, linetype = "dashed") +
     geom_line(data = N_MODIFIED, aes(x = time, y = quantile_10), colour = "chartreuse4", size = 1, linetype = "dashed") +
     geom_line(data = N_MODIFIED, aes(x = time, y = quantile_90), colour = "chartreuse4", size = 1, linetype = "dashed") +
     geom_line(data = N_MODIFIED, aes(x = time, y = quantile_25), colour = "indianred", size = 1, linetype = "dashed") +
     geom_line(data = N_MODIFIED, aes(x = time, y = quantile_75), colour = "indianred", size = 1, linetype = "dashed") +
     
     # LEGEND;
     geom_line(aes(x = seq(10, 14, 0.04), y = rep(0.9, 101)), colour = "chartreuse4", size = 1, linetype = "dashed") +
     geom_text(aes(x = 20, y = 0.9), label = "80%", hjust = 1.2, size = 4) +
     geom_line(aes(x = seq(10, 14, 0.04), y = rep(0.8, 101)), colour = "indianred", size = 1, linetype = "dashed") +
     geom_text(aes(x = 20, y = 0.8), label = "50%", hjust = 1.2, size = 4) +
     geom_line(aes(x = seq(10, 13.35, 0.0335), y = rep(0.7, 101)), colour = "dodgerblue4", size = 2) +
     geom_text(aes(x = 23, y = 0.7), label = "Median", hjust = 1.2, size = 4) +
    
     theme_bw() +
     labs(subtitle = "Distribution of APE for SEISMIC",
                 y = "APE",
                 x = "Time (minute)") +
     theme(text = element_text(size = 14)) +
    scale_x_log10() +
    scale_y_log10() 
  
    }
  )

##            ########################################################################################################################################################################################################################################################################################################################
## REFERENCES ########################################################################################################################################################################################################################################################################################################################
##            ########################################################################################################################################################################################################################################################################################################################

# Package streamR
# https://cran.r-project.org/web/packages/streamR/streamR.pdf

# Initializing the Twitter API
# http://politicaldatascience.blogspot.com/2015/12/rtutorial-using-r-to-harvest-twitter.html

# Twitter Authentication with R
# http://thinktostart.com/twitter-authentification-with-r/

# Package rtweet
# https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

# Using Geodata and Geolocation in the Social Sciences: Mapping our Connected World
# https://books.google.it/books?id=6KHUDAAAQBAJ&printsec=frontcover&hl=it#v=onepage&q&f=false

# Intro to rtweet: Collecting Twitter Data
# https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html

# Quick guide to Game of Thrones S8 - Twitter
# https://www.kaggle.com/monogenea/quick-guide-to-game-of-thrones-s8-twitter

# fitdistrplus: An R Package for Fitting Distributions
# https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf

# How to determine which distribution fits my data best?
# https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best

# FITTING DISTRIBUTIONS WITH R
# https://cran.r-project.org/doc/contrib/Ricci-distributions-en.pdf

# Use R to Compute Numerical Integrals
# http://homepages.math.uic.edu/~jyang06/stat401/handouts/handout8.pdf
