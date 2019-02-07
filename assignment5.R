library(tidyverse)
library(data.table)
library(dplyr)
library(plyr)
library(gsubfn)
library(qdapRegex)
library(stringr)
library(ggridges)


#Assignment 5

#Function that reads any txt files
#path_file = The path to the file in a string EX) "C:/Users/antho/Documents/sta141/messy/losangeles/"
#file = txt file name in a string EX) "_ant_apa_d_1bd-1ba-tennis-court_6738904358.txt"
read_post <- function(path_file, file) {
  
    #Concatenate the path to file with file into a single string
    read_the_file <- str_c(path_file, "/", file, sep = "", collapse = NULL)
    readLines(con = read_the_file)

}
file.number.1 <- read_post(path_file = "C:/Users/antho/Documents/sta141/messy/losangeles", file = "_ant_apa_d_1bd-1ba-tennis-court_6738904358.txt")



#Function that reads all txt files into a DataFrame

read_all_posts <- function(directory) {
  
  #Empty List to be filled
  all.list.items <- list()

  #Finding all text file names
  files <- list.files(directory, full.names = TRUE)
  
  #Concatenating paths to text filenames, Filling the empty List
  all.list.items <- sapply(files, function(file) {
      post = readLines(file, encoding = "UTF-8")
      post_string = str_c(post, collapse = "\n")
  })
  
  #Extracting the Title (First Row of Text)
  posts_df <- (data.frame(text = all.list.items, region = basename(directory)))
  result = str_split_fixed(posts_df$text, "\n", 7)
  posts_df$title = result[, 1]
  
  
  #Extracting the Price (Question 4)
  user.price_pattern = ("Price:\\s\\$[0-9,]+")
  user_price =  sapply(posts_df$text, str_extract, user.price_pattern)
  user_prices = substring(user_price, 9)
  posts_df$price <- user_prices
  posts_df$price <- as.numeric(posts_df$price)
  
  #Extracting the Deposit (Question 5)
  #user.deposit_text = str_extract(tolower(posts_df$text), "(\\$[0-9,.]+ deposit)|(deposit[:] \\$[0-9,.])")
  #deposit_text = str_remove_all(user.deposit_text,"[deposit(:| )\\$]|(\\$|deposit$|,)|(\\.$)+")
  
  user.deposit_text = regex(".eposits?[:] \\$[0-9,]+|\\[0-9,]+ deposits?", ignore_case = TRUE)
  deposit_text = sapply(posts_df$text, str_extract, user.deposit_text)
  deposit_text = substring(deposit_text, 11)
  posts_df$deposit <- deposit_text
  posts_df$deposit <- as.numeric(gsub(",","",posts_df$deposit))
  
  
  #Pets ok? (Question 6)
  
  #Pet Patterns
  cats_only = regex("cats? allowed|cats? ok|cats? deposit|cats? only", ignore_case = TRUE)
  dogs_only = regex("dogs? allowed|dogs? ok|dogs? deposit|dogs? only", ignore_case = TRUE)
  both_pets = regex("all pets|all pets? allowed|both dogs? and cats?|dogs? and cats? ok|pets?-friendly|pet friendly|pets? deposit|cats? and dogs? OK", ignore_case = TRUE)
  no_pets = regex("no pets?|cannot have pets?|no animals?", ignore_case = TRUE)
  
  #String Extraction
  only_cats = str_extract(posts_df$text, cats_only)
  only_dogs = str_extract(posts_df$text, dogs_only)
  pets_both = str_extract(posts_df$text, both_pets)
  pets_none = str_extract(posts_df$text, no_pets)
  
  #Binding Together
  posts_df$pets <- coalesce(only_cats, only_dogs, pets_both, pets_none)
  
  #Replacing String
  posts_df$pets = str_replace(posts_df$pets, pattern = dogs_only, "Dogs")
  posts_df$pets = str_replace(posts_df$pets, pattern = cats_only, "Cats")
  posts_df$pets = str_replace(posts_df$pets, pattern = both_pets, "Both")
  posts_df$pets = str_replace(posts_df$pets, pattern = no_pets, "No pets")
  
  #Extracting Pet Deposit
  pet.deposit.pattern1 = regex("pet deposit[:] \\$[0-9,]+", ignore_case = TRUE)
  pet.deposit_text1 = str_extract(posts_df$text, pet.deposit.pattern1)
  pet.deposit.pattern2 = regex("pet security deposit[:] \\$[0-9,]+", ignore_case =TRUE)
  pet.deposit_text2 = str_extract(posts_df$text, pet.deposit.pattern2)
  pet.deposit.pattern3 = regex("pet deposit \\$[0-9,]+", ignore_case = TRUE)
  pet.deposit_text3 = str_extract(posts_df$text, pet.deposit.pattern3)
  pet.deposit.pattern4 = regex("pet security deposit \\$[0-9,]+", ignore_case = TRUE)
  pet.deposit_text4 = str_extract(posts_df$text, pet.deposit.pattern4)

  #Creating Pet Deposit Column
  posts_df$pet_deposit <- coalesce(pet.deposit_text1, pet.deposit_text2, pet.deposit_text3, pet.deposit_text4) #pet.deposit.pattern5)
  posts_df$pet_deposit <- as.numeric(gsub("\\D", "", posts_df$pet_deposit))

  #Problem 7 Heating and Cooling
  
  #Patterns
  heater = regex("only heater|heater only|central heating|heating inside|thermostat|temperature control", ignore_case = TRUE)
  fireplace = regex("fire place|fireplace|woodstove|wood stove", ignore_Case = TRUE)
  no.heating = regex("no heating|none heating|no heater|heating none|heating no", ignore_case = TRUE)
  
  #Creating the numeric DataFrames
  air_condition = regex("Air Conditioning|\b(A|a)ir\b|cooler", ignore_case = TRUE) 
  air_condition.1 = regex("AC")
  air_conditioning = str_extract(posts_df$text, air_condition)
  air_conditioning.1 = str_extract(posts_df$text, air_condition.1)
  posts_df$aircondition = coalesce(air_conditioning, air_conditioning.1)
  posts_df$aircondition = str_replace(posts_df$aircondition, pattern = air_condition, "AC")
  posts_df$aircondition = str_replace(posts_df$aircondition, pattern = air_condition.1, "AC")
  
  
  #Str Extraction
  posts_df$heater = str_extract(posts_df$text, heater)
  posts_df$fireplace = str_extract(posts_df$text, fireplace)
  
  posts_df$heater = str_replace(posts_df$heater, pattern = heater, "heater")
  posts_df$fireplace = str_replace(posts_df$fireplace, pattern = fireplace, "fireplace")

  #Assigning numeric values to combine
  posts_df$heater[is.na(posts_df$heater)] <- 0
  posts_df$heater[posts_df$heater =="heater"] <- 1
  posts_df$heater = as.numeric(posts_df$heater)
  posts_df$fireplace[is.na(posts_df$fireplace)] <- 0
  posts_df$fireplace[posts_df$fireplace =="fireplace"] <- 2
  posts_df$fireplace = as.numeric(posts_df$fireplace)

  #Creating the desired Dataframe
  posts_df$heating_fireplace = posts_df$heater + posts_df$fireplace
  posts_df$heating_fireplace[posts_df$heating_fireplace == 0] <- NA
  posts_df$heating_fireplace[posts_df$heating_fireplace == 1] <- "heater"
  posts_df$heating_fireplace[posts_df$heating_fireplace == 2] <- "fireplace"
  posts_df$heating_fireplace[posts_df$heating_fireplace == 3] <- "both"

  
  
  #Problem 8 Email Address Hiding?
  
  #Patterns
  email_pattern = regex("@...", ignore_case = TRUE)
  phone_pattern = regex("\\(?\\d{3}\\)?( |-)\\d{3}( |-)(\\d|o){4}", ignore_case = TRUE)
  show_contact_info = regex("show contact info", ignore_case = TRUE)
  
  #String Extraction
  posts_df$email = str_extract(posts_df$text, email_pattern)
  posts_df$phone = str_extract(posts_df$text, phone_pattern)
  posts_df$contact_info = str_extract(posts_df$text, show_contact_info)
  
  #String Replacement
  posts_df$email = str_replace(posts_df$email, pattern = email_pattern, "email")
  posts_df$phone = str_replace(posts_df$phone, pattern = phone_pattern, "phone")
  posts_df$contact_info = str_replace(posts_df$contact_info, pattern = show_contact_info, "contact info shown")
  
  #Assigning numbers to combine
  posts_df$email[is.na(posts_df$email)] <- 0
  posts_df$email[posts_df$email =="email"] <- 1
  posts_df$email = as.numeric(posts_df$email)
  
  posts_df$phone[is.na(posts_df$phone)] <- 0
  posts_df$phone[posts_df$email =="phone"] <- 2
  posts_df$phone = as.numeric(posts_df$phone)
  
  posts_df$contact_info[is.na(posts_df$contact_info)] <- 0
  posts_df$contact_info[posts_df$contact_info =="contact info shown"] <- 4
  posts_df$contact_info = as.numeric(posts_df$contact_info)
  
  #Constructing the desired column
  posts_df$information = posts_df$email + posts_df$phone + posts_df$contact_info
  posts_df$information[posts_df$information == 0] <- NA
  posts_df$information[posts_df$information == 1] <- "Email Address"
  posts_df$information[posts_df$information == 2] <- "Phone Number"
  posts_df$information[posts_df$information == 3] <- "email and phone only"
  posts_df$information[posts_df$information == 4] <- "Feature Used"
  posts_df$information[posts_df$information == 5] <- "Feature Used and Email Shown"
  posts_df$information[posts_df$information == 6] <- "show contact information and phone"
  posts_df$information[posts_df$information == 7] <- "all avaliable information"
  

  return(posts_df)
}

dirs <- list.files("C:/Users/antho/Documents/sta141/messy/", full.names = TRUE)

posts = lapply(dirs, read_all_posts)
posts_df = do.call(rbind, posts)


#Question 5 Plotting Rent against security deposit, Color is craigslist region
ggplot(posts_df, aes(x = price, y = deposit, color = region)) +
  geom_jitter(width = 500, height = 500, alpha = 0.35) + 
  xlim(0, 6000) + 
  ylim(0, 6000) + 
  guides(fill=guide_legend(title="Craigslist Region")) +
  labs(title = "Rent vs. Deposit on Craiglist Apartments", x = "Monthly Rent", y = "Security Deposit") 



cor(posts_df$price, posts_df$deposit, use = "complete.obs")

#Question 6 Plotting Pet Security Deposit against the type of pet
ggplot(posts_df, aes(x = pet_deposit, y = pets)) +
  xlim(200, 750) +
  geom_density_ridges() + 
  labs(title = "Pet Security Deposit vs. Type of Pet: Dogs, Cats, and Both") +
  xlab("Pet") +
  ylab ("Pet Deposit")


#Question 7 Plotting Heating Options to AC
ggplot(data=subset(posts_df, !is.na(heating_fireplace)), aes(x = (heating_fireplace), fill = aircondition)) + 
  geom_bar(position = "fill") +
  labs(title = "Heating and Air Conditioning", x = "Type of Heating", y = "Proportion") +
  guides(fill=guide_legend(title="Air Conditioning Avaliability"))
  

table(posts_df$heating_fireplace)
table(posts_df$aircondition)

#Question 8 Plotting Most common ways to access the listers information
ggplot(data=subset(posts_df, !is.na(information))) +
  geom_bar(aes(x = information)) +
  labs(title = "Type of Contact Information Avaliable", x = "Avaliable Contact Information", y = "Count") 




    