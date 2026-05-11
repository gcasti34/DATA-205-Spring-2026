# DATA: 

My capstone project plan will focus on animal welfare, primarily in Montgomery County. The two datasets are from Data Montgomery and are named OAS - Animal Impound and OAS – Animal Shelter Pathway. The two datasets contain information on animals impounded by the county shelter. The shelter is an open admission shelter that takes in lost, abandoned, and surrendered pets as well as injured/ill wildlife. The “Animal Shelter Pathway” dataset is rich in animal characteristics such as Animal type, Breed, Sex, and Age. While other datasets contain more information on intake type, outcome, time of entry/exit, and shelter location, including longitude/latitude data. The primary objective is to analyze variables within a one-year time window to provide the most current trends and findings. The only quality issue is with the OAS Animal Intake dataset, which confirms that animals enter the adoption process but lacks a variable to confirm if the animal has a place to call home. This raises the possibility of contacting the shelter that maintains this data to confirm whether the animals have been adopted or are still seeking an owner. Fortunately, the two animal datasets join easily due to their common variable of Animal ID, and little data is lost in the join. The huge concern for the project is to get the time variables prepped in MDYhms to create a new variable of total time spent by the animal in the shelter.
## Project Structure

- [Capstone Project Code](Capstone%20Project%20Code/)
- [Raw Datasets](Raw%20Datasets/)
- [visuals](visuals/)
# Goals:
The goal of the capstone project is to analyze shelter data by examining intake type, breed, age, length of stay, outcome, and other variables to determine which factors have the greatest influence on animal outcomes. Providing valuable information to pet owners surrendering their pets and shelters to reduce impounds and improve adoption rates.

# Data cleaning/pre-processing:

The two datasets were joined easily with a left join, as both datasets had the same variable, Animal ID. Through the join, I was able to retain around 85% of each dataset in the combined dataframe throughout the project. The process for the cleaning was fairly simple, involving renaming columns, which required the removal of periods and underscores. This made it easier to recall variables in my R code and in the creation of the graphs. The other issue was creating a new time variable to calculate the total time each animal spent from arrival to outcome. This required the arrival and departure date to be set to a POSIT time variable, and using the difftime function to get the difference between the times in days.

 
 

# Research Questions:  

This capstone will be used to analyze patterns in animal intakes and shelter outcomes. As well, discover the main factors that influence whether an animal faces adoption, transfer, and euthanizing. To establish a better understanding of my primary goals for this project, questions must be asked and answered:

·       What are the most common types of animals entering the shelter system?

Method: I will group by animal type, create a count variable, and visualize with a bar chart

·       How long do animals typically stay in the shelter?

Method: I will create a new variable called Length of stay from the time variable and get the mean.

·       What percentage of animals are adopted vs. euthanized vs. transferred?

Method: Group by outcome and visualize with a bar chart.

·       What breeds are most frequently impounded?

Method: a frequency count on breed and make a simple table. 

·       Are certain times of the year associated with higher intake rates?

Method: create a frequency table by month 

·       Are certain intake reasons more likely to lead to euthanasia?

Method: Group animals by Intake Reason and create a percentage table and then graph the result as a line graph

·       Does age affect euthanizing likelihood?

Method: I will group age from baby, young, and old and create a bar chart to compare the results 

·       What are the most common reasons for intake?

Method: Frequency count of Intake Reason and bar chart

·       Which shelters receive the most animals?

Method: Count entries by Shelter Location and bar chart

·       Does the animal bite history affect its outcome?

Method: I will group outcome types with bite history and compare them through some percentages. This will be shown through a stacked bar chart
# Key Findings
![Graph](visuals/Animal%20Outcomes%20by%20percent.png)

The graph shows that most animals are getting adopted with 62.2%, while the Return to owner 16.2%, and euthanizations 10.4% follow it.  
![Graph2](visuals/Animal%20Type%20by%20Age%20Group%20Euthanizations.png)

This bar chart clearly shows the age distribution of the animals that were euthanized. Most of the animals euthanized fall in the ages of 1- 7, and mostly dogs. However, the 10+ age group only consists of dogs and cats, and is the third largest age group for euthanizing. The main observation is that dogs and cats make up most of the groups throughout every age group, but most animals are euthanized in the ages 1-7 and 10+. 

![Graph3](visuals/Average%20Shelter%20Stay.png)

The line chart shows mostly an upward trend peaking in August and December. In August, the average animal stayed around 22 days, while in December it peaked at 26 days. These two numbers are way too long, and it’s a sad analysis that around these months are also when there are the most animals, as the shelters try to keep the animals as long as possible so they can be able to get adopted. Numbers around the start of the year of 2025 should be the average throughout the year of  <10 days on average. But the averages kept going up through the year and started to fall in February. 

![Graph4](visuals/Euthansia%20Rate%20by%20intake.png)

The bar chart shows that Euthanasia Required is the highest cause, with around 76% that the shelters euthanizing the animals. However, the second most cases is when the animal is brought in as a stray, with less than 10% of the cases. The owner's surrenders also make up less than 10 %. 

![Graph5](visuals/Monthly%20Shelter%20Outcomes.png)

The bar chart shows that throughout July, August, and December, these are the months that have the most impounds. The big observation is that Adoption ranks the highest outcome for each month for the animals. This is a huge positive for the animals, and the second ranking outcome is the return to the owner, which is seen in the yellow. This shows that the shelter system does a good job in returning lost animals to their owners. However, the big issue is that the numbers for euthanizing are high, and this is where my project will focus on reducing by analyzing the trends of the animals that fall under this terrible outcome. 

![Graph6](visuals/Outcome%20with%20bite%20history.png)

The chart shows that most animals that have a bite history are saved and adopted to a new home. But the euthanization cases rank 2, with around 140 cases when the animal has a bite history. 

![Graph7](visuals/Outcomes%20with%20no%20bite%20history.png)

The chart also shows that most animals that have no bite history are, for the most part, adopted. But unlike with bite history, these animals are mostly returned to their owners.

![Graph8](visuals/Top%20Breeds.png)
![Graph7](visuals/Top%2010%20Breeds%20by%20Adoption%20Rate.png)

The top graphs show that most of the animals being impounded are the Domestic Shorthair Cat being over 2,500 cases. Following the shorthair will be the American Pitbull Terrier with around 500 cases. These two animals are the most impounded, but only the Domestic Shorthair pops up in the top 10 most adopted breeds. It goes to show that the reputation the Pitbull has terrible toll for being adopted in Maryland. Sadly, in Prince George, they aren’t allowed to be adopted, so shelters are forced to look for rescue groups outside the county (Leybengrub, 2025). However, Shelter is striving to reach 40 dog adoptions or foster a week to avoid overcrowding (Leybengrub, 2025). These two graphs highlight that even the most impounded animals aren’t going to be the most adopted in percentage, as more niche pets like tropical, Parakeet, and Finches. This suggests that owners who are looking to surrender their Pitbulls or Shorthairs do everything possible to keep the pet or give it to a family member. 



# Basic descriptive statistics:

The combined Dataset has a lot of good variables that I was able to explore. The key variables I examined were the Age, Bite history, Breed, Outcome, and Animal type. Another variable I used was the days-spent variable for each animal, which I calculated from the arrival and departure dates. The two quantitative variables I had a mean analysis on were the age and the days spent. The mean for days spent was 17.30, while the age mean was 3.74. The age mean surprised me the most because I thought it would be at least 5+, but many animals are entering the shelter system at a very young age. The number of days spent is also concerning because 17 days is a long time for these animals to be without homes, and this is what I proposed to help reduce the time by examining data. 


# Tools, methods, and resources: 
 I will be using R for the first part of my project due to its statistical analysis and easy data visualization. The methods that I will use in R are summary statistics like averages, grouping, and counts. The main library I will use is ggplot2, along with the already installed R libraries dplyr and tidyr. The other program I will be using is Tableau to create the GIS map for my project. I find this program to be very user-friendly and much easier to create a GIS map than R.
# Final Product/Tableau Link:
https://public.tableau.com/views/AnimalShelterUser-InterfaceTableau/AnimalDashboard?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link 
# Acknowledgements:
I would like to acknowledge my professor, Lori Perine, for all the help and guidance she provided to get the resources and advice to better my project. I will also acknowledge Victoria Liu for helping me find people who are in charge of the two datasets for further questions on the variables and questions about my results. Thank you to Maria Anselmo for answering all the questions I had on the datasets. 
# References:
Leybengrub, N. (2025, October 6). Economic hardship drives overcrowding at Maryland’s animal shelters. The Banner. https://www.thebanner.com/community/local-news/housing-insecurity-animal-shelter-overcrowding-NDIV52NSC5DRHKDBUAUYF26HCM/ 
Maryland demographics: Maryland Business Data. business.maryland.gov. (2025, January 13). https://business.maryland.gov/plan-your-move/demographics/  
Montgomery County, M. (2026, May 10). Oas - animal impound: Open Data Portal. OAS - Animal Impound | Open Data Portal. https://data.montgomerycountymd.gov/Public-Safety/OAS-Animal-Impound/6nf9-ewgt/about_data 
Montgomery County, M. (2026b, May 10). Oas - Animal Shelter Pathway: Open data portal. OAS - Animal Shelter Pathway | Open Data Portal. https://data.montgomerycountymd.gov/Public-Safety/OAS-Animal-Shelter-Pathway/hsz7-ef2y/about_data 
<img width="2523" height="144" alt="image" src="https://github.com/user-attachments/assets/4ec72922-b49c-4d19-a9e0-195f035461d4" />






