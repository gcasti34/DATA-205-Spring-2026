DATA: 

My capstone project plan will focus on animal welfare, primarily in Montgomery County. The two datasets are from Data Montgomery and are named OAS - Animal Impound and OAS – Animal Shelter Pathway. The two datasets contain information on animals impounded by the county shelter. The shelter is an open admission shelter that takes in lost, abandoned, and surrendered pets as well as injured/ill wildlife. The “Animal Shelter Pathway” dataset is rich with animal characteristics like: Animal type, Breed, Sex, and Age. While other dataset contains more information on intake type, outcome, time of entry/exit, and shelter location, including longitude/latitude data. The primary objective is to analyze variables within a one-year time window to provide the most current trends and findings. The only quality issue is with the OAS Animal Intake dataset, which confirms that animals enter the adoption process but lacks a variable to confirm if the animal has a place to call home. This raises the possibility of contacting the shelter that records all this data to obtain confirmation on the animals, whether they have been adopted or are still in search of a possible owner. Fortunately, the two animal datasets join easily due to their common variable of Animal ID, and little data is lost in the join. The huge concern for the project is to get the time variables prepped in MDYhms to create a new variable of total time spent by the animal in the shelter.

Research Questions:  

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

Method: I will group outcome types with bite history and compare through some percentages. This will be shown through a stacked bar chart

Tools, methods and resources: 

       I will be using R for the first part of my project due to its statistical analysis and easy data visualization. The methods that I will use in R are summary statistics like averages, grouping, and counts. The main library I will use is ggplot2, along with the already installed R libraries dplyr and tidyr. The other program I will be using is Tableau to create the GIS map for my project. I find this program to be very user-friendly and much easier to create a GIS map than R. 



