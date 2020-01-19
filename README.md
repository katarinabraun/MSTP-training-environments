# MSTP-training-environments
To accompany the manuscript entitled: "Gender Can Influence Student Experiences in MD-PhD Training"

The repository contains raw data and code (Python 3 and R) to generate each figure in this paper. 

**Figure 1**. Program gender demographics varied over time. Yearly program size by gender shows female representation has varied, but has been increasing in recent years. The total number of female students in the program each year is represented by light-grey circles; the total number of male students in the program each year is represented by dark-grey squares. 

**Figure 2**. Invited guest and faculty speakers at the annual symposium are more likely to be male. Equal representation of male and female student, faculty and keynote speakers at the annual symposium was determined using an exact binomial test. (A) Female and male students were equally like to be invited to speak at symposium (p=0.576). (B) Faculty speakers were significantly more likely to be male (p=0.001). (C) Invited Keynote speakers were also significantly more likely to be male (p=0.012).  

**Figure 3**. Number of total and first-author publications by gender. Publications counts are from MSTP students while they are students in the program. These data are from students who graduated between the years 2009 - 2019. There are no statistically significant differences in first author and total publications from men and women. However, male students, on average, had one additional paper from their PhD work compared to female students.  

**Figure 4**. Gender predicts question-asking behavior in a weekly MSTP seminar during the 2017-2018 academic year. Questions posed to a female speaker are represented as light-grey circles; questions posed to a male speaker are represented as dark-grey squares. (A) Women were significantly underrepresented as question-askers (p<0.005). (B) The percentage of audience members who asked questions when the speaker was female is compared to the percentage who asked questions when the speaker was male. Female speakers were more likely than male speakers to receive questions from both male and female audience members (p=0.03).

**Figure 5**. Gender does not predict question-asking behavior in a weekly MSTP seminar during the 2018-2019 academic year. Questions posed to a female speaker are represented as light-grey circles; questions posed to a male speaker are represented as dark-grey squares. (A) Women and men were equally represented as question-askers (p=0.30). (B) The percentage of audience members who asked questions when the speaker was female is compared to the percentage who asked questions when the speaker was male. The likelihood of female and male speakers receiving questions from both female and male audience members did not differ significantly (p=0.65).

### Dependencies
All required dependencies are imported into the Jupyter Notebook Files or R scripts as they run. 

### To run 
**Figures 1, 2, and 3**: Navigate to the `code` directory and launch Jupyter Notebook from the command line by typing `Jupyter Notebook`. Run each script in full or cell by cell. 

**Figures 4 and 5**: Download the R scripts and run `data_cleaning`, followed by `analysis` and finally `make_figures`. 

## Output
These scripts should generate a PDF and save it to the `figure` directory. Figures 1, 2, and 3 generate PDFs and figures 4 and 5 generate png files. 

Please reach out to Kat Braun @ kmbraun2@gmail.edu with questions. 

## Authors
**Figures 1, 2 and 3**: code written by Katarina Braun. 
**Figures 4 and 5**: code written by Cora Allen-Coleman. 

