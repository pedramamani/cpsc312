# CPSC 312 Project

# Liva

Liva is a simple personalizable tool for recording your daily activities and visualizing these records. It empowers you to observe trends in your habits and manage your time more effectively.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).


## Team Members

Our team is:

+ Pedram Amani (73993008)
+ Bokai Wang (81121865)

We call ourselves: Nostalgia.


## Product Pitch

In a given day, you spend each of the 1440 valuable minutes on some activity (i.e. sleeping, working, pondering). Survey a day of your life and record everything you do down to the minute. You may be surprised at how much time is wasted on futile activities that do not align with your goals. I was.

Now imagine having access to this data for the past year of your life. What insights into your habits would it provide? How would it guide you to better manage your time? We believe in the value of time and the utility of keeping such a record. Our project introduces an entirely personalizable tool to record your activities, called Liva.

Liva starts with a user-provided personal activity tree. Below is a simple activity tree example. For an explanation of the formatting and a simplified version of my own activity tree, refer to Appendix 1.

```
Life
    Work
    Study
        CPSC 312
        Physics
    Sleep
```

Assuming the labels are not duplicates (which we do), an activity is defined simply by its label.  For example, “CPSC 312” corresponds to studying CPSC 312 and “Study” corresponds to studying (which is less specific, but that is okay). A record is then an activity with a start time, end time, and a date.

Finally, our vision for Liva is a web application with a simple user interface that provides the following functions:

+ add new records and store them in an online database
+ modify records
+ modify activity tree by adding new categories
+ visualize records for any time span and at any level of the activity tree
+ visualize trends in records across any time period


## Minimal Viable Project

For the MVP, Liva will have a command-line interface with only the following of the above functions:

+ add new records and store them in a local database
+ visualize records for any time span and at any level of the activity tree

We spent some time discussing the command-line interface needed to provide these functions and came up with the following:

+ `liva load <activityTreeFilePath>` will parse the activity tree from the given file and store it in a local database
+ `liva add <activityLabel> <startTime> <endTime> <date>` will create a new record with provided arguments and store it in a local database
+ `liva start <activityLabel>` and `liva end <activityLabel>` will perform the same function as `liva add` except the start/end times and the date are inferred
+ `liva show <activityLabel> <startDate> <endDate>` will load all the records in the given date range, aggregate the times spent performing each sub-activity of the provided activity, and display the results as a pie chart

This MVP covers a wide range of programming topics and will build on what was learnt in the Haskell portion of the course, i.e. functions, recursion, data types and polymorphism, etc. In addition, it will require us to make extensive use of:
+ `System.IO` for creating and modifying the activity tree and records databases
+ `System.Console.ArgParser` for creating the command-line interface


## Proof of Concept

The personalizable activity tree is at the heart of Liva and all the other functions interact with it in some way. That is why, for the proof-of-concept, we focus on the key part of `liva load` defined above, which is parsing the activity tree from a file into a tree data structure. For this, we will implement the functions:

+ `checkFormat <filePath>` to check if the file has correct format and return a helpful error
+ `load <filePath>` to parses the file contents and return the corresponding tree in some recursive representation

Moving data into or out of a database is a key part of Liva, of which the latter is arguably more challenging. Completing the activity tree parsing component, which is one of the most tricky, shows that Liva is a viable project given the time-frame of this course.


### How to test and run the code

We have included 7 example tree files in the `haskell/assets` directory. Running `stack test`
tests the code on 5 of these examples. The other 2 files are `tree_my.txt` (which is a realistic activity tree input) and `tree_your.txt` which is an opportunity to write your own tree and have it parsed by Liva.

After running `stack ghci`, you can parse `tree_your.txt` by simply running `main` (no need to reload GHCI after making changes to the file). Or you can try format-checking/parsing any of the other trees via `checkFormat` and `load`. We recommend at least trying `checkFormat "assets/tree_error_indentation.txt"` and `load "assets/tree_my.txt"`.

Note: Calling `load` on a file without first checking its format does not fail but may produce an undesirable (incorrect) output. We have not combined the two into one function because in the future there are use cases where a tree that is already format-checked needs to be loaded.

## Appendix 1: Activity tree example
Below is a textual representation of my activity tree. `Life` is the root node and each of the following 1-indented lines (`Learn`, `Work`, `Apply`, `Entertain`, `Sustain`) are a child node of `Life`. Similarly, each extra indentation makes a node a child of the prior unindented node.

```
Life
    Learn
        High school
        UBC
            Year 1
            Year 2
            Year 3
            Year 4
                CPSC 312
                CPSC 340
                PHYS 526
        Self-teach
            ML
            Guitar
    Work
        Research
            Quantum information
            Molecular optics
        TA
        Self-employed
            Tutor
            Website design
    Apply
        Government
            Tax
            Citizenship
        School
            Undergraduate
            Graduate
        Job
    Entertain
        Read
        Watch
            YouTube
            Movie
        Produce
            3D print
            Code
                Hackathon
                Personal website
                Quadroid
            Graphic design
            Student team
    Sustain
        Hygiene
        Sleep
        Eat
        Exercise
            Hike
            Gym
            Run
        Plan
```
