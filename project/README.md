# Liva

Liva is a simple personalizable tool for recording your daily activities and visualizing these records. It empowers you to observe trends in your habits and manage your time more effectively.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).


## Team Nostalgia

+ Pedram Amani (73993008)
+ Bokai Wang (81121865)


## Demonstration Video

The video is uploaded [here](https://youtu.be/CBErXl8trHI). It is slightly longer than 4 minutes, so please watch at 1.25x speed if necessary. At 1.25x speed, it takes about 3:50 to watch the video.


## Proposal Document

Out Haskell proposal for Liva can be found [here](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/Proposal.md).


## MVP Overview

As proposed, we have implemented a command-line interface for Liva.
Our implementation includes all the proposed functionality (with minor adjustments) and more.
Below is a summary of the available commands:

+ `load <filePath>`: Load activity tree from a text file
+ `reload`: Reload the current activity tree
+ `start <activityName>`: Start recording an activity
+ `stop`: Stop recording the current activity
+ `cancel`: Cancel the current activity recording
+ `status`: Get current activity status
+ `reset`: Erase activity tree and all recordings
+ `view <activityName> <startDate> <endDate>`: View a summary of all sub-activities in given date range

The command-line application is implemented in [Main.hs](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/app/Main.hs), and the above functions are implemented in [Interface.hs](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Interface.hs).
Other than that, below are some key elements of Liva we want to showcase:

+ Functions [`today`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L138) and [`now`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L94) to obtain the current date and time using the `Data.Time` module
+ A recursive data type [`Node`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L202) for representing activity trees and useful functions [`allKeys`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L229) and [`find`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L232) to work with them
+ The function [`visualize`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L248) to draw a bar graph on the command-line
+ The class [`Parsable`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L37) for elements that can store data to and parse data from database files


## Guide To Running MVP

We have added tests for some basic functionalities of Liva. Run these from the `haskell` directory with `stack test`. Then proceed to the fun part.

Since Liva is a command-line application, you can run it from the `haskell` directory with `stack run <..>` and replacing `<..>` with any of the available commands.
We have already included an activity [tree](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/assets/tree.txt) and some activity [records](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/assets/records.txt) for you to try out. Here's a step-by-step tutorial:

+ Try `stack run start Code` to start Coding
+ Then check status with `stack run status`
+ Then run `stack run stop` to stop Coding and add a record to the records database
+ Alternatively you can run `stack run cancel` to cancel the recording
+ The purpose of Liva is to track how you spend your time. So run `stack run view Entertain 2021-05-01 2021-08-31` to see how much of last summer your spent on various entertainment activities. A bit too much Gaming I see!

If you are enjoying this and want to have a more personalized experience, you can try constructing your own activity tree and recording your own activities. Follow this step-by-step tutorial:

+ Build your activity tree in a `.txt` file following the format of [tree.txt](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/assets/tree.txt)
+ To reset things run `stack run reset`
+ Now load your new tree with `stack run load <filePath>`, replacing `<filePath>` with the path to your `.txt` file
+ Now you can follow the previous tutorial for adding activity records, etc.


## New Learning

Completing this project took us a lot of time and required learning more new elements than we anticipated.
We have highlighted some of the new learning and how they were applied below:


### Storing Data

Storing activities and records is a core part of Liva, so we needed a database.
Since we wanted the stored data to be in a simple and readable format, we decided to write our own database.
A key step was writing the [`Parsable`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L54) class so that any datatype implementing it can be stored to and parsed from a database.
We then used the `System.Directory` module to create/delete files.
And we built on our learnings from class to read/write to files. Examples:
+ [Parsing a `Date`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L157)
+ [Parsing a `Tree`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L205)
+ [Creating a file](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Interface.hs#L162)
+ [Deleting a file](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Interface.hs#L167)


### Command-line Parser

We needed a module to write the command-line interface for Liva.
We decided on using `Options.Applicative` for its simplcity.
Nevertheless, that required learning the Applicative style of writing Haskell code which we used in several parts of the code. Examples:
+ [`<$>` operator](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/app/Main.hs#L30)
+ [`<*>` operator](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/app/Main.hs#L72)
+ [`<**>` operator](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/app/Main.hs#L76)
+ [`Monoid.mconcat`](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/app/Main.hs#L80)


### Custom Data Constructors

Another smaller but important part was writing our own custom data constructors.
This helped us ensure correctness by only constructing a data if the input parameters satisfied some conditions.
For example, `Date 2021 13 30` is not valid and we can throw an error.
Here, we learnt to use the `pattern` keyword and the `{-# LANGUAGE PatternSynonyms #-}` language extension. Examples:
+ [`Time` pattern](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L85)
+ [`Date` pattern](https://github.students.cs.ubc.ca/pamani/cpsc312-project/blob/d55a79d1d6056d878b68e1e3b7ac40ad2d29c0e1/haskell/src/Base.hs#L123)
