---
title: 'My First Four Step Model: a simple and accessible tool to teach travel demand modeling'
tags:
  - travel demand modeling
  - four-step model
  - urban planning
  - transportation planning
  - transportation engineering
authors:
  - name: Matthew Wigginton Bhagat-Conway
    orcid: 0000-0002-1210-2982
    affiliation: 1
affiliations:
 - name: Department of City and Regional Planning, University of North Carolina at Chapel Hill
   index: 1
date: 12 March 2026
bibliography: paper.bib
---

# Summary

Virtually every metropolitan area has a travel demand model, that is used to forecast how transport outcomes will change in the face of future changes to land use, population, and the transportation system. Many of the models follow a traditional "four-step" structure. The first step (trip generation) predicts the number of trips that will take place in the region. The second step (trip distribution) predicts out where those trips are likely to go. The third step (mode choice) predicts what mode (car/walk/transit/etc.) these trips will use. The final step (network assigment) figures out which specific roads these trips will use.

Most transportation planning and engineering programs teach travel demand modeling to some extent [@zhou_transportation_2009]. In my experience, it is almost invariably taught with a 'bottom-up' approach. Students take classes on transportation planning, econometrics, and choice modeling. Students then work with individual components of demand models, such as trip generation or mode choice. Often, students never "put it all together" to run a regional model from start to finish. The vast majority of learning time is spent on theory and mathematics, rather than applications.

Instead of this traditional approach, this learning module and associated software introduces a 'top-down' approach where running a complete model is one of the first steps, with in-depth investigation of theory and mathematics coming later, if at all. The module is supported by an R package, "My First Four Step Model," which students use to quickly run a simple demand model.

Students interested in modeling may take further classes, but all students will have some first-hand experience with demand models---something many students do not get at all today, even after taking many classes on modeling.  I use this approach in my introductory Planning Methods course, where we spend only a week discussing transportation modeling and engineering, and at the conclusion the students run a simple demand model for the Research Triangle region of North Carolina. 

# Statement of need

Despite the ubiquity of travel demand models, most enigneers and planners have never worked with them hands-on. Even if they have taken a class that covers travel demand modeling, they likely have studied the theory and component models, but likely have not actually worked with a complete model themselves. Theory and mathematics are paramount for the small group of students who will build and run travel demand models themselves. However, at most metropolitan planning organizations and Departments of Transportation, demand models are estimated and run by consultants or a small in-house team. Consumers of model output are a larger group: transportation planners and engineers, land use planners, developers, advocates, and so on. For this larger group, only a cursory understanding of the mathematics is required; the general mechanisms and assumptions the model relies on are far more important. 

Most students in transportation planning and engineering programs will fall into the latter group. A better understanding of modeling among this group will help promote better communications between modelers and model consumers. Consumers will be more aware of what the model can and can't do, and more able to come up with situations where the model may be helpful. Understanding will also promote a "healthy skepticism" of the model, enabling feedback from users on the model and ultimately leading to better models and decision support.

# Learning objectives

My First Four-Step Model is a software package and learning module that allows students with minimal experience and consumer-grade computer hardware to run a simple four-step travel demand model. Specifically, it is designed to address these student learning outcomes:

1. Have a basic understanding of the structure and mathematics of travel demand model,
1. Understand the types of scenarios travel demand models are appropriate for,
1. Understand the limitations and uncertainty of travel demand modeling, and
1. Be able to have constructive conversations with travel modelers.

It is implemented as an R package [@r_2026], which has several advantages. R is a free, open-source, and cross-platform statistical programming language, allowing students to run it on their own computers regardless of configuration. Furthermore, R is becoming the _lingua franca_ of quantitative urban planning. Using the My First Four-Step Model package in an assignment gives students a gentle introduction to the language and potentially piques their interest in learning more. The package has several key design goals:

1. The four steps of the model map directly onto four functions in the package;
1. Any place where there is a tradeoff between simplicity and predictive accuracy, simplicity is chosen;
1. It can be estimated for any location in the United States using only publicly-available data;
1. There are intuitive tools to visualize model inputs, outputs, and parameters, so students can interpret and understand the model;
1. Preparing land-use and transportation scenarios is simple;
1. It runs on any computer a student is likely to have (including Windows, Macs, and Chromebooks); and
1. It depends only on R itself and common R packages that are easily installed from the Comprehensive R Archive Network (CRAN).

To meet these goals, the model is highly simplified, and this certainly affects its predictive accuracy, but predictive accuracy is not one of the goals. Epstein [-@epstein_why_2008] lists 16 reasons to build models other than prediction. One of them, train practitioners, is the primary goal of this model. This goal does not depend on high predictive accuracy. My First Four Step Model will never be appropriate for production travel demand modeling. It is also not appropriate as a sole teaching tool for students who will ultimately become modelers. However, it is useful as a first exercise even in courses that focus only on demand modeling, where students can have a chance to work with a simple model before diving into the more complex theories and software that are necessary for a detailed education in this area.

The open-source package is available on Github at <https://github.com/mattwigway/MyFirstFourStepModel>, with documentation (including the learning module and example assignments) at <https://projects.indicatrix.org/MyFirstFourStepModel>

# References