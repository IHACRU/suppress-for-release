# suppress-for-release

Demonstrates the methods of suppressing small counts in a provincial surveillance system to prepare data for public release.

# Background

When a surveillance agency intends to release incidence counts of some health conditions (like in [BC Chronic Disease Dashboard][dashboard]), one must take precaution NOT to disclose values considered "too small", which may present a privacy/re-identification risk. To avoid manual redaction, which is prone to human error and lacks transperancy, [BC Observatory][observatory] has developed a suite of R functions to arrive at recommendation for redaction automatically, based on logical tests developed for standard data forms.

For detailed background of the problem this project addresses, please view the slides from the Community of Practice [presentation][cop-presentation] at BCCDC on 2018-03-07 by Brent and Andriy. 

The following scripts comprise the workflow of the mechanized redaction of small cells:

- [`./manipulation/0-greeter.R`][greeter] - imports data, establishes decison frame
- [`./manipulation/1-tuner.R`][tuner] - cleans and transformes data
- [`./manipulation/2-tester.R`][tester] - applies logical tests to each frame
- [`./manipulation/3-grapher.R`][grapher] - redacts and plots decisions

The workflow can be summarized in the following pictogram:  
[![workflow](./libs/materials/suppress-for-release-image-support/Slide2.JPG)][workflow]

The dependency of key function is schematized as:  
[![dependency_tree](libs/materials/suppress-for-release-image-support/Slide3.JPG)][dependency_tree]


[cop-presentation]:https://rawgit.com/IHACRU/suppress-for-release/master/libs/materials/community-of-practice-2018-03-07.pdf

[greeter]:https://github.com/IHACRU/suppress-for-release/blob/master/manipulation/0-greeter.R
[tuner]:https://github.com/IHACRU/suppress-for-release/blob/master/manipulation/1-tuner.R
[tester]:https://github.com/IHACRU/suppress-for-release/blob/master/manipulation/2-tester.R
[grapher]:https://github.com/IHACRU/suppress-for-release/blob/master/manipulation/3-grapher.R

[workflow]:https://raw.githubusercontent.com/IHACRU/suppress-for-release/master/libs/materials/suppress-for-release-image-support/Slide2.JPG

[dependency_tree]:https://raw.githubusercontent.com/IHACRU/suppress-for-release/master/libs/materials/suppress-for-release-image-support/Slide3.JPG

[observatory]:http://www.bccdc.ca/our-services/programs/bc-observatory-for-pop-public-health

[dashboard]:http://www.bccdc.ca/health-info/disease-system-statistics/chronic-disease-dashboard