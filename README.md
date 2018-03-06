# suppress-for-release

Demonstrates the methods of suppressing small counts in a provincial surveillance system to prepare data for public release.

For the background please view the slides from the Community of Practice [presentation][cop-presentation] at BCCDC on 2018-03-07 by Brent and Andriy. 

The following scripts comprise the workflow of the mechanized redaction of small cells:

- [`./manipulation/0-greeter.R`][greeter] - imports data, establishes decison frame
- [`./manipulation/1-tuner.R`][tuner] - cleans and transformes data
- [`./manipulation/2-tester.R`][tester] - applies logical tests to each frame
- [`./manipulation/3-grapher.R`][grapher] - redacts and plots decisions

The workflow can be summarized in the following pictogram:
[![workflow][workflow]][workflow]

The dependency of key function is schematized as:
[![dependency_tree][dependency_tree]][dependency_tree]


[cop-presentation]:
[greeter]:https://github.com/IHACRU/suppress-for-release/blob/master/manipulation/0-greeter.R
[]:https://github.com/IHACRU/suppress-for-release/blob/master/manipulation/1-tuner.R
[]:https://github.com/IHACRU/suppress-for-release/blob/master/manipulation/2-tester.R
[]:https://github.com/IHACRU/suppress-for-release/blob/master/manipulation/3-grapher.R
[workflow]:https://raw.githubusercontent.com/IHACRU/suppress-for-release/master/libs/materials/suppress-for-release-image-support/Slide2.JPG
[dependency_tree]:[workflow]:https://raw.githubusercontent.com/IHACRU/suppress-for-release/master/libs/materials/suppress-for-release-image-support/Slide3.JPG