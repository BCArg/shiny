README file for shiny apps of Statistical eLearning Tools project http://sites.uclouvain.be/selt

Author : Gregoire VINCKE http://www.uclouvain.be/gregoire.vincke or http://www.gregoirevincke.be

1) How to install Shiny ?
*************************
Shiny is a library for R project.
So first you have to install the latest version of R :
Then open R, and install Shiny from R, using :
> libraries.install("shiny",dependencies=TRUE)

2) How to run a Shiny app locally ?
***********************************

a) From R :
***********
Open R and load the Shiny library to provide the runApp() function :
>library(shiny)
Run the desired app using :
> runApp("path_to_app_directory")
where path_to_app_directory is the path on your hard drive from the working directory of R to the directory which contain the app files.
The runApp() function will execute the app files, and serve the result to your web browser on an URL looking like :
http://localhost:8100 (the port number 8100 can be different on your installation)

Exemples :
++++++++++
On GNU/Linux : 
--------------
If your working directory is ~/ (user home directory, /home/gvincke for exemple) and your app files are stored on /media/datas/shiny/myapp, then, to run the testhyp app you have to run :
> runApp("/media/datas/shiny/myapp")
OR
> runApp("../../media/datas/shiny/myapp")
But if you have already set your R session working directory to /media/datas/shiny, then you only have to run :
> runApp("myapp")

On MS Windows :
---------------
Path to files ARE NOT in MS Windows usal path syntax BUT in UNIX usual path synthax.


b) From outside of R :
**********************
A shiny app can be lounched directly from terminal, with the single command :
$ R -e "shiny::runApp('path_to_app_directory')
where path_to_app_directory is the path on your hard drive from the working directory of R to the directory which contain the app files (see above for exemples).
This single command will open R, load shiny library, and execute runApp().
The result will be a message like : Listening on port 8100 
Then go to your favorite web browser and open http://localhost:8100 (replace 8100 by the number of the port where shiny is listening)