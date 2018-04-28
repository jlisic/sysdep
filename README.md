# sysdep
SysDep is an R package to identify system dependencies of installed packages and suggest dependencies for packages not installed.


Progress:
============== 
 
General:
-----------

Finsihed:
 - General framework for shared object detection.
 
In progress 
 - Web API for not installed packages - done
   - Setup Nginx, MariaDB, and PHP server - done
   - Setup database schema - done 
   - Build scripts to populate server - 50%
   - Populate server with bulk builds - tested
   - Build PHP application
   - Deploy to digital ocean

Todo: 
 - Move R scripts to a package.
 - Buld test infrastructure.


Homebrew:
-----------

Finished:
 - Initial functions written.

Todo:
 - Keg integration and testing (post initial release).
 - Cask integraton and testing (post initial release).


Ubuntu 18.04/16.04:
-----------

Finished:
 - Initial dpkg testing.
 - Initial functions written. 

Todo:
 - Create a function to identify dev libraries associated with shared objects.
 - Handle PPAs (post initial release). 


Not Started:
-----------

 - RPM based distros: 
   - Centos 6/7x
   - SLES/LEAP (post initial release)

  

