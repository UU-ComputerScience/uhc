rm -rf `svn st | grep \? | cut -d " " -f 7`
rm -rf doc hdoc build install config.log config.status autom4te.cache

