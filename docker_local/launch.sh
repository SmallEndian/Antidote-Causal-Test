 docker build . -t alocal  
 Name=$(docker run -d alocal )
 echo Launched "$Name"
 docker exec -ti "$Name" /opt/antidote/bin/env console


