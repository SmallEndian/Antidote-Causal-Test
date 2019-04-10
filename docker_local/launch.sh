 docker build . -t alocal || exit
 Name=$(docker run -d -p 127.0.0.1:8087:8087 alocal )
 echo Launched "$Name"
 docker exec -ti "$Name" /opt/antidote/bin/env console
 docker stop "$Name"


