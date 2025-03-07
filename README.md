## Proyecto de datos maestros 

mvn clean


# Compila el código fuente
mvn compile

# Ejecuta los tests (opcional, pero recomendado)
mvn test

# Empaqueta el proyecto y genera el JAR
mvn package



borrar contenedores
docker-compose down

borrar imagenes
docker-compose down --rmi all


constuir
docker-compose up --build -d