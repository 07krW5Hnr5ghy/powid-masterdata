#FROM openjdk:17-jdk-slim
#ARG JAR_FILE=target/masterdata-0.0.1-SNAPSHOT.jar
#COPY ${JAR_FILE} masterdata_backend.jar
#EXPOSE 8080
#ENTRYPOINT ["java","-jar","masterdata_backend.jar"]


FROM maven:3.9.9-eclipse-temurin-17 AS builder
WORKDIR /app

RUN mkdir -p /home/uploads /home/uploads/products /home/uploads/orders /home/uploads/couriers && \
    chown -R root:root /home/uploads/products /home/uploads/orders /home/uploads/couriers

COPY pom.xml .
RUN mvn dependency:go-offline -B
COPY src ./src
RUN mvn clean package -DskipTests

FROM openjdk:17-jdk-slim
WORKDIR /app

RUN mkdir -p /home/uploads /home/uploads/products /home/uploads/orders /home/uploads/couriers && \
    chown -R root:root /home/uploads/products /home/uploads/orders /home/uploads/couriers

ARG JAR_FILE=target/masterdata-0.0.1-SNAPSHOT.jar
COPY --from=builder /app/${JAR_FILE} masterdata_backend.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "masterdata_backend.jar"]
