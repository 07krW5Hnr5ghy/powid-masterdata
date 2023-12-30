package com.proyect.masterdata.services.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.StandardCharset;
import com.proyect.masterdata.dto.LocationDTO;
import com.proyect.masterdata.services.IJsonFileReader;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import java.io.File;

@Service
@RequiredArgsConstructor
@Log4j2
public class JsonFileReaderImpl implements IJsonFileReader {

    private final ResourceLoader resourceLoader;

    @Override
    public String readJsonFile() throws IOException {
        Resource resource = resourceLoader.getResource("classpath:peru.json");

        try {
            Scanner scanner = new Scanner(resource.getInputStream(), StandardCharset.UTF_8.name());

            return scanner.useDelimiter("\\A").next();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
        }
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'readJsonFile'");
    }

    @Override
    public List<LocationDTO> filterDepartment() {

        List<LocationDTO> filteredDepartments = new ArrayList<>();

        try {
            File file = new File("src/main/peru.json");
            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(file,
                    new TypeReference<List<LocationDTO>>() {
                    });

            filteredDepartments = locations.stream().collect(
                    Collectors.toMap(LocationDTO::getDepartment, obj -> obj, (existing, replacement) -> existing))
                    .values().stream().collect(Collectors.toList());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return filteredDepartments;
    }

    @Override
    public List<LocationDTO> filterProvince() {

        List<LocationDTO> filteredProvinces = new ArrayList<>();

        try {
            File file = new File("src/main/peru.json");
            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(file,
                    new TypeReference<List<LocationDTO>>() {
                    });

            filteredProvinces = locations.stream().collect(
                    Collectors.toMap(LocationDTO::getProvince, obj -> obj, (existing, replacement) -> existing))
                    .values().stream().collect(Collectors.toList());
        } catch (Exception e) {
            e.printStackTrace();
        }

        return filteredProvinces;
    }

    @Override
    public List<LocationDTO> filterDistrict() {

        List<LocationDTO> filteredDistrict = new ArrayList<>();

        try {

            File file = new File("src/main/peru.json");
            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(file,
                    new TypeReference<List<LocationDTO>>() {
                    });

            filteredDistrict = locations.stream().collect(
                    Collectors.toMap(LocationDTO::getDistrict, obj -> obj, (existing, replacement) -> existing))
                    .values().stream().collect(Collectors.toList());
        } catch (Exception e) {
            e.printStackTrace();
        }

        return filteredDistrict;
    }

}
