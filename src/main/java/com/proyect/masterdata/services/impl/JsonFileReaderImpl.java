package com.proyect.masterdata.services.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import com.proyect.masterdata.dto.CountryDTO;
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
    public List<LocationDTO> filterDepartment() {

        List<LocationDTO> filteredDepartments = new ArrayList<>();

        try {
            // uncomment for deployment
            File file = new File("src/main/resources/peru.json");
            //File file = new File(
              //      "C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\resources\\peru.json");
            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(file,
                    new TypeReference<List<LocationDTO>>() {
                    });

            filteredDepartments = new ArrayList<>(locations.stream().collect(
                            Collectors.toMap(LocationDTO::getDepartment, obj -> obj, (existing, replacement) -> existing))
                    .values());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return filteredDepartments;
    }

    @Override
    public List<LocationDTO> filterProvince() {

        List<LocationDTO> filteredProvinces = new ArrayList<>();

        try {
            // uncomment for deployment
            File file = new File("src/main/resources/peru.json");
            //File file = new File(
            //        "C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\resources\\peru.json");
            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(file,
                    new TypeReference<List<LocationDTO>>() {
                    });

            filteredProvinces = new ArrayList<>(locations.stream().collect(
                            Collectors.toMap(LocationDTO::getProvince, obj -> obj, (existing, replacement) -> existing))
                    .values());
        } catch (Exception e) {
            e.printStackTrace();
        }

        return filteredProvinces;
    }

    @Override
    public List<LocationDTO> filterDistrict() {

        List<LocationDTO> filteredDistrict = new ArrayList<>();

        try {

            // uncomment for deployment
            File file = new File("src/main/resources/peru.json");
            //File file = new File(
            //        "C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\resources\\peru.json");

            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(file,
                    new TypeReference<List<LocationDTO>>() {
                    });

            filteredDistrict = new ArrayList<>(locations.stream().collect(
                            Collectors.toMap(LocationDTO::getDistrict, obj -> obj, (existing, replacement) -> existing))
                    .values());
        } catch (Exception e) {
            e.printStackTrace();
        }

        return filteredDistrict;
    }

    @Override
    public List<CountryDTO> filterCountry() {
        List<CountryDTO> filteredCountry = new ArrayList<>();
        try{
            // uncomment for deployment
            // File file = new File("src/main/country.json");
            File file = new File(
                    "C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\resources\\country.json");
            ObjectMapper mapper = new ObjectMapper();
            List<CountryDTO> locations = mapper.readValue(file,new TypeReference<List<CountryDTO>>(){});
            filteredCountry = new ArrayList<>(locations.stream().collect(Collectors.toMap(CountryDTO::getValue,obj->obj,(existing,replacement) -> existing)).values());
        }catch (Exception e){
            e.printStackTrace();
        }
        return filteredCountry;
    }

}
