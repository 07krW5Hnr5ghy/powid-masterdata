package com.proyect.masterdata.services.impl;

import java.io.IOException;
import java.io.InputStream;
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

            Resource resource = resourceLoader.getResource("classpath:static/json/peru.json");
            InputStream inputStream = resource.getInputStream();
            Scanner scanner = new Scanner(inputStream).useDelimiter("\\A");
            String content = scanner.hasNext() ? scanner.next() : "";
            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(content,
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

            Resource resource = resourceLoader.getResource("classpath:static/json/peru.json");
            InputStream inputStream = resource.getInputStream();
            Scanner scanner = new Scanner(inputStream).useDelimiter("\\A");
            String content = scanner.hasNext() ? scanner.next() : "";

            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(content,
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

            Resource resource = resourceLoader.getResource("classpath:static/json/peru.json");
            InputStream inputStream = resource.getInputStream();
            Scanner scanner = new Scanner(inputStream).useDelimiter("\\A");
            String content = scanner.hasNext() ? scanner.next() : "";

            ObjectMapper mapper = new ObjectMapper();

            List<LocationDTO> locations = mapper.readValue(content,
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
            Resource resource = resourceLoader.getResource("classpath:static/json/country.json");
            InputStream inputStream = resource.getInputStream();
            Scanner scanner = new Scanner(inputStream).useDelimiter("\\A");
            String content = scanner.hasNext() ? scanner.next() : "";
            ObjectMapper mapper = new ObjectMapper();
            List<CountryDTO> locations = mapper.readValue(content,new TypeReference<List<CountryDTO>>(){});
            filteredCountry = new ArrayList<>(locations.stream().collect(Collectors.toMap(CountryDTO::getValue,obj->obj,(existing,replacement) -> existing)).values());
        }catch (Exception e){
            e.printStackTrace();
        }
        return filteredCountry;
    }

}
