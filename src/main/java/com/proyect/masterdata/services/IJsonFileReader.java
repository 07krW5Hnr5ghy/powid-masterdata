package com.proyect.masterdata.services;

import java.io.IOException;
import java.util.List;

import com.proyect.masterdata.dto.LocationDTO;

public interface IJsonFileReader {
    public String readJsonFile() throws IOException;

    public List<LocationDTO> filterDepartment();

    public List<LocationDTO> filterProvince();

    public List<LocationDTO> filterDistrict();
}