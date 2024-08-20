package com.proyect.masterdata.services;

import java.io.IOException;
import java.util.List;

import com.proyect.masterdata.dto.CountryDTO;
import com.proyect.masterdata.dto.LocationDTO;

public interface IJsonFileReader {
    List<LocationDTO> filterDepartment();
    List<LocationDTO> filterProvince();
    List<LocationDTO> filterDistrict();
    List<CountryDTO> filterCountry();
}