package com.proyect.masterdata.controller;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.LocationDTO;
import com.proyect.masterdata.services.IJsonFileReader;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("json")
@AllArgsConstructor
public class JsonController {

    private final IJsonFileReader iJsonFileReader;

    @GetMapping()
    public List<LocationDTO> getJsonData() {
        List<LocationDTO> departmentList = new ArrayList<>();
        try {
            departmentList = iJsonFileReader.filterDepartment();
        } catch (RuntimeException e) {
            e.printStackTrace();
        }

        return departmentList;
    }
}
