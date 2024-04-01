package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CountryDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICountry;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("country")
@AllArgsConstructor
public class CountryController {
    private final ICountry iCountry;
    @GetMapping()
    public ResponseEntity<List<CountryDTO>> listCountry() throws BadRequestExceptions{
        List<CountryDTO> result = iCountry.listCountry();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
