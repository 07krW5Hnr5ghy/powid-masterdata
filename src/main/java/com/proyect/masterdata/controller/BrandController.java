package com.proyect.masterdata.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IBrand;
import org.springframework.web.bind.annotation.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import lombok.AllArgsConstructor;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/brand")
@AllArgsConstructor
public class BrandController {

    private final IBrand iBrand;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iBrand.save(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "brands", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> namesList,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iBrand.saveAll(namesList, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iBrand.delete(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
