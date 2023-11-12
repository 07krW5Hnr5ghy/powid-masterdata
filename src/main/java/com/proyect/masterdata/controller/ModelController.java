package com.proyect.masterdata.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IModel;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/model")
@AllArgsConstructor
public class ModelController {

    private final IModel iModel;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("brand") String brand,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iModel.save(name, brand, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "models", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> names,
            @RequestParam("brand") String brand,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iModel.saveAll(names, brand, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
