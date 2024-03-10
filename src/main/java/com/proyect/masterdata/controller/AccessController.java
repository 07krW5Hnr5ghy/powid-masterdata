package com.proyect.masterdata.controller;

import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IAccess;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("access")
@AllArgsConstructor
public class AccessController {

    private final IAccess iAccess;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam(value = "name") String name,
            @RequestParam(value = "user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iAccess.save(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}