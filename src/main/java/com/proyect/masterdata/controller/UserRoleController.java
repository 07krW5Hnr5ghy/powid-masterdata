package com.proyect.masterdata.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IUserRole;

import lombok.AllArgsConstructor;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("user-role")
@AllArgsConstructor
public class UserRoleController {

    private final IUserRole iUserRole;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<ResponseSuccess> save(
            @RequestParam(value = "username") String username,
            @RequestParam(value = "role") String role,
            @RequestParam(value = "tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iUserRole.save(username, role, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
