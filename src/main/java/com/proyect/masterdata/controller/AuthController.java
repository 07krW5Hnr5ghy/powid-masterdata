package com.proyect.masterdata.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.request.RequestLogin;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.response.ResponseLogin;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IAuthentication;
import com.proyect.masterdata.services.IUser;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("auth")
@AllArgsConstructor
public class AuthController {

    private final IUser iUser;
    private final IAuthentication iAuthentication;

    @PostMapping(value = "register", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> register(
            @RequestBody() RequestUser requestUser) throws BadRequestExceptions {
        ResponseSuccess result = iUser.save(requestUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "login", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseLogin> login(
            @RequestBody() RequestLogin requestLogin) throws BadRequestExceptions {
        ResponseLogin result = iAuthentication.loginUser(requestLogin.getUsername(), requestLogin.getPassword());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
